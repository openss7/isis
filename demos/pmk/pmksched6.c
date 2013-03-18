/*   $RCSfile: pmksched6.c,v $ $Revision: 2.1 $ $Date: 90/07/31 16:18:36 $ */
/*
     PARALLEL MAKE SCHEDULING MODULE - CENTRAL REAL TIME SF VERSION:

     Give steps to servers in modified critical time order
     as needed in real time taking spped factors into account
     */
 
#include <pmkdat.h>
 
#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/file.h>
#include <fcntl.h>
#include <signal.h>
#if	!defined(HPUX)
#include <string.h>
#endif
#include <stdio.h>

/* lists of graph nodes */
extern list_type servers,imports,step_list;
extern step_addr_type *step_addr;
extern server_addr_type *server_addr;
extern int num_items,num_steps,num_servers;
extern int isis_socket;

extern int errno;   /* system error numbers */ 
extern char **environ;
 
extern int take_state;   /* IDLE or STEP_REQUESTED */

/* action decision results */
#define DO_NOTHING 0
#define GO_IDLE 1
#define WAIT_PREREQ 2
#define MOVE_AND_EXEC 3
#define TAKE_WHILE_IDLE 4
#define TAKE_WHILE_DEP 5
#define GRAPH_DONE 6
#define START_EXEC 7
 
/* time to delay before requesting a step again */
#define RE_REQ_DELAY (time_type)5000  /* 5 seconds */
#define RESORT_THRESH 2
  
extern char my_graph_file[fname_sz]; /* my graph file name */
extern server_type *my_server;       /* my server node data structure */
extern list_type servers;            /* all the other servers */
extern int debug;                    /* real time debug flag */
extern int num_procs;                /* number of processors to use */
extern groupview cur_view;           /* current process view */

struct ctime_node_type {             /* node describing all ready steps
					with a given ctime-duration */
         int tag_time,               /* ctime-dur value for this node */
             steps,                  /* number of steps */
	     total_comp;             /* total compute time for all steps in
					  this node */
	 list_type step_list;        /* list of steps with ctime-dur */
       };

typedef struct ctime_node_type ctime_node;

bool central_queue;                   /* indicates central queue environment */
list_type ctime_list;                /* executable steps organized by ctime */
list_type work_list;                 /* steps avail. for execution */
bool  work_sorted;                   /* indicates whether list is sorted */
bool  rebuild_rt;                    /* indicates that real time structures
					must be rebuilt */

int avail_work_time;                 /* total duration of work available */

gen_ptr                 dequeue();

int next_action(p_step,p_node)
graph_node **p_step;   /* returns step to take action on */
list_node **p_node;    /* returns server queue node pointing to p_step */
/* decide what action should be taken by the current server.
 
   precondition: the server list and graph have been initialized, and
        my_server points to this server's node.
	No step is executing (exec_state may be left over from done_step
	
   postcondition: no changes in the graph or server list - only a recommended
        action.
        */
{ list_node *cur_node;  /* scan the step list */
  list_node *my_next_node=NULL;  /* the next node in my queue */
  list_node *srvr_node;  /* scan the list of servers */

  dump_servers();
    
  /* find the next node I should execute */
  if(my_server->done_step)
    my_next_node=my_server->done_step->next;
  else if(my_server->queue.head)
    my_next_node=my_server->queue.head;

  /* if my next node exists and is SCHEDULED, recommend execution. */
    
  if (my_next_node &&
      (((graph_node *)my_next_node->content)->calc.status==SCHEDULED))
    {
      *p_node=my_next_node;
      *p_step=(graph_node *) my_next_node->content;
      return START_EXEC;
    }
  
  /* Simple minded part:  look for work immediately based on the following:
     1- MOVE_AND_EXEC the first SCHEDULED step in my queue.
     2- TAKE the first SCHEDULED step located by scanning
        through server's queues beginning from the last server in the
	server list.
     3- Steps whose transfers have been denied less than RE_REQ_DELAY
        ticks ago are not eligible for transfer.

  look for work in my own queue */
  for(cur_node=my_next_node;cur_node&&
      (((graph_node *)cur_node->content)->calc.status!=SCHEDULED);
      cur_node=cur_node->next);

  /* if cur_node not NULL then we must have found work. */
  if(cur_node)
    {
      *p_node=cur_node;
      *p_step=(graph_node *)cur_node->content;
      return MOVE_AND_EXEC;
    }

  /* NO TAKING STEP IN THIS VERSION */
  
  /* dull, dull, dull.  nothing to steal.  If I have more work in my
        queue, just wait for it. */
  if(my_next_node) return WAIT_PREREQ;

  /* maybe we're all done!! */
 
  for(cur_node=step_list.head;cur_node;cur_node=cur_node->next)
    if (((graph_node *)cur_node->content)->calc.status!=DONE)
      return GO_IDLE;
 
  return GRAPH_DONE;  /* everybody's done. */
    
}

/**************************************************************************

  Routines for managing ctime_list
  */

ctime_add(p_step)
     graph_node *p_step;   /* step to be added to ctime_list */
     /* add the indicated step to ctime_list.  ctime_list must have been
	initialized sometime in the past. p_step->calc must be set up.
	*/
{
  register list_node *cur_node;
  int targ_time;            /* ctime - dur of p_step */
  ctime_node *cur_ctime;    /* node to include p_step */
  
  targ_time = p_step->calc.cpath_time - p_step->calc.duration;
  
  /* find a ctime_node with the same ctime-dur as p_step */
  for (cur_node=ctime_list.head;cur_node &&
       (((ctime_node *)cur_node->content)->tag_time != targ_time);
       cur_node=cur_node->next)
    continue;

  if(cur_node)
    /* applicable node found */
    cur_ctime = (ctime_node *)cur_node->content;

  else
    {  /* new node needed */
      cur_ctime = (ctime_node *)malloc(sizeof(ctime_node));
      enqueue((gen_ptr)cur_ctime,&ctime_list);
      cur_ctime->tag_time = targ_time;
      new_list(&(cur_ctime->step_list));
      cur_ctime->steps=0;
      cur_ctime->total_comp=0;
    }
  
  /* add step to cur_ctime node */
  ++cur_ctime->steps;
  cur_ctime->total_comp += p_step->calc.duration;
  enqueue((gen_ptr)p_step,&(cur_ctime->step_list));

  dbgp "add step %d ctime-dur %d: steps=%d total_comp %d\n",
    p_step->step_num,cur_ctime->tag_time,cur_ctime->steps,cur_ctime->total_comp
      dbge;
  
}

ctime_delete(p_step)
     graph_node *p_step;   /* step to be removed from ctime_list */
     /* remove p_step from the ctime_list structure.  If p_step was the last
	step in a node, delete the node.
	precondition : ctime_list initialized and p_step->calc set
	*/

{
  list_node *cur_node,*step_node;
  int targ_time = p_step->calc.cpath_time - p_step->calc.duration;
  ctime_node *cur_ctime;
  
  /* find the ctime node containing p_step */
  for (cur_node=ctime_list.head;cur_node &&
       (((ctime_node *)cur_node->content)->tag_time != targ_time);
       cur_node = cur_node->next)
    continue;

  if(!cur_node) pmkerr("cant find step in ctime\n");

  cur_ctime = (ctime_node *)cur_node->content;
  step_node = (list_node *)find_node_in_list((gen_ptr)p_step,
					     cur_ctime->step_list);
  if (!step_node) pmkerr("cant find step in ctime\n");

  dbgp "delete step %d from ctime\n",p_step->step_num dbge;
  
  delete(step_node,&(cur_ctime->step_list));
  cur_ctime->total_comp -= p_step->calc.duration;
  if(!(--cur_ctime->steps))
    {
     dbgp "delete ctime group %d\n",cur_ctime->tag_time dbge;
     delete(cur_node,&ctime_list);
    }
}

int comp_sched_time()
     /* compute the current schedule_time as defined in algorithm description
	ctime_list must be defined
	*/
{
  int sched_time=0;   /* maximum schedule time found so far */
  register list_node *cur_node;/* current ctime_list element being evaluated */
  float min_sf=1;            /* min speed factor */
  register list_node *eval_node;      /* step or server being checked */

  dbgp "speeds" dbge;
  for (eval_node=servers.head;eval_node;eval_node=eval_node->next)
    { dbgp " - %5.2f",((server_type *)eval_node->content)->speed_factor dbge;
 
      if((((server_type *)eval_node->content)->state != SERVER_DOWN) &&
	 (((server_type *)eval_node->content)->speed_factor < min_sf))
	min_sf = ((server_type *)eval_node->content)->speed_factor;
    }

  dbgp "\nctime groups " dbge;
  
  for(cur_node = ctime_list.head;cur_node;cur_node=cur_node->next)
    { ctime_node *cur_ctime = (ctime_node *)cur_node->content;

      /* if not enough processors, sched time is tag_time + total_comp */
      if(cur_ctime->steps > cur_view.gv_nmemb)
	{
	  if((cur_ctime->tag_time + (cur_ctime->total_comp/cur_view.gv_nmemb))
	       > sched_time)
	    sched_time = cur_ctime->tag_time +
	                   (cur_ctime->total_comp/cur_view.gv_nmemb);
	  dbgp " - %d:%d",
	         cur_ctime->tag_time, cur_ctime->tag_time +
		   (cur_ctime->total_comp/cur_view.gv_nmemb) dbge;
		   
	}
      else
	{  /* pick largest duration step and multiply by smallest sf. */
	   int largest_dur=0;           /* duration of largest step */
	
	   for (eval_node=cur_ctime->step_list.head;eval_node;
		eval_node=eval_node->next)
	     if (((graph_node *)eval_node->content)->calc.duration
		 > largest_dur)
	       largest_dur = ((graph_node *)eval_node->content)->calc.duration;

	   if((cur_ctime->tag_time + ((largest_dur + (min_sf<1?1:0))
				      * min_sf)) > sched_time)
	   sched_time = cur_ctime->tag_time + ((largest_dur + (min_sf<1?1:0))
						* min_sf);

	   dbgp " - *%d:%9.2f", cur_ctime->tag_time,cur_ctime->tag_time
	                        +largest_dur * min_sf  dbge;
	 }
    }
  dbgp " - sched_time = %d\n",sched_time dbge;
  
  return sched_time;
}

/************************************************************************
  ctime and work_list management
  */
  
int cmp_ctimes(p_node1,p_node2)
graph_node *p_node1,*p_node2;
/*
   compare the critical times of 2 graph nodes

*/
{
  time_type time1=0,time2=0;
  if (p_node1)
    time1= p_node1->calc.cpath_time;
  if (p_node2)
    time2= p_node2->calc.cpath_time;

  return (time2-time1);
}

init_rt()
/* initialize real time structures for preschedule, or rcv_dag */
{
  central_queue = TRUE;
  new_list(&work_list);  /* empty work list */
  new_list(&ctime_list); /* empty ctime list */
  rebuild_rt = TRUE;
  avail_work_time = 0;
} 

init_work_list()
     /* build a work list consisting of all SCHEDULED steps in the
	AVAIL_WORK server */
{
  register list_node *srvr_node, *step_node;

  avail_work_time = 0;
  /* deallocate old work_list */
  for(step_node=work_list.head;step_node;step_node=work_list.head)
    delete(step_node,&work_list);    

  /* deallocate old ctime_list */
  for(srvr_node=ctime_list.head;srvr_node;srvr_node=ctime_list.head)
    {
      for (;dequeue(&(((ctime_node *)srvr_node->content)->step_list));)
	continue;
      free(dequeue(&ctime_list));
    }
         
  
  for (srvr_node=servers.head;srvr_node;srvr_node=srvr_node->next)
    {
      if (((server_type *) srvr_node->content)->rank == AVAIL_WORK)
	{
	  for (step_node = ((server_type *) srvr_node->content)->queue.head;
	       step_node; step_node=step_node->next)
	    {
	      if(((graph_node *)step_node->content)->calc.status
		 ==SCHEDULED)
		{
		  enqueue(step_node->content,&work_list);
		  ctime_add((graph_node *)step_node->content);
		  avail_work_time += ((graph_node *)step_node->content)
		                            ->calc.duration;
		}
	    }
	}
    }
  sort_list(&work_list,cmp_ctimes);
  rebuild_rt = FALSE;
}


#define DIV_CONQ_THRESHOLD 0.1

int compute_skip_time(p_srvr)
     server_type *p_srvr;
     /* compute the amount of compute time to SKIP ( in cumulative step
	duration ) in order to reach the work assigned to p_srvr.
	preconditions : avail_work_time initialized, server_list initialized
	postconditions : returns time to skip
	*/
{
  int time_to_skip = 0;    /* skip time accumulated tso far */
  float avg_share;         /* average processor's share of the work */
  
  list_node *srvr_node;    /* server being checked */

  /* skip the amount of work executable by each server which is faster than
     p_srvr by more than DIV_CONQ_THRESHOLD

     Assuming that the average speed factor is always 1 ( not entirely
     accurate, but expedient ) the work allocated to a given processor
     is avail_compute_time/(number of servers * speed factor)
     */
  
  avg_share = avail_work_time / cur_view.gv_nmemb;

  dbgp "speeds = " dbge;
  for(srvr_node=servers.head;srvr_node;srvr_node=srvr_node->next)
    if ((((server_type *)srvr_node->content)->state != SERVER_DOWN))
      { dbgp " - %5.2f",((server_type *)srvr_node->content)->speed_factor dbge;
 
	if ((((server_type *)srvr_node->content)->speed_factor +
	     DIV_CONQ_THRESHOLD) < p_srvr->speed_factor)
	  time_to_skip += avg_share /
	    ((server_type *)srvr_node->content)->speed_factor;
      }
  dbgp "\n" dbge;
  return time_to_skip;
}

#define COMPLETION_MARGIN 1000

graph_node *give_work(p_srvr)
     int *p_srvr;    /* server to receive work */

     /* find the first server that doesn't have work to do, and
	give it the step with the latgest ctime that will fit between
	the calculated schedule time and the ctime-dur of the step.
	precondition: preschedule called and current scheduler is
	              manager of work_list ( rank 0 )
		      init_work_list ( or equivilent code in preschedule)
		      called
	*/
{
  list_node *cur_node;   /* server being checked */
  graph_node *give_step;
  list_node *give_node;
  int best_time=0;       /* size of goal overshoot of closest node to goal */
  int sched_time;        /* schedule time computed */
  int computed_time,cmpl_goal;     /* temp storage for goal meeting */

  server_type *cur_srvr;  /* fastest server in need of work */
  float min_sf=0;         /* speed factor of fastest server so far */

  int time_left_to_skip;
  
  if (rebuild_rt) init_work_list();

  cur_srvr = (server_type *)0;
  for (cur_node=servers.head;cur_node;cur_node=cur_node->next)
    {
      if ((((server_type *)cur_node->content)->state == SERVER_UP) &&
	  ((!((server_type *)cur_node->content)->queue.head ||
	   (((server_type *)cur_node->content)->done_step ==
	    ((server_type *)cur_node->content)->queue.tail))) &&
	      ((((server_type *)cur_node->content)->speed_factor < min_sf)
	       || (min_sf == 0)))
	{
	  cur_srvr = (server_type *)cur_node->content;
	  min_sf = cur_srvr->speed_factor;
	}
    }

  if(cur_srvr)
    {
      /* YES - please rush a step to this server */
      
      /* sort the work list first if necessary */
      if (!work_sorted)
	{
	  sort_list(&work_list,cmp_ctimes);
	  work_sorted == TRUE;
	}
      
      /* compute schedule time */
      sched_time = comp_sched_time();
      time_left_to_skip = compute_skip_time(cur_srvr);
      
      dbgp "find fit" dbge;

      /* look for a node that fits */
      for(give_node=work_list.head;give_node;
	  give_node=give_node->next)
	{
	  give_step = (graph_node *)give_node->content;
	  if ((time_left_to_skip -= give_step->calc.duration) <= 0)
	    {
	      dbgp " - divide and conquor fit\n" dbge;
	      break;
	    }
	  
	  /* we want to schedule this step if :
	     schedule time - sf * duration >=
	     ctime - duration.  ( remember, time runs backwards )
	     the following conditional tests that.
	     */
	  
	  if((computed_time = (sched_time - ((cur_srvr->speed_factor) *
	      			     (float)give_step->calc.duration) + 1)) >=
	     (cmpl_goal = give_step->calc.cpath_time-give_step->calc.duration
	                   + COMPLETION_MARGIN))
	    break;   /* schedule step */
	  else
	    { 
	      dbgp " - %d:%d>%d",give_step->step_num,
	      cmpl_goal,computed_time dbge;
	    }
	}
      dbgp "\n" dbge;
      
      /* if found node that fits, use it  else use tail*/
      if ((!give_node) && work_list.tail)
	{
	  dbgp "Using smallest ctime node\n" dbge;
	  give_node = work_list.tail;
	  give_step = (graph_node *)work_list.tail->content;
	}
      
      /* give node null only if no steps in work_list */
      if (give_node)
	{
	  delete(give_node,&work_list);
	  ctime_delete(give_step);	  
	  *p_srvr = cur_srvr->server_num;
	  cur_srvr->state = STEP_SENT;
	  avail_work_time -= give_step->calc.duration;
	  
	  dbgp "give step %d to server %d\n",
	  give_step->step_num,*p_srvr dbge;
	}
      else
	{
	  dbgp "no steps available\n" dbge;
	  give_step = (graph_node *)0;
	}
      return(give_step);
    }
  
  dbgp "no servers need work\n" dbge;
  return (graph_node *) 0;
}

work_avail(p_step)
     graph_node *p_step;
     /* place the available step in the work_list */
{
  if(my_server->rank == 0)
    {
      ctime_add(p_step);
      enqueue(p_step,&work_list);
      avail_work_time += p_step->calc.duration;
      work_sorted = 0;
    }
  else rebuild_rt = TRUE;
}
	 
preschedule()
/* read the raw graph and devise an initial schedule for it */
{ list_node *cur_node;  /* step thru lists */
  list_node *data_node;
  server_type *work_server;

  dealloc_all();
  init_digraph();
  read_graph(my_graph_file);
  alloc_index(num_items,num_steps,
	      (num_servers > MIN_SRVRS ? num_servers : MIN_SRVRS));
  
  sort_graph();
  calc_times();

  init_rt();       /* init work list */
  
  /* initialize available work server */
  work_server=(server_type *)new_proc("unallocated",&servers);
  work_server->rank = AVAIL_WORK;
  work_server->addr.addr_site = 0;

  avail_work_time = 0;
  
  /* initialize my server 
  my_server=(server_type *)new_proc("origin",&servers);
  */
  
  /* temporarily schedule work server with steps in topo order */
 
  for(cur_node=step_list.head;cur_node;cur_node=cur_node->next)
  {
    graph_node *cur_step=(graph_node *)cur_node->content;
    allocate(cur_step,work_server);
    cur_step->cur_dep=cur_step->num_deps;  /* initialize dep. count */
    if(cur_step->num_deps) cur_step->calc.status=ALLOCATED;
    else
      {
	cur_step->calc.status=SCHEDULED;
	enqueue(cur_step,&work_list);
	ctime_add(cur_step);
	avail_work_time += cur_step->calc.duration;
      }
  }

  /* note import dependencies and initialize cur_dep. */
  for(data_node=imports.head;data_node;data_node=data_node->next)
    for(cur_node=((data_item *)data_node->content)->dependers.head;
        cur_node;cur_node=cur_node->next)
      if(!--((graph_node *) cur_node->content)->cur_dep)
        {
	  ((graph_node *)cur_node->content)->calc.status=SCHEDULED;
	  enqueue(((graph_node *) cur_node->content),&work_list);
	  ctime_add((graph_node *) cur_node->content);
	  avail_work_time += ((graph_node *)cur_node->content)->calc.duration;
	}

  sort_list(&work_list,cmp_ctimes);
  work_sorted = TRUE;
  rebuild_rt = FALSE;
  
  write_graph("tmp.gph");

 }
