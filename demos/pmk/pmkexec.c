/*   $RCSfile: pmkexec.c,v $ $Revision: 2.4 $ $Date: 90/08/27 14:03:55 $ */
/*
 * pmake
 * pmkexec.c
 * modified by Mark J. Steiglitz, 10/26/89
 */

/* Parallel make executor module
  
   This module uses the ISIS tasking system to manage the distributed
   parallel execution of unix commands according to dependencies represented
   by a DAG data structure.  The theory of operation of this module is
   presented in Parallel Make Implementation which contains relevent
   information not duplicated here.  The following routines are included
   in this module:
 
   UTILITIES

   dump_servers - dump all current server's queues
    
   pmkerr - report an error detected by pmake

   pmk_isis_err - report an error code returned by an isis call

   find_server - return a pointer to the server data structure associated with
                 as given address

   timestamp - get the current wall time for this CPU.
 
   find_node_in_list - find the node in a list which points to a given
                       address as its content.
 
   GRAPH MANIPULATION                  
                       
   reallocate_step - move a step node from one server to another
 
   mark_done - uptdate the graph to reflect step completion
 
   mark_deny - mark a step as denyed and record the time.
 
   poll_child_completion - datermine whether a child completed by polling

   
   SERVER MANIPULATION
   
   server_up - allocate a server queue to a new group member and return a
               pointer to that server queue structure.
 
   server_down - deallocate the indicated server.
 
   server_req - indicate that some server is requesting the
                       same step as the current one
 
   server_not_req - the indicated server is NOT requesting a step.
 
   update_alloc - update the allocation of processes to server structures
                  based on a new group view
 
                  
   JOIN AND STATE TRANSFER
 
   send_DAG - write the current graph to a file and inform a new group member
              of its name
  
   rcv_DAG  - receive a DAG description from another server
 
   accept_change - respond to a change in group membership

   
   OUTBOUND MESSAGES
   
   send_done - send a step done message
 
   send_request - send a step request message
 
   send_transfer - send a step transfer message
 
   send_deny - send a step denied message
 
   send_kill - send a kill message
 
   start_exec - begin execution of a step
 
   exec_or_take - begin execution of a command or take a command if possible

   try_giving_work - try to find work in the available work queue for each
                     idle server

		     
   INBOUND MESSAGE HANDLERS
   
   accept_done - process a step done message from another server
   
   accept_exec_done - process completion of the execution of a step by this
                      server
 
   accept_request - respond to a request for step transfer
 
   accept_transfer - respond to the transferrance of a step
   
   accept_deny - respond to the denyal of a transfer request
 
   accept_kill - respond to a kill request

   accept_cmd - accept an interactive command from the cmd tool

   new_process - Add a new server to the group if possible
   
   start_server - initialize the DAG, join the process group, and begin
                  execution of a command if possible.

This module uses graph and list management routines which are, for the most
part, self explanitory.  Routines to transfer graph contents to and from files
are also used.

The step scheduling algorithms are located in a seperate module accessed via
preschedule, next_action, work_avail and give_work calls.  There are 6
scheduling algorithms available for linking with pmkexec.

preschedule - Establish initial step allocation

next_action - decide whether to execut, tkae, or wait for a step.  next_action
              is also the first to notice when all work is complete.

work_avail - indicate that a step has become available for execution

give_work - give one step to another server if possible

		  
INCLUDES - THE REST OF THE PMK SYSTEM
 
*/

#ifndef ABSDIR
#define ABSDIR "/usr/u/isis/SUN/bin"
#endif

#include <pmkdat.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/file.h>
#include <fcntl.h>
#include <signal.h>
#include <utmp.h>
#include <rpcsvc/rusers.h>
#include <pwd.h>

/* lists of graph nodes, data items, and server descriptors */
extern list_type servers, imports, step_list;
extern step_addr_type *step_addr;
extern server_addr_type *server_addr;
extern int num_items, num_steps, num_servers;
extern int isis_socket;

extern int errno;			/* system error numbers */
extern char **environ;			/* unix environment */
extern bool central_queue;		/* flag indicating central queue environment */

int exec_state;				/* IDLE, WAIT_PRED, or WAIT_EXEC */

int take_state;				/* IDLE or STEP_REQUESTED */

/* srvr_state is in the server data structure and can have the values:
    UP DOWN or STEP_REQUESTED  */

/* pmk message types */

#define STEP_DONE 1
#define REQUEST_STEP 2
#define TRANSFER_STEP 3
#define DENY_STEP 4
#define KILL 5

/* action decision results */
#define DO_NOTHING 0
#define GO_IDLE 1
#define WAIT_PREREQ 2
#define MOVE_AND_EXEC 3
#define TAKE_WHILE_IDLE 4
#define TAKE_WHILE_DEP 5
#define GRAPH_DONE 6
#define START_EXEC 7

/* message field descriptors */
#define STEP_FLD 1
#define SERVER_FLD 2
#define TIME_FLD 3

/* time to delay before requesting a step again */
#define RE_REQ_DELAY (time_type)10000	/* 10 seconds */
#define SPEED_CHANGE_COEF .1	/* attenuation on speed factor change */

/* global variables */
int exec_step;				/* step being executed if exec_state=WAIT_EXEC */
int req_step;				/* step being requested if take_state=STEP_REQUESTED */

int exec_pid;				/* the UNIX process ID of my executing child */
bool exec_step_retry;			/* indicates whether or not the current step execution is a 
					   retry */

bool child_running;			/* flag to remember whether child is running. IF ever
					   child_running is true and wait3 returns -1, assume the
					   child has completed with good exit code. */

address my_gid;				/* my group ID */
char my_gname[fname_sz];		/* my group name */
int my_port;				/* my ISIS port number */
char my_graph_file[fname_sz];		/* my graph file name */
char my_cwd[fname_sz];			/* directory of original pamke cmd. */
groupview cur_view;			/* current group view */
server_type *my_server;			/* my server node data structure */
bool got_kill = FALSE;			/* boolean indicating kill signal */
int debug;				/* real time debug flag */
int num_procs;				/* number of processors to use in this job */

float total_speed_factor;		/* total speed factor of all completed steps */
int steps_done;				/* number of steps completed */

int base_time;				/* the time of day to be considered to be 0. */

/***************************************************************************/

dump_servers()
    /* display the current contents of all server's queues */
{
	register list_node *cur_srvr, *cur_step;

	if (debug) {
		for (cur_srvr = servers.head; cur_srvr; cur_srvr = cur_srvr->next) {
			if ((server_type *) cur_srvr->content == my_server)
				printf("*");
			printf("server %d rank %d : ",
			       ((server_type *) cur_srvr->content)->server_num,
			       ((server_type *) cur_srvr->content)->rank);

			for (cur_step = ((server_type *) cur_srvr->content)->queue.head;
			     cur_step; cur_step = cur_step->next) {
				if (((server_type *) cur_srvr->content)->done_step == cur_step)
					printf("*");
				printf("%d ", ((graph_node *) cur_step->content)->step_num);
			}
			printf("\n");
		}
		printf("exec_state= %d  take_state= %d\n", exec_state, take_state);
	}
}

pmkerr(p_msg)
	char *p_msg;

/* indicate the fatal error stated in the string */
{
	printf("PMAKE ERROR: %s\n", p_msg);
	write_graph("fatal.gphr");
	exit(-1);
}

pmk_isis_err(p_string)
	char *p_string;

     /* report an error generated by isis preconditions : isis_errno was set by the error
        postconditions : execution terminated */
{
	isis_perror(p_string);
	pmkerr("fatal ISIS error");
}

time_type
timestamp()
/* get as accurate a timestamp as possible from the system
   base_time must be set. */
{
	struct timeval tp;		/* time */
	struct timezone tzp;		/* timezone */

	gettimeofday(&tp, &tzp);
	return (((tp.tv_sec - base_time) * 1000) + (tp.tv_usec / 1000)) & 0x7fffff;
}

server_type *
find_server(p_addr)
	address p_addr;			/* the ISIS address to locate */

/* locate the server associated with the indicated ISIS address and
   return a pointer to it.
   preconditions: servers list was initialized using new_list
   postconditions: returns pointer to server with address if it is not
                   DOWN and exists.  else return NULL.
*/
{
	register list_node *cur_srvr;

	for (cur_srvr = servers.head; cur_srvr; cur_srvr = cur_srvr->next) {	/* return pointer
										   to first UP
										   server with
										   matching ISIS
										   address */
		if ((((server_type *) cur_srvr->content)->state != SERVER_DOWN) &&
		    (addr_isequal(&p_addr, &((server_type *) cur_srvr->content)->addr)))
			return (server_type *) cur_srvr->content;
	}
	return NULL;
}

list_node *
find_node_in_list(p_item, p_list)
	list_type p_list;		/* a list supposedly containing p_item */
	gen_ptr p_item;			/* the item to find */

/* find the list_node whose content field matches the p_item, a pointer to
   some element in the list.  Return pointer to list node if found, otherwise
   return NULL. */

{
	register list_node *cur_node;	/* the node being checked */

	for (cur_node = p_list.head;
	     cur_node && (!(cur_node->content == p_item)); cur_node = cur_node->next) ;
	return cur_node;
}

/***************************************************************************/
/* STEP TRANSFERS */

list_node *
reallocate_step(p_step, p_from, p_pred, p_to)
	server_type *p_from, *p_to;	/* source and destination servers */
	list_node *p_step, *p_pred;	/* step to move and new predecessor */

/* move p_step from p_from's queue to p_to's queue and position it after
   p_pred.
   preconditions: p_step is in p_from's queue and p_pred is either NULL
                  indicating insert at head, or in p_to's queue.
                  p_from and p_to must be initialized using init_proc
   postconditions: p_step is in p_to's queue */
{
	insert(p_pred, p_step->content, &(p_to->queue));
	((graph_node *) p_step->content)->processor = p_to;
	delete(p_step, &(p_from->queue));
	return (p_pred ? p_pred->next : p_to->queue.head);
}

mark_deny(p_step)
	graph_node *p_step;

/* mark the denied time of p_step with a timestamp */
{
	p_step->deny_time = timestamp();
}

mark_done(p_step)
	graph_node *p_step;		/* graph node whose execution has completed */

/* change the status of the indicated node to DONE,  Decrement the
   current depender counts of dependent nodes, and change their status to
   SCHEDULED.  Return the address of the next node to be executed in the
   queue of the server which just completed the node if it is SCHEDULED.
   preconditions: p_step has status SCHEDULED and has been allocated to
                  a server using allocate or reallocate_step.
   postconditions: step is DONE and depender's status updated to reflect it. */

{
	register list_node *data_node,	/* output item being visited */
	*step_node;			/* dependent step being visited */

	p_step->calc.status = DONE;

	/* visit all dependent nodes */
	for (data_node = p_step->output.head; data_node; data_node = data_node->next)
		for (step_node = ((data_item *) data_node->content)->dependers.head;
		     step_node; step_node = step_node->next) {
			graph_node *cur_step = (graph_node *) step_node->content;

			/* if not already 0, decrement cur_dep.  If 0 after decrement, status
			   becomes SCHEDULED */

			if (cur_step->cur_dep) {
				if (!(--(cur_step->cur_dep))) {
					cur_step->calc.status = SCHEDULED;	/* deps done,
										   SCHEDULE cmd. */

					/* note that step is now available. */
					work_avail(cur_step);
				}
			} else
				pmkerr("dependency count underflow");
		}

	/* advance done_step on the server which completed the step.  If the step completed was not 
	   the next step in line, complain. */
	if (step_node = p_step->processor->done_step) {	/* done step not null.  see if node after
							   done step refers to p_step */
		if (step_node->next && (((graph_node *) step_node->next->content) == p_step))
			/* OK to step done_step to next node in list */
			p_step->processor->done_step = step_node = step_node->next;
		else {		/* p_step was not next in list.  move the step if not mine, else
				   complain. */
			if (p_step->processor != my_server)
				p_step->processor->done_step = step_node =
				    reallocate_step(find_node_in_list(p_step,
								      p_step->processor->queue),
						    p_step->processor,
						    p_step->processor->done_step,
						    p_step->processor);
			else
				pmkerr("completed step not next in own server's list");
		}
	} else {		/* done step was null so p_step must be first in queue */
		if (((graph_node *) p_step->processor->queue.head->content) == p_step)
			p_step->processor->done_step = step_node = p_step->processor->queue.head;
		else {		/* p_step was not first in list.  move the step if not mine, else
				   complain. */
			if (p_step->processor != my_server)
				p_step->processor->done_step = step_node =
				    reallocate_step(find_node_in_list(p_step,
								      p_step->processor->queue),
						    p_step->processor, NULL, p_step->processor);
			else
				pmkerr("completed step not first in own server's list");
		}
	}
}

poll_child_completion()
    /* schedule accept_exec_done to check for child completion which may have been hidden by a
       system call ( or whatever reason ) */
{
	int accept_exec_done();

	t_fork((vfunc *) accept_exec_done, 0);
}

/***************************************************************************/
/* SERVER STATE MANIPULATION */

server_type *
server_up(p_addr)
	address p_addr;			/* address of process to allocate */

/* allocate the first DOWN server to process p_addr.  If all servers are
    UP, allocate a new one with an empty queue.
    preconditions: server list has been initialized
    postconditions: p_addr has a server data structure in server list 
*/
{
	list_node *cur_node;		/* server node being checked */
	server_type *cur_srvr;		/* server node allocated */
	int exec_take_true();
	int try_giving_work();
	int new_process();

	for (cur_node = servers.head; cur_node; cur_node = cur_node->next)
		if ((((server_type *) cur_node->content)->state == SERVER_DOWN) && (((server_type *) cur_node->content)->rank != AVAIL_WORK)) {	/* found 
																		   an 
																		   available 
																		   server. 
																		   assign 
																		   address 
																		   and 
																		   return 
																		 */
			((server_type *) cur_node->content)->addr = p_addr;
			strcpy(((server_type *) cur_node->content)->name,
			       site_names[p_addr.addr_site]);

			dbgp "old server %d site %d up\n",
			    ((server_type *) cur_node->content)->server_num,
			    ((server_type *) cur_node->content)->addr.addr_site dbge;

			/* if the server that has just come up is my own, set my_server */
			if (addr_isequal(&p_addr, &my_address)) {
				my_server = (server_type *) cur_node->content;
				strcpy(my_server->name, site_names[p_addr.addr_site]);
				t_fork((vfunc *) exec_take_true, 0);	/* begin work asap */

				t_fork((vfunc *) new_process, 0);

			}

			((server_type *) cur_node->content)->state = SERVER_UP;

			t_fork((vfunc *) try_giving_work, 0);

			return (server_type *) cur_node->content;
		}

	/* no DOWN server structure found.  Allocate one.  */
	cur_srvr = (server_type *) new_proc(site_names[p_addr.addr_site], &servers);
	cur_srvr->addr = p_addr;

	/* if the server that has just come up is my own, set my_server */
	if (addr_isequal(&p_addr, &my_address)) {
		dbgp "my server\n" dbge;
		my_server = cur_srvr;
		t_fork((vfunc *) exec_take_true, 0);

		t_fork((vfunc *) new_process, 0);

	}

	dbgp "new server %d site %d up\n", cur_srvr->server_num, cur_srvr->addr.addr_site dbge;

	cur_srvr->state = SERVER_UP;

	t_fork((vfunc *) try_giving_work, 0);

	return cur_srvr;
}

server_down(p_addr)
	address p_addr;			/* ISIS address of server to mark down */

/* set the state of the server with the indicated address to DOWN.
   If I was requesting a step from that server, erase the request.
   preconditions: servers list has been initailized
   postconditions: server with p_addr is DOWN if it is in servers list
*/
{
	int exec_take_true();
	int try_giving_work();

	server_type *cur_srvr;		/* server node found */

	cur_srvr = find_server(p_addr);
	if (cur_srvr) {
		cur_srvr->state = SERVER_DOWN;
		cur_srvr->rank = SRVR_AVAIL;
		dbgp "server %d down\n", cur_srvr->server_num dbge;

		/* for centralized scheme, if the first server in the list is AVAIL_WORK,
		   implicitly transfer all of the down server's steps to that queue, and signal
		   them as available work, then schedule try_giving_work */

		if (central_queue) {
			server_type *work_server = (server_type *) servers.head->content;
			list_node *step_node = cur_srvr->done_step;

			/* begin search at done_step if not nil . */
			if (!step_node)
				step_node = cur_srvr->queue.head;
			for (; step_node;) {
				graph_node *cur_step = (graph_node *) step_node->content;
				list_node *last_node = step_node;

				step_node = step_node->next;

				if (cur_step->calc.status == SCHEDULED) {
					reallocate_step(last_node, cur_srvr,
							work_server->queue.tail, work_server);
					work_avail(cur_step);
				}
			}
			t_fork((vfunc *) try_giving_work, 0);
		}
	}

	if ((take_state == STEP_REQUESTED) && ((*step_addr)[req_step]->processor == cur_srvr)) {
		list_node *cur_node;	/* need to step thru server list */

		/* my request was effectively denied */
		take_state == IDLE;	/* forget request */

		/* so was any other server's outstanding request for same step */
		for (cur_node = servers.head; cur_node; cur_node = cur_node->next)
			if (((server_type *) cur_node->content)->state == STEP_REQUESTED)
				((server_type *) cur_node->content)->state = SERVER_UP;
	}
	/* also, I might need to try another take. */
	t_fork((vfunc *) exec_take_true, 0);

}

server_not_req(p_addr)
	int p_addr;			/* server number of server to mark */

/* set the state of the server with the indicated address to UP if it
   is currently STEP_REQUESTED.
   preconditions: servers list has been initailized
   postconditions: server with p_addr is UP if it is in servers list
                     and was STEP_REQUESTED
*/
{
	server_type *cur_srvr;		/* server node found */

	cur_srvr = (*server_addr)[p_addr];
	if (cur_srvr && (cur_srvr->state == STEP_REQUESTED))
		cur_srvr->state = SERVER_UP;
}

server_req(p_addr, p_step)
	int p_addr;			/* server number of server to mark */
	int p_step;			/* step number being requested */

/* set the state of the server with the indicated address to 
   STEP_REQUESTED if the current server is also requesting the
   same step.
 
   preconditions: servers list has been initailized
   postconditions: server with p_addr is STEP_REQUESTED if it is contending 
                    for the same step as me, AND ISN'T ME!!
*/
{
	server_type *cur_srvr;		/* server node found */

	/* see if I am requesting the same step */
	if ((take_state == STEP_REQUESTED) && (req_step == p_step)) {
		/* mark indicated server as contenting */
		cur_srvr = (*server_addr)[p_addr];
		if (cur_srvr)
			cur_srvr->state = STEP_REQUESTED;
	}
}

bool
update_alloc(p_view)
	groupview *p_view;		/* new view */

  /* bring the current server allocation up to date based on the differences in membership between
     the new view in p_view and the old one in cur_view. preconditions: server list is initialized
     postconditions: each member of the new view has a server allocated to it.

     delete departed members, then add new ones */
{
	int old_slot, new_slot;		/* pg_alist slots in new and old lists */
	server_type *cur_srvr;		/* used for re-ranking */

	if (!addr_isnull(&p_view->gv_departed))
		server_down(p_view->gv_departed);
	if (!addr_isnull(&p_view->gv_joined))
		server_up(p_view->gv_joined);

	/* since view has changed we must also reassign RANKS. DOWN servers already have rank = -1 */
	for (new_slot = 0; p_view->gv_members[new_slot].addr_site != 0; new_slot++)
		if (cur_srvr = find_server(p_view->gv_members[new_slot])) {
			cur_srvr->rank = new_slot;
			dbgp "rank %d is server %d\n", new_slot, cur_srvr->server_num dbge;
		} else
			pmkerr("unallocated server during ranking");

}

/*******************************************************************************/
/*  JOIN AND STATE TRANSFER
 
    When a new process joins the group ISIS selects an existing process to
    send a message containing the state of the DAG which is guaranteed to be
    consistent amoung all members.  For this implementation the state will be
    saved in a file and the ISIS transfer contains the file name.
 
    It is important that the new process see any membership
    changes which are globally viewed as AFTER the view at the state transfer,
    even though they may in fact have occurred before the new process joins.
 
*/

#define DAG_XFER 1

int
send_DAG(bno)
	int bno;			/* the number of the block to be transferred. */

  /* send the indicated block number to a joining process */
  /* preconditions: invoked via ISIS, specified in allow_xfer postconditions: a temporary file
     containing the data has been generated and the name of the file is in the message. */
{
	static char fname[2 * fname_sz];	/* the name of the temp file */

	dbgp "transfer DAG\n" dbge;
	/* generate the temp file */
#ifdef MACH
	strcpy(fname, PMK_SCR);
	strcpy(fname, "/stateXXXXXX");
	mktemp(fname);
#else
	strcpy(fname, (char *) tempnam(PMK_SCR, "state"));
#endif
	dbgp "writing graph to file %s (%s) \n", fname, PMK_SCR dbge;
	write_graph(fname);

	xfer_out(DAG_XFER, "%s %s %s %d", fname, my_cwd, my_graph_file, base_time);

	poll_child_completion();

}

rcv_DAG(bno, msg)
	int bno;			/* block number */
	message *msg;

  /* read the graph data indicated by the msg and initialize the graph preconditions: must be
     invoked by ISIS during a join_and_xfer postconditions: new graph is installed

   */
{
	list_node *cur_srvr;		/* step through servers */

	switch (bno) {
	case DAG_XFER:{
		char state_file[fname_sz];

		dbgp "receive dag\n" dbge;
		/* starting a new graph - deallocate all parts of old graph */
		dealloc_all();
		init_digraph();

		msg_get(msg, "%s %s %s %d", state_file, my_cwd, my_graph_file, &base_time);

		/* get new graph */
		read_graph(state_file);

		unlink(state_file);
		init_rt();

		/* set the current view */
		cur_view = *pg_getview(&my_gid);

		dbgp "init view\n" dbge;

		/* fill in the actual addresses of servers.  note that their RANKS were read from
		   the graph file, but not their addresses */

		for (cur_srvr = servers.head; cur_srvr; cur_srvr = cur_srvr->next) {
			dbgp "existing server %d rank %d\n",
			    ((server_type *) cur_srvr->content)->server_num,
			    ((server_type *) cur_srvr->content)->rank dbge;

			if (((server_type *) cur_srvr->content)->rank >= 0) {
				((server_type *) cur_srvr->content)->addr =
				    cur_view.gv_members[((server_type *) cur_srvr->content)->rank];
				((server_type *) cur_srvr->content)->state = SERVER_UP;
			}
		}
		break;
	}

	default:{
		printf("unrecognized state transfer\n");
	}
	}
}

accept_change(pg, arg)
	groupview *pg;			/* the new view */
	char *arg;			/* unused arguement */

  /* process a change in group membership preconditions: cur_view has been initialized, call only
     via monitor_change postconditions: changed servers are reallocated and cur_view updated */
{
	int new_process();

	/* ignore group change if I havn't gotten my state yet. */
	if (servers.head) {
		dbgp "group change\n" dbge;
		/* update server allocation */
		update_alloc(pg);

		/* set in new view */
		cur_view = *pg;

		/* see if I need to start a replacement */
		/* new_process requires mutual exclusion.  It is called from only 2 places: here
		   and immediately after a process has joined the group.  Since the other call is
		   always executed by the NEWEST process to join the group, mutual exclusion on
		   new_process can be attained by forcing the newest group member to call it here
		   too. */

		if (!addr_isnull(&cur_view.gv_departed) && addr_isnull(&cur_view.gv_members[my_server->rank + 1]) && !got_kill && (steps_done < (num_steps - num_procs)))	/* -1 
																						   because 
																						   I 
																						   may 
																						   have
																						   done
																						   messages 
																						   pending. 
																						 */
			t_fork((vfunc *) new_process, 0);
	} else
		printf("group change ignored\n");
}

/************************************************************************/
/* OUTBOUND MESSAGES */

send_done(p_step)
	graph_node *p_step;

  /* send a CBCAST message indicating that the execution of p_step is complete. preconditions:
     server node describing p_ste is recorded in the step index as p_step->step_num ISIS is
     initialized and my_gid is set postconditions: the step done message has been CBCAST */

{
	int step_time;			/* actual time */

	step_time = timestamp() - p_step->start_time;

	dbgp "send done %d\n", p_step->step_num dbge;

	/* the STEP_DONE message format contains the step number and duration.  */

	if (cbcast(&my_gid, STEP_DONE, "%d %d %d", cur_view.gv_viewid,
		   p_step->step_num, step_time, 0))
		pmk_isis_err("send done");
	flush();

	dbgp "done sent\n" dbge;
}

send_request(p_step)
	graph_node *p_step;

  /* send a CBCAST message requesting ownership of the p_step preconditions: server node describing 
     p_step is recorded in the step index as p_step->step_num ISIS is initialized and my_gid is set
     postconditions: the step request message has been CBCAST */

{
	dbgp "send request %d\n", p_step->step_num dbge;

	/* the REQUEST_STEP message format contains the step number and the number of the
	   requesting server.  */

	if (cbcast(&my_gid, REQUEST_STEP, "%d %d %d", cur_view.gv_viewid,
		   p_step->step_num, my_server->server_num, 0))
		pmk_isis_err("send request");
	flush();
}

send_transfer(p_step, p_srvr)
	graph_node *p_step;
	int p_srvr;			/* NUMBER of server receiving step */

  /* send a CBCAST message requesting ownership of the p_step preconditions: server node describing 
     p_step is recorded in the step index as p_step->step_num ISIS is initialized and my_gid is set
     postconditions: the step transfer message has been CBCAST */

{
	dbgp "send transfer %d to %d\n", p_step->step_num, p_srvr dbge;

	/* the TRANSFER_STEP message format contains the step number and the number of the
	   receiving server.  */

	if (cbcast(&my_gid, TRANSFER_STEP, "%d %d %d", cur_view.gv_viewid,
		   p_step->step_num, p_srvr, 0))
		pmk_isis_err("send transfer");
	flush();
}

send_deny(p_step, p_srvr)
	graph_node *p_step;
	int p_srvr;			/* NUMBER of server denyed step */

  /* send a CBCAST message denying ownership of the p_step preconditions: server node describing
     p_step is recorded in the step index as p_step->step_num ISIS is initialized and my_gid is set
     postconditions: the step transfer message has been CBCAST */

{
	dbgp "send deny %d to %d\n", p_step->step_num, p_srvr dbge;

	/* the DENY_STEP message format contains the step number and server.  */

	if (cbcast(&my_gid, DENY_STEP, "%d %d %d", cur_view.gv_viewid, p_step->step_num, p_srvr, 0))
		pmk_isis_err("deny step");
	flush();

}

send_kill()

    /* send a GBCAST message to kill all group activity preconditions: ISIS is initialized and
       my_gid is set postconditions: the kill message has been GBCAST */
{
	dbgp "send kill \n" dbge;

	got_kill = TRUE;

	/* the KILL message format contains no additional information.  */

	if (gbcast(&my_gid, KILL, "", 0))
		pmk_isis_err("send kill");

}

/************************************************************************/
     /* execution routines */

start_exec(p_step)
	graph_node *p_step;

  /* fork off a UNIX task to execute the command in the indicated step. p_step->command and
     p_step->env contain the command to be executed and the environment pointer compatible with
     environ. The parent does not wait for the child to complete.  The completion signal is
     intercepted later and used to invoke accept_exec_done. The parent sets the exec_state to
     WAIT_EXEC, and records the exec_step variable before the fork, and the exec_pid after.

     The child closes all file descripters except standard in, out and error, then exec's the
     command described by p_step. */
  /* preconditions: p_step is initialized postconditions: a child of the current process is
     executing the command in p_step, or a pmkerr is generated */
{
	dbgp "execute %d\n", p_step->step_num dbge;
	exec_state = WAIT_EXEC;
	exec_step = p_step->step_num;
	exec_pid = 0;		/* in case of kill before fork completion */

	p_step->start_time = timestamp();

	switch (exec_pid = fork()) {
	case -1:{
		char err_string[80];

		/* build fork error message */
		sprintf(err_string, "fork error %d", errno);
		pmkerr(err_string);
		break;
	}

	case 0:{

		int cur_fd, num_fds;	/* number of file descriptors in table and index for close */
		char std_out_name[fname_sz];
		char dummy_step[20];

		FILE *out;

		/* dummy stub - if p_step->dummy_duration <> 0, replace command with sleep */

		if (p_step->dummy_duration) {
			p_step->sname = (char *) sprintf(dummy_step, "sleep %d",
							 (int) (p_step->dummy_duration *
								my_server->dummy_speed / 1000));
			dbgp "dummy cmd - duration %d speed %f\n", p_step->dummy_duration,
			    my_server->dummy_speed dbge;
		}

		isis_disconnect();

		/* construct the name of the standard output file for this server.  The name used
		   is graph name +.sout + server number on the PMK_SCR account */
		std_out_name[0] = 0;
		sprintf(std_out_name, "%s/%s.stdo%d", my_cwd, my_graph_file, my_server->server_num);

		out = fopen(std_out_name, "a+");
		fprintf(out, "***********************************");
		fprintf(out, "***********************************\n");
		fprintf(out, "STEP %d : %s\n", p_step->step_num, p_step->sname);
		fclose(out);

		cur_fd = open(std_out_name, O_WRONLY | O_APPEND | O_CREAT, 0000777);
		if (cur_fd < 0) {
			char err_str[80];

			err_str[0] = 0;
			sprintf(err_str, "stdout open error %d on %s.\n", errno, std_out_name);
			pmkerr(err_str);
		} else {	/* redirect stdout and errout to cur_fd */
			fchmod(cur_fd, 0000777);
			close(1);
			dup(cur_fd);
			close(2);
			dup(cur_fd);
		}

		/* execute the command under /bin/sh with correct env. */
		execle("/bin/sh", "/bin/sh", "-c", p_step->sname, 0, p_step->env);

		pmkerr("exec call error");
		exit(0);
		break;
	}
	      DEFAULT:child_running = TRUE;
		/* I am the parent and exec_pid is the child. */
	}
}

exec_or_take(p_take_allowed)
	bool p_take_allowed;		/* flag enabling or disabling take command */

/* If something is executing, do nothing.  Otherwise if the next step
   belonging to this server is SCHEDULED, execute it.  Otherwise if
   p_take_allowed call next_action, and take the recommended action.
   preconditions : the server and graph structures are initialized, and the
                   state variables are valid.
   postconditions : a step is executing or being taken, or no steps are
                   available or prerequisites are expected to be filled soon.
 */
{
	list_node *cur_node;		/* the queue list node pointing to said step */
	graph_node *cur_step;

	/* do nothing if a step is executing */
	if ((exec_state != WAIT_EXEC) || ((*step_addr)[exec_step]->calc.status == DONE)) {
		/* make an execute or take decision and live with result */
		switch (next_action(&cur_step, &cur_node)) {
		case START_EXEC:{
			start_exec(cur_step);
			break;
		}
		case GO_IDLE:
			exec_state = IDLE;

		case DO_NOTHING:
			break;

		case WAIT_PREREQ:{
			exec_state = WAIT_DEP;
			break;
		}
		case MOVE_AND_EXEC:{
			/* move cur_node from its current position in our queue to a position after 
			   done_step */

			reallocate_step(cur_node, my_server, my_server->done_step, my_server);

			/* execute the chosen step */
			start_exec(cur_step);
			break;
		}

		case TAKE_WHILE_IDLE:{
			exec_state = IDLE;
			if (p_take_allowed == TRUE) {
				take_state = STEP_REQUESTED;
				req_step = ((graph_node *) cur_node->content)->step_num;
				send_request((graph_node *) cur_node->content);
			}
			break;
		}
		case TAKE_WHILE_DEP:{
			exec_state = WAIT_DEP;
			if (p_take_allowed == TRUE) {
				take_state = STEP_REQUESTED;
				req_step = ((graph_node *) cur_node->content)->step_num;
				send_request((graph_node *) cur_node->content);
			}
			break;
		}
		case GRAPH_DONE:{

			/* If I was last member, post completed graph */
			if (!my_server->rank) {
				char full_graph_name[fname_sz];

				strcpy(full_graph_name, my_cwd);
				strcat(full_graph_name, "/");
				strcat(full_graph_name, my_graph_file);
				write_graph(strcat(full_graph_name, "r"));
				show_run(strcat(full_graph_name, ".sum"), &servers, "actual run");

				if (cur_view.gv_members[1].addr_site == 0) {
					/* leave the group - Accept no more changes */
					/* pg_delete(&my_gid); */
					exit(0);
				}
			} else {
				/* leave the group - Accept no more changes */
				/* pg_leave(&my_gid); */
				exit(0);
			}
		}
		}
	}
}

int
exec_take_true()
{
	exec_or_take(TRUE);
}

int
try_giving_work()
     /* call the scheduler routine for removing work from AVAIL_WORK server */
{
	graph_node *cur_step;
	int dest_server;

	/* dole out steps if necessary */
	if (my_server->rank == 0)
		while (cur_step = (graph_node *) give_work(&dest_server))
			send_transfer(cur_step, dest_server);
}

int *
accept_done(p_msg)
	message *p_msg;			/* the message received */

/* process receipt of a STEP_DONE message.  The step must be marked as
   done and its dependers updated.  Then evaluate the situation and take
   new action if possible.
   preconditions: call only from ISIS entry after graph is initialized
   postconditions: execution or step acquisition underway if possible.
 */
{
	graph_node *cur_step;
	int dest_server;
	int cur_step_num, duration;	/* message fileds returned */
	int view_id;

	msg_get(p_msg, "%d %d %d", &view_id, &cur_step_num, &duration);

	/* determine the identity of the step which has been completed */
	cur_step = (*step_addr)[cur_step_num];

	/* log the time in many places */

	cur_step->stop_time = timestamp();

	/* compute speed factor for step and influance speed factor for processor */
	total_speed_factor += cur_step->speed_factor = ((float) duration) / ((float)
									     (cur_step->calc.
									      duration ? cur_step->
									      calc.duration : 1));

	cur_step->processor->speed_factor +=
	    SPEED_CHANGE_COEF * (((cur_step->speed_factor * ++steps_done) /
				  total_speed_factor) - cur_step->processor->speed_factor);

	cur_step->start_time = cur_step->stop_time - duration;

	cur_step->processor->comp_time += duration;

	dbgp "accept done %d - speed %f\n", cur_step_num, cur_step->speed_factor dbge;

	/* process step completion */
	mark_done(cur_step);

	try_giving_work();

	/* execute action permitted by step completion */
	exec_or_take(TRUE);
}

accept_exec_done()

/* the step I was executing has completed with the indicated status.
   If status is non-0, retry ONCE.  exec_step_retry indicates whether
   or not we are already retrying
   
  preconditions: invoke only in response to signal from child process.
                 exec_step must be set. exec_step_retry initialized to FALSE
		 before first step befins execution.
  postconditions: the step is locked to prevent transfer until DONE message
                 arrives here, and the DONE message is CBCAST
*/
{
# ifndef AIX
	union wait wait_stat;
# else
	int wait_stat;
# endif
	struct rusage ru;
	int wait_pid;

	if (((wait_pid = wait3(&wait_stat, WNOHANG | WUNTRACED, &ru)) > 0) ||
	    ((wait_pid == -1) && child_running)) {
		child_running = FALSE;

#     if (!(AIX|AIXRS))
		if (wait_pid == -1)
			dbgp "poll detected missed child completion\n" dbge;
		else
			dbgp "exit code = %d\n", wait_stat.w_retcode dbge;
		if ((!wait_stat.w_retcode) || exec_step_retry || (wait_pid == -1)) {
			exec_step_retry = FALSE;

			(*step_addr)[exec_step]->calc.status = SCHED_LOCK;
			/* inhibit activity until done message is received */
			send_done((*step_addr)[exec_step]);
		} else {
			exec_step_retry = TRUE;

			start_exec((*step_addr)[exec_step]);
		}
#     else
		if (wait_pid == -1)
			dbgp "poll detected missed child completion\n" dbge;
		else
			dbgp "exit code = %d\n", wait_stat & 0xFF dbge;
		if ((wait_stat & 0xFF) || exec_step_retry || (wait_pid == -1)) {
			exec_step_retry = FALSE;

			(*step_addr)[exec_step]->calc.status = SCHED_LOCK;
			/* inhibit activity until done message is received */
			send_done((*step_addr)[exec_step]);
		} else {
			exec_step_retry = TRUE;

			start_exec((*step_addr)[exec_step]);
		}
#     endif
	} else
		dbgp "false wait\n" dbge;
}

accept_request(p_msg)
	message *p_msg;			/* the message received by ISIS */

/* decide whether or not to transfer the requested message to the indicated
   server, and CBCAST the transfer message if transfer is desired.  If the
   message IS transferred the transfer does not become effective until each
   server ( including the sender of the transfer message ) receives the
   message.  Therefore the transferred step is simply locked until the
   transfer is complete.  This will inhibit execution or sending of the
   step.
 
   If the requested step belongs to an DOWN server and the current server
   is the oldest, then the transfer should be managed by the current server.
   This means that the current server may request a step from itself.  For
   this reason the transfer and deny routines cannot interact in any way
   with the current state of the server. */
 /* preconditions: call only under ISIS control.  graph must be initialized postconditions: the
    transfer message is sent and the step is locked NOTE: the fact that a step is locked is hidden
    during state transfer, so there is effectively no local trace left by this routine */
{
	graph_node *cur_step;		/* the requested step */
	int server_num;			/* the requesting server */
	int step_num, view_id;
	groupview *my_view;

	msg_get(p_msg, "%d %d %d", &view_id, &step_num, &server_num);

	cur_step = (*step_addr)[step_num];

	dbgp "see request %d from %d\n", cur_step->step_num, server_num dbge;

	/* set the current view */
	my_view = pg_getview(&my_gid);

	/* decide whether or not the step is under my jurisdiction.  Does it point to my server or
	   is it ovned by a DOWN server and I am oldest */
	if ((cur_step->processor == my_server) ||
	    ((cur_step->processor->state == SERVER_DOWN) &&
	     addr_isequal(&my_address, &my_view->gv_members[0]))) {
		/* decide whether or not the step can be transferred */
		if ((cur_step->calc.status == SCHEDULED) && (!((exec_state == WAIT_EXEC) && (exec_step == cur_step->step_num)))) {	/* transfer 
																	   the 
																	   step 
																	 */
			cur_step->calc.status = SCHED_LOCK;
			send_transfer(cur_step, server_num);
		} else
			/* deny step transfer */
			send_deny(cur_step, server_num);
	}

	/* If I am not the requesting server, mark the server as requesting */
	if (my_server->server_num != server_num)
		server_req(server_num, cur_step->step_num);

}

accept_transfer(p_msg)
	message *p_msg;			/* ISIS message causing this step. */

/* unlock the step being transferred and modify the graph to
   reflect the transfer.  If the step was transferred to me then
   lock the step and deny any unresponded requests for the step by
   other servers.  Then unlock the step and call exec_or_take to decide
   whether to execute.
   preconditions: call only from ISIS entry.  graph initialized.
   postconditions: step transfer recorded, executing step if possible.
 */
{
	graph_node *cur_step;		/* the transferred step */
	server_type *cur_server;	/* the receiving server */
	server_type *sending_server;	/* the sending server */
	list_node *cur_node;		/* server queue node associated with step */
	groupview *my_view;

	int view_id, step_num, server_num;	/* message fields */

	msg_get(p_msg, "%d %d %d", &view_id, &step_num, &server_num);

	cur_step = (*step_addr)[step_num];
	sending_server = cur_step->processor;
	cur_node = find_node_in_list((gen_ptr) cur_step, sending_server->queue);

	cur_server = (*server_addr)[server_num];

	/* note reciept of transfer sent by central scheduler */
	if (cur_server->state == STEP_SENT)
		cur_server->state = SERVER_UP;

	my_view = pg_getview(&my_gid);

	/* step transfers are invalid if the view changed. */
	if (view_id != my_view->gv_viewid) {
		dbgp "view changed - ignore transfer %d to %d\n", step_num, server_num dbge;
		server_not_req(cur_server->server_num);
		cur_step->calc.status = SCHEDULED;

		/* disable take if I was transfer destination */
		if (cur_server == my_server) {
			take_state = IDLE;
			exec_or_take(TRUE);
		}

		/* if the source of the transfer was the available work server add it back into the 
		   work list */
		if (cur_step->processor->rank == AVAIL_WORK)
			work_avail(cur_step);
	} else {
		dbgp "accept transfer %d to %d\n", step_num, server_num dbge;

		/* move the step unless I am the destination NOTE: transfers to myself and to
		   others can end up in different queue positions because of a server's willingness 
		   to reorder its own queue without informing others. */
		if (cur_server != my_server) {
			reallocate_step(cur_node, sending_server, cur_server->done_step,
					cur_server);
			/* mark the current server as responded if it was requesting the same step. 
			 */
			server_not_req(cur_server->server_num);
			cur_step->calc.status = SCHEDULED;	/* in case I locked the step as its 
								   sender */
		} else {
			take_state = IDLE;	/* I am no longer requesting a step */

			/* the step is being transferred to ME.  put it after the last step I
			   executed, or the step I am executing */
			if (exec_state == WAIT_EXEC)
				reallocate_step(cur_node, sending_server,
						find_node_in_list((gen_ptr) (*step_addr)[exec_step],
								  my_server->queue), my_server);
			else
				reallocate_step(cur_node, sending_server,
						my_server->done_step, my_server);

			/* lock the step so I won't give it away */
			cur_step->calc.status = SCHED_LOCK;

			/* deny unresponded servers access */
			for (cur_node = servers.head; cur_node; cur_node = cur_node->next)
				if (((server_type *) cur_node->content)->state == STEP_REQUESTED) {
					((server_type *) cur_node->content)->state = SERVER_UP;
					send_deny(cur_step,
						  ((server_type *) cur_node->content)->server_num);
				}
			/* unlock the step and see if it needs executing */
			cur_step->calc.status = SCHEDULED;
			exec_or_take(TRUE);
		}
	}
}

accept_deny(p_msg)
	message *p_msg;			/* ISIS message which caused entry */

/* mark denial of the indicated request and exec_or_take
   if current server was the
   requester.  mark request responded on the requester, IF the requester
   is not DOWN.
   preconditions: invoke only from ISIS.  graph initialized.
   postconditions: denial noted
*/
{
	graph_node *cur_step;		/* the denied step */
	server_type *cur_server;	/* the receiving server */
	list_node *cur_node;		/* server queue node associated with step */
	int field_len;
	int view_id, step_num, server_num;	/* message fields */

	msg_get(p_msg, "%d %d %d", &view_id, &step_num, &server_num);

	/* get a pointer to the step and the server number */
	cur_step = (*step_addr)[step_num];
	cur_server = (*server_addr)[server_num];

	dbgp "accept deny %d from %d\n", step_num, server_num dbge;

	mark_deny(cur_step);	/* even if I wasn't requesting */
	if (cur_server == my_server) {
		take_state = IDLE;
		exec_or_take(TRUE);
	} else
		server_not_req(cur_server->server_num);
}

accept_kill(p_msg)
	message *p_msg;			/* ISIS kill message - no interesting contents */

/* stop execution of any commands and leave the ISIS group.  If current server
   is oldest which kill GBCAST is received, current server posts graph.
   preconditions: call only as ISIS entry,  graph initialized
   postconditions: task shut down
*/
{
	dbgp "accept kill\n" dbge;

	pg_leave(&my_gid);

	/* see if I should post the graph */
	if (!cur_view.gv_members[1].addr_site)
		write_graph(strcat(my_graph_file, "k"));

	/* See if I need to kill my child */
	if ((exec_state == WAIT_EXEC) && (exec_pid > 0))
		kill(exec_pid, 9);

	exit(0);
}

accept_cmd(p_msg)
	message *p_msg;

     /* process a dump request from the cmd tool.  The first ( and only) parameter must be a file
        name, the destination of the dump.  the rank of the current server will be appended to the
        file name */
{
	int argc;
	char **argv[32];
	char fname[80];

	getargs(p_msg, argc, argv, 32);

	if (argc == 1) {
		sprintf(fname, "%s%d", argv[0], my_server->rank);
		write_graph(fname);
		reply(p_msg, "%s", "dump completed\n");
	} else
		reply(p_msg, "%s", "say what?\n");

}

new_process()

    /* start a new server.  If the number of group members < num_procs, add a server which is in
       the current site view but not in the process group view. preconditions : ISIS is running
       postconditions : new pmkexec server, or error printer */
{
	site_id sites[2];
	char **args = (char **) malloc(5 * sizeof(char *));
	address *pname = (address *) malloc(2 * sizeof(address));
	char prog[fname_sz];
	char port[10];
	char srvrs[3];
	int err;

	sview *site_view;		/* current site view */
	int cur_site;			/* current site */
	int cur_member;			/* current group member */

	dbgp "start new process\n" dbge;

	/* count group members */
	for (cur_member = 0; cur_view.gv_members[cur_member].addr_site; cur_member++)
		continue;

	/* see if group is full enough */
	if (cur_member < num_procs) {
		/* find an unused server */
		site_view = (sview *) site_getview();
		for (cur_site = 0; site_view->sv_slist[cur_site]; cur_site++) {
			for (cur_member = 0; cur_view.gv_members[cur_member].addr_site &&
			     (cur_view.gv_members[cur_member].addr_site !=
			      SITE_NO(site_view->sv_slist[cur_site])); cur_member++)
				continue;

			if (cur_view.gv_members[cur_member].addr_site == 0) {	/* eureka! found a
										   site in the site 
										   view member
										   that's not in
										   the process view 
										 */

				char shortname[fname_sz];
				char loadcmd[fname_sz];
				char *dotpos;
				int nusers;

				strcpy(shortname,
				       site_names[SITE_NO(site_view->sv_slist[cur_site])]);
				dotpos = (char *) strchr(shortname, '.');
				if (dotpos)
					dotpos[0] = 0;

				nusers =
				    rnusers(site_names[SITE_NO(site_view->sv_slist[cur_site])]);
				dbgp "%s has %d users \n", shortname, nusers dbge;

				/* don't be selective with dummy runs. */
				if (nusers < 4 || my_server->dummy_speed)
					/* This used to test nusers==0 but that is almost always
					   true, even if the workstation is idle. We should either
					   check for non-idle users, or wait until a proper rexec
					   service which knows about load factors. */
				{
					strcpy(prog, ABSDIR);
					strcat(prog, "/pmkexec");

					if (access(prog, X_OK) == 0) {
						sprintf(port, "%d", my_port);
						sprintf(srvrs, "%d", num_procs);

						args[0] = prog;
						args[1] = my_gname;
						args[2] = port;
						args[3] = srvrs;
						args[4] = NULL;	/* termination */

						dbgp "%s on %s, group %s port %s\n",
						    prog,
						    site_names[SITE_NO
							       (site_view->sv_slist[cur_site])],
						    args[1], args[2] dbge;

						sites[0] = site_view->sv_slist[cur_site];
						sites[1] = 0;

						err =
						    isis_rexec(1, &NULLADDRESS, sites, prog, args,
							       environ, getpwuid(getuid())->pw_name,
							       "", pname);
						if (pname->addr_site == 0)
							pmkerr("can't start server");
						dbgp "r_exec = %d entry %d\n", err,
						    pname->addr_entry dbge;

					} else {
						printf
						    ("Can't find pmkexec.\n  Check pmake build file ");
						printf("in isis/demos/pmk for correct ABSDIR.\n");
					}

					free(args);

					return;
				}
			}
		}
	}
}

start_server()

    /* join my_gname and start executing steps if possible.  If the group does not exist, create it 
       and start pmkexec on the list of sites in the topology file. preconditions: either the group 
       exists and has a process running in it, or the graph and topology files exist. my_graph_file 
       and my_gname are set from args postconditions: full pmkexec is started */
{
	/* try to find the group */
	my_gid = *pg_lookup(my_gname);

	if (my_gid.addr_site) {	/* I am joining an existing group.  state transfer during the join
				   will deliver graph and allocate me a server. */

		message *mp;		/* dummy message used as join verifier */

		dbgp "join group\n" dbge;
		my_gid = *pg_join(my_gname,
				  PG_XFER, 0, send_DAG, rcv_DAG, PG_MONITOR, accept_change, 0, 0);
	} else {
		/* Im am to be the founder of a group.  In this case my graph has been read already 
		   during pre-scheduleing.  NOTE: at this point the graph nodes must have cur_dep
		   and calc.status set to exactly reflect the imported data items */
		char rm_cmd[fname_sz];
		int i;			/* new process counter */
		struct timeval tp;	/* time */
		struct timezone tzp;	/* timezone */

		/* set base_time */
		gettimeofday(&tp, &tzp);
		base_time = tp.tv_sec;

		/* remove old output files for this graph */
		rm_cmd[0] = 0;
		sprintf(rm_cmd, "rm -f %s/%s.stdo*", my_cwd, my_graph_file);
		system(rm_cmd);

		dbgp "start group\n" dbge;

		my_gid = *pg_join(my_gname,
				  PG_XFER, 0, send_DAG, rcv_DAG, PG_MONITOR, accept_change, 0, 0);

		cur_view = *pg_getview(&my_gid);

		/* NOT NEEDED - I ALWAYS SEE MYSELF JOIN if(!my_server) my_server = (server_type
		   *)new_proc("origin",&servers);

		   my_server->state=SERVER_UP; my_server->rank=0; my_server->addr=my_address;

		   exec_or_take(FALSE); */
	}

	isis_start_done();
}

main(argc, argv)
	int argc;
	char *argv[];			/* arguements */

/* execute an ISIS server !!!
   arguements: argv[1]:graph file name           (optional)
               argv[2]:ISIS group to be created
               argv[3]:port for ISIS to use
               argv[4]:number of processors to use
 
   This routine initializes the ISIS package, installs pmkexec entries,
   then becomes the tasking loop for those entries.  In order to intercept
   UNIX signals I do my own select in the tasking loop.  The child status
   change and kill signals are intercepted and cause delayed fork calls.
   Ths ISIS fd is serviced with calls to isis_read and run_tasks.
   All other signals are ignored.
 
 */
{
	/* get arguements */
	if (argc > 4) {
		strcpy(my_graph_file, argv[1]);
		strcpy(my_gname, argv[2]);
		sscanf(argv[3], "%d", &my_port);
		debug = FALSE;
		sscanf(argv[4], "%d", &num_procs);
#if	defined(HPUX)
		getcwd(my_cwd, sizeof(my_cwd));
#else
		getwd(my_cwd);
#endif

	} else if (argc == 4) {	/* no file name - I'm not first. */
		strcpy(my_gname, argv[1]);
		sscanf(argv[2], "%d", &my_port);
		debug = FALSE;
		sscanf(argv[3], "%d", &num_procs);
	} else
		pmkerr("Not enough arguements");

	if (num_procs < 0) {
		debug = TRUE;
		num_procs = 0 - num_procs;
	}

	/* init several global vars */
	exec_state = WAIT_DEP;
	take_state = IDLE;
	exec_step_retry = FALSE;
	child_running = FALSE;
	total_speed_factor = 0;
	steps_done = 0;

	/* start ISIS */
	isis_init(my_port);	/* use system site list */

	init_digraph();
	/* preschedule the graph if I am the first member of the group */
	if (argc > 4)
		preschedule();

	/* establish entries */
	isis_entry(STEP_DONE, (vfunc *) accept_done, "Accept Step Done");
	isis_entry(REQUEST_STEP, (vfunc *) accept_request, "Accept Request");
	isis_entry(TRANSFER_STEP, (vfunc *) accept_transfer, "Accept Transfer");
	isis_entry(DENY_STEP, (vfunc *) accept_deny, "Accept Deny");
	isis_entry(KILL, (vfunc *) accept_kill, "Accept Kill");
	isis_entry(MSG_COMMAND, (vfunc *) accept_cmd, "accept cmd");

	isis_signal(SIGQUIT, send_kill, 0);
	isis_signal(SIGTERM, send_kill, 0);
	isis_signal(SIGCHLD, accept_exec_done, 0);

	isis_task((vfunc *) start_server, "start_server");
	isis_task((vfunc *) exec_take_true, "exec_take");
	isis_task((vfunc *) new_process, "new_process");
	isis_task((vfunc *) accept_change, "accept_change");
	isis_task((vfunc *) send_DAG, "send_DAG");
	isis_task((vfunc *) rcv_DAG, "rcv_DAG");

	isis_mainloop((vfunc *) start_server, NULL);

}
