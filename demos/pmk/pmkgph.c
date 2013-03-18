/*   $RCSfile: pmkgph.c,v $ $Revision: 2.1 $ $Date: 90/07/31 16:18:07 $ */
/* graph data structure management
 
   this file contains routines which manage the graph data structure.
   There are 2 types of nodes in the graph: data_item and graph_node.
   Refer to pmkdat.h for details on node contents.
 
   Within the graph, graph nodes representing process steps are
   bidirectionally linked to the data items on which they depend.
   Each graph node is also bidirectionally linked to the data items
   which it creates.  Routines are provided for initialization of nodes,
   creation of links, and manipulation of node contents.
 
   In addition, all data items are referenced in one list which is used
   to search for items by name. A similar list is kept for graph nodes.

   On top of that there are arrays which correlate step and data numbers with
   node addresses, allowing fast access to nodes based on their numbers.
   Note that the step index should not be accessed while the graph is being
   sorted.
   In order to handle arbitrary graph sizes, the index arrays are allocated on
   the heap.  This cannot be done until the sizes of the arrays are known.
   For this reason, global pointers to the arrays are left at NULL until the
   graph size is known.
 
   For manipulating data items:
      data_item *find_data(name[dname_sz]) - return a pointer to the data
                                             item with the specified name
      data_item *new_data(name[dname_sz],type,
                           size,changed ) - add a new data item with the
                                             specified name and
                                             return a pointer to it.
      graph_node *new_step(name[sname_sz]) - add a new graph node with
                                             with specified name and return
                                             a pointer to it.
      graph_node *find_step(name[sname_sz])-return a pointer to the process
                                             step graph node with the
                                             specified name
      calc_type get_calcs(*graph_node) - return the calculation structure
                                             of graph_node
      set_calcs(calc_type,*graph_node) - set the calculation information in
                                             graph_node
      add_in_dep(*data_item,*graph_node) - establish an input dependency
                                             between a data item and a graph
                                             node
      add_out_dep(*graph_node,*data_item) - establish an output dependency
 
*/

#include <pmkdat.h>
#if	defined(HPUX)
char *strcpy();
#else
#include <string.h>
#endif
#include <stdio.h>


/* lists to insure access to every data_item and graph_node  */
list_type data_list;  /* list of all data nodes */
list_type step_list;  /* list of all process steps */
list_type imports;    /* list of all imported data nodes */
list_type servers;    /* list or server nodes */

int num_steps;        /* number of steps in graph */
int max_steps;        /* number of index elements allocated for step nodes */
int num_items;        /* number of data items in the graph */
int max_items;        /* index elements allocated for data items */
int num_servers;      /* server nodes */
int max_servers;      /* index elements allocated */

/* each of these is a pointer to an array of pointers to nodes */
step_addr_type *step_addr;  /* pointer to the step array */
item_addr_type *item_addr;   /* pointer to the data item address array */
server_addr_type *server_addr;  /* pointer to the server address array */

int graph_err;        /* 0 if graph built since init. is consistent:
                              1 - data item output by more than 1 step
                              2 - cycle in graph
                              3 - graph has too many items for alloc. index 
                     */
#define GRAPH_OK 0
#define MULTI_OUT_GRAPH_ERR 1
#define CYCLE_GRAPH_ERR 2
#define INDEX_GRAPH_ERR 3

/*************************************************************************/

/* special storage allocation tricks: 
   1 - save heap space by allocating exactly enough space for a string's
       current length and copying it
   2 - allocate space for an array of a specified number of pointers
   3 - allocate exactly enough space for an environment array, and
       copy it.
*/

char *alloc_strcpy(s1)
char *s1;
/* allocate exactly enough room for a copy of s1 and make the copy 

   preconditions: s1 is terminated with \0
                  there is enough heap for copy of s1
   postconditions: return pointer to copy of s1.
*/
{
  char *new_string=(char *)malloc((strlen(s1)+1)*sizeof(char));
  return(strcpy(new_string,s1));
}

gen_ptr *alloc_ptr_array(p_size)
int p_size;
/* allocate an array of p_size pointers, and return a pointer to the array
   preconditions: enough heap space for p_size pointers
   postconditions: array allocated from heap
*/
{
  return (gen_ptr *)malloc(p_size*sizeof(gen_ptr));
}
  

char **build_env(p_env)
list_type *p_env; 
/* allocate a unix compatible environment array consisting of pointers to
   the character strings in the list of environment strings p_env and
   deallocate p_env.

   preconditions: p_env is a list_type whose content is of type *char
                  and there is enough heap for the array
   postconditions: return a pointer to a newly allocated array of pointers
                  to the strings originally in p_env.
                  the list structure of p_env is deallocated, but the
                  strings therein are not.

*/
{
  int num_strings=0;  /* number of strings in the environment */
  list_node *cur_node; /* pointer used to step through p_env */
  env_ptr new_env;      /* pointer to the newly allocated array */
  env_ptr cur_env;      /* pointer to our current position in the environment
                           array */

  /* count the strings */
  for(cur_node=p_env->head;cur_node;cur_node=cur_node->next) num_strings++;

  /* allocate the array */
  new_env=(env_ptr)alloc_ptr_array(num_strings+1);

  /* copy the strings and delete list nodes as we go */
  cur_env=new_env;
  for(;p_env->head;)
  {
    *cur_env=(char *)p_env->head->content;
    cur_env++;
    delete(p_env->head,p_env);    /* delete the head node */
  }
  /* place a null pointer at the end of the array */
  *cur_env=(char *)NULL;
  return new_env;
}

bool envcmp(p_env1,p_env2)
     env_ptr p_env1,p_env2;
     /* compare the 2 environments and return 0 if equal, or result
	of first non-0 strcmp */
{ int result;

  /* OK to increment parameters since that doesn't change originals */
  
  if(p_env1 && p_env2)
    {
      for(;*p_env1 && *p_env2;)
	if(result=strcmp(*(p_env1++),*(p_env2++))) return result;

      if(*p_env1 || *p_env2) return 1;  /* one ended before other */
      else return 0;
    }
  else return 1;   /* one was totally nil. */
}

/**************************************************************************/

/* GRAPH MANAGEMENT */

void init_digraph()
/* initialize the graph to empty by setting access lists to nil.
   this routine does not deallocate previous graph, it just makes it
   inaccessible. . */
{
#ifdef dbggph
  
   printf("init graph \n");
#endif
 
   new_list(&data_list);
   new_list(&step_list);
   new_list(&imports);
   new_list(&servers);
   
   num_steps= 0;
   num_items= 0;
   num_servers=0;
   
   graph_err= GRAPH_OK;

  /* initialize index system */
   max_steps=max_items=0;    /* no indices allocated */
   step_addr= NULL;
   item_addr= NULL;
   server_addr=NULL;

}

void set_step_addr(p_step)
graph_node *p_step;
/* put the address of p_step in the index.  Ignore if index not allocated,
   set graph error if step number too large 

   preconditions: index allocated, p_step->step_num<max_steps
   postconditions: index set or graph error if index allocated.
*/
{
  if (step_addr)
  { /* index allocated */
    if (p_step->step_num < max_steps)
      (*step_addr)[p_step->step_num]=p_step;
    else graph_err=INDEX_GRAPH_ERR;
  }
}

void set_item_addr(p_item)
data_item *p_item;
/* put the address of p_item in the index.  Ignore if index not allocated,
   set graph error if data number too large 

   preconditions: index allocated, p_item->data_num<max_items
   postconditions: index set or graph error if index allocated.
*/
{
  if (item_addr)
  { /* index allocated */
    if (p_item->data_num < max_items)
      (*item_addr)[p_item->data_num]=p_item;
    else graph_err=INDEX_GRAPH_ERR;
  }
}
void set_server_addr(p_server)
server_type *p_server;
/* put the address of p_server in the index.  Ignore if index not allocated,
   set graph error if step number too large 

   preconditions: index allocated, p_server->server_num<max_servers
   postconditions: index set or graph error if index allocated.
*/
{
  if (server_addr)
  { /* index allocated */
    if (p_server->server_num < max_servers)
      (*server_addr)[p_server->server_num]=p_server;
    else graph_err=INDEX_GRAPH_ERR;
  }
}


void alloc_index(p_items,p_steps,p_servers)
int p_items, p_steps,p_servers;

/* allocate space for item server and step indices of specified size, and
   fill them with steps currently in step_list and data_list.
   set step_addr server_addr and item_addr to point to the arrays. */
/*  
   preconditions: graph initialized with init_digraph
   postconditions: new indices allocated and up to date.
*/
{
  list_node *cur_node;  /* pointer to list node being processed */

  /* if there are already arrays allocated, free them */
  if (step_addr) free(step_addr);
  if (item_addr) free(item_addr);
  if (server_addr) free(server_addr);

  /* allocate new arrays and set sizes */
  step_addr=(step_addr_type *)alloc_ptr_array(max_steps=p_steps);
  item_addr=(item_addr_type *)alloc_ptr_array(max_items=p_items);
  server_addr=(server_addr_type *)alloc_ptr_array(max_servers=p_servers);
  
  /* set currently known addresses */
  for (cur_node=step_list.head;cur_node;cur_node=cur_node->next)
    set_step_addr((graph_node *)cur_node->content);

  for (cur_node=data_list.head;cur_node;cur_node=cur_node->next)
    set_item_addr((data_item *)cur_node->content);

  for (cur_node=servers.head;cur_node;cur_node=cur_node->next)
    set_server_addr((server_type *)cur_node->content);
  
}
 
data_item *new_data(p_name,p_type,p_size,p_changed)
char *p_name;
dtype p_type;
int p_size;
bool p_changed;
 
/* allocate a new data item with the contents specified by the parameters */
/* preconditions : the graph has been initialized
   postconditions : a new data node is in the data_list with no links
                     to steps. */
 
{
   data_item *new_node=(data_item *) malloc(sizeof(data_item));
#ifdef dbggph
   printf("new node %d = %s at %x \n",num_items,p_name,new_node);
#endif
 
   if (!new_node) printf("no memory for data node");
   else
   {
     /* set node name and number */
     new_node->dname=alloc_strcpy(p_name);
     new_node->data_num=num_items++;

     /* set node information */
     new_node->file_type = p_type;
     new_node->size = p_size;
     new_node->changed = p_changed;

     /* initialize dependency pointers and lists */
     new_list(&(new_node->dependers));
     new_node->source=NULL;
     new_list(&(new_node->gen_deps));
     
     /* and put the node in the list of data items */
     enqueue((gen_ptr) new_node,&data_list);
     set_item_addr(new_node);
   }
   return new_node;
}
 
data_item *find_data(p_name)
char *p_name;
/* search for the data_item in data_list with the name p_name and return
   a pointer to it.  Return nil if not found.
   preconditions : graph initialized
   postconditions : graph unchanged
 
*/
{
  register list_node *cur_node;
#ifdef dbggph
   printf("find node %s ",p_name);
#endif
    for(cur_node=data_list.head;cur_node;cur_node=cur_node->next) 
      if (!strcmp(p_name,((data_item *) cur_node->content)->dname))
      {
#ifdef dbggph
   printf("found %x \n",cur_node->content);
#endif
 
        return (data_item *) cur_node->content;
      }
#ifdef dbggph
   printf("not found \n");
#endif
 
  return NULL;
}
 
graph_node *new_step(p_name)
char *p_name;
/* allocate a new graph node with name p_name and no dependencies or
   output.  put the node in step_list
   preconditions: graph initialized
   postconditions: new graph node is in step_list
*/
{
  graph_node *new_node=(graph_node *) malloc(sizeof(graph_node));
#ifdef dbggph
   printf("new step %d at %x \n",num_steps,new_node);
#endif
 
  if (!new_node) printf("no memory for graph node");
  else
  {
    /* set name and number */
    new_node->sname=alloc_strcpy(p_name);
    new_node->step_num= num_steps++;
    new_node->stdout_file = NULL;

    /* initialize dependencies */
    new_list(&(new_node->in_list));
    new_list(&(new_node->output));

    /* init. general info */
    new_node->num_deps= new_node->cur_dep= 0;
    new_node->calc.status = RAW;
    new_node->calc.duration = new_node->calc.cpath_time =
              new_node->calc.dep_time = 0;
    new_node->speed_factor = 1.0;
    new_node->calc.recompute = FALSE;
    new_node->env= NULL;
    new_node->processor = NULL;
    new_node->start_time = new_node->stop_time = new_node->exp_stop_time=
                            new_node->deny_time= 0;
    new_node->dummy_duration = 0;
    
    enqueue((gen_ptr) new_node,&step_list);
    set_step_addr(new_node);
  }
  return new_node;
}
 
graph_node *find_step(p_name)
char p_name[sname_sz];
/* search for the graph_node in step_list with the name p_name and return
   a pointer to it.  Return nil if not found.
   preconditions : graph initialized
   postconditions : graph unchanged
 
*/
{
   register list_node *cur_node;
 
#ifdef dbggph
   printf("find step %s",p_name);
#endif
 
  for(cur_node=step_list.head;cur_node;cur_node= cur_node->next)
    if (!strcmp(p_name,((graph_node *) cur_node->content)->sname))
    {
#ifdef dbggph
   printf("found %x\n",cur_node->content);
#endif
      return (graph_node *) cur_node->content;
    }
 
#ifdef dbggph
   printf("not found \n");
#endif
  return NULL;
}
 
calc_type *get_calcs(p_node)
graph_node *p_node;
/* return the current value of the calculations for node p_node
   preconditions: new_step has been called for p_node
   postconditions: p_node is unchanged
*/
{
#ifdef dbggph
   printf("get calcs %d %d %d %d %d\n",p_node->calc.duration,
        p_node->calc.cpath_time,p_node->calc.dep_time,
        (int)p_node->calc.recompute,(int)p_node->calc.status);
#endif
 
   return &(p_node->calc);
}
 
void set_calcs(p_calcs,p_node)
graph_node *p_node;
calc_type *p_calcs;
/* update the calculation structure in p_node with the new data indicated by
   p_calcs
   preconditions:new_step was called to init. p_node,  p_calcs was initialized
                  using get_calcs.
   postconditions:new calc values in p_node
*/
{
 p_node->calc.cpath_time=p_calcs->cpath_time;
 p_node->calc.dep_time=p_calcs->dep_time;
 p_node->calc.recompute=p_calcs->recompute;
 p_node->calc.status=p_calcs->status;
 
#ifdef dbggph
   printf("put calcs %d %d %d %d %d\n",p_node->calc.duration,
        p_node->calc.cpath_time,p_node->calc.dep_time,
        (int)p_node->calc.recompute,(int)p_node->calc.status);
#endif
}
 
server_type *new_proc(p_name,p_list)
char p_name[pname_sz];
list_type *p_list;
/* allocate a new processor node and put it in p_list.  return a pointer
    to it.
    preconditions: p_list initialized
    postconditions: p_list contains new proc node
*/
{
  server_type *new_node;
 
#ifdef dbgsim
  printf("new proc %s\n",p_name);
#endif
 
  new_node=(server_type *) malloc(sizeof(server_type));
  strcpy(new_node->name,p_name);
  new_list(&(new_node->queue));
  new_node->server_num=num_servers++;
  new_node->done_step=NULL;    /* no steps done */
  new_node->comp_time=0;
  new_node->speed_factor = 1.0;
  new_node->sort_factor = 1.0;
  new_node->resched=0;
  new_node->server_ena= TRUE;
  new_node->state=SERVER_DOWN;
  new_node->addr=NULLADDRESS;
  new_node->rank= SRVR_AVAIL;   /* unranked */
  new_node->dummy_speed = 0;
  
  set_server_addr(new_node);
  enqueue((gen_ptr)new_node,p_list);
  return new_node;
}
  
void allocate(p_gnode,p_srvr)
graph_node *p_gnode;
server_type *p_srvr;
/* allocate p_gnode to be executed by  server type p_srvr.
   zero the start and stop times of p_gnode. */
{
#ifdef dbgsim
  printf("alloc %d to %s\n",p_gnode->step_num,p_srvr->name);
#endif
 
  enqueue((gen_ptr)p_gnode,&(p_srvr->queue));
  p_gnode->start_time=0;
  p_gnode->stop_time=0;
  p_gnode->processor=p_srvr;
  p_gnode->calc.status=ALLOCATED;
 
}

dealloc_all()
/* deallocate all of the data items, graph nodes, and server_types in the
   current graph.
   preconditions: the graph has been initialized
   postconditions: graph space is freed
   */
{
  list_node *cur_node;  /* step thru nodes */
  list_node *ref_node;   /* step thru additional pointers to nodes */

  /* deallocate data nodes */
  for (cur_node=data_list.head;cur_node;cur_node=data_list.head)
    {
      for (ref_node=((data_item *)cur_node->content)->dependers.head;
	   ref_node;
	   ref_node=((data_item *)cur_node->content)->dependers.head)
	delete(ref_node,&(((data_item *)cur_node->content)->dependers));

      for (ref_node=((data_item *)cur_node->content)->gen_deps.head;
	   ref_node;
	   ref_node=((data_item *)cur_node->content)->gen_deps.head)
	delete(ref_node,&(((data_item *)cur_node->content)->gen_deps));

      free(((data_item *)cur_node->content)->dname);
      free((data_item *)cur_node->content);
      delete(cur_node,&data_list);
    }

  /* imports */
  for (dequeue(&imports);imports.head;dequeue(&imports)) continue;
  
  /* deallocate graph nodes */
  for (cur_node=step_list.head;cur_node;cur_node=step_list.head)
    {
      for (ref_node=((graph_node *)cur_node->content)->in_list.head;
	   ref_node;
	   ref_node=((graph_node *)cur_node->content)->in_list.head)
	delete(ref_node,&(((graph_node *)cur_node->content)->in_list));
      
      for (ref_node=((graph_node *)cur_node->content)->output.head;
	   ref_node;
	   ref_node=((graph_node *)cur_node->content)->output.head)
	delete(ref_node,&(((graph_node *)cur_node->content)->output));

      if(((graph_node *)cur_node->content)->stdout_file)
	free(((graph_node *)cur_node->content)->stdout_file);
      free(((graph_node *)cur_node->content)->sname);
      if(((graph_node *)cur_node->content)->env)
	{
	  env_ptr cur_env = ((graph_node *)cur_node->content)->env;
	  for(;*cur_env;) free((char *)*cur_env++);
	  free(((graph_node *)cur_node->content)->env);
	}
      free((graph_node *)cur_node->content);
      delete(cur_node,&step_list);
    }

  /* deallocate server nodes */
  for (cur_node=servers.head;cur_node;cur_node=servers.head)
    {
      for (ref_node=((server_type *)cur_node->content)->queue.head;
	   ref_node;
	   ref_node=((server_type *)cur_node->content)->queue.head)
	delete(ref_node,&(((server_type *)cur_node->content)->queue));
      
      free((server_type *)cur_node->content);
      delete(cur_node,&servers);
    }

  /* deallocate the indices */
  if (step_addr) free(step_addr);
  if (item_addr) free(item_addr);
  if (server_addr) free(server_addr);

  /* initialize index system */
   max_steps=max_items=0;    /* no indices allocated */
   step_addr= NULL;
   item_addr= NULL;
   server_addr=NULL;
  
} 
/****************************************************************************/
/* DEPENDENCY MANAGEMENT */
 
void add_in_dep(p_data,p_step)
data_item *p_data;
graph_node *p_step;
/* add an input dependency on p_data to p_step.  This requires addition of
   references in both directions
   preconditions: graph init, both *p_data and *p_step init. using new_data and
                  new_step.  Neither may be nil.
   postconditions: p_data and p_step reflect input dependency
*/
{
#ifdef dbggph
   printf("in dep %d at %x on %d at %x\n",p_step->step_num,p_step,
               p_data->data_num,p_data);
#endif
 
  enqueue((gen_ptr) p_data,&(p_step->in_list));
  enqueue((gen_ptr) p_step,&(p_data->dependers));
  ++p_step->num_deps;
}
 
void add_out_dep(p_data,p_step)
data_item *p_data;
graph_node *p_step;
/* add an output dependency on p_step to p_data.  This requires addition of
   references in both directions
   preconditions: graph init, both *p_data and *p_step init. using new_data and
                  new_step.  Neither may be nil.  p_data not already output.
   postconditions: p_data and p_step reflect output dependency
                   p_step also reflects p_data's gen_deps
*/
{
#ifdef dbggph
   printf("out dep %d at %x on %d at %x\n",p_data->data_num,p_data,
           p_step,p_step->step_num);
#endif

  if (!p_data->source)  /* only add dep. if no other node outputs p_data */ 
  {
    list_node *in_dep;

    enqueue((gen_ptr)p_data,&(p_step->output));
    p_data->source = p_step;

    /* if p_data has any gen_deps, now is the time to add them to p_step */
    for (in_dep=p_data->gen_deps.head;in_dep;in_dep=in_dep->next)
      add_in_dep((data_item *)in_dep->content,p_step);
  } 
  else graph_err= MULTI_OUT_GRAPH_ERR;
    
}
 
void add_gen_dep(p_dep,p_data)
data_item *p_data, *p_dep;
/* add generated dependency on p_dep to p_data.  This implies that whatever
   step outputs p_data must receive p_dep as an input dependency, whether
   or not the output dependency of p_step has been declared.
   
   preconditions: graph init, both *p_data and *p_dep init. using new_data
                  Neither may be nil.
   postconditions: p_data reflects generated dependency
*/
{
#ifdef dbggph
   printf("gen dep %d at %x on %d at %x\n",p_data->data_num,p_data,
               p_dep->data_num,p_dep);
#endif
 
  enqueue((gen_ptr) p_dep,&(p_data->gen_deps));
  
  /* add input dependency to output step if defined yet */
  if (p_data->source)
    add_in_dep(p_dep,p_data->source);
}

/**************************************************************************/
/* sorting the graph */
 
int cmp_data(p_node1,p_node2)
gen_ptr p_node1,p_node2;
/* compare the sorted ordinals or 2 data nodes.  The ordinal of a
   data node is the step number of its source or 0 if it is imported. */
{ int val1,val2;
  val1=val2=0;
  if(p_node1 && ((data_item *) p_node1)->source)
              val1= ((data_item *) p_node1)->source->step_num;
  if(p_node2 && ((data_item *) p_node2)->source)
              val2=((data_item *) p_node2)->source->step_num;
 
#ifdef dbggph
  if(!(val1-val2)) printf(" %3d<%3d ",val1,val2);
  else if((val1-val2)<0) printf(" %3d<%3d ",val1,val2);
       else printf(" %3d>%3d ",val1,val2);
#endif
  return val1-val2;
}
 
int cmp_step(p_node1,p_node2)
gen_ptr p_node1,p_node2;
/* compare the sorted ordinals or 2 process steps.  The ordinal of a
   step is its step number */
 
{ int val1,val2;
  val1=val2=0;
  if(p_node1) val1=((graph_node *) p_node1)->step_num;
  if(p_node2) val2=((graph_node *) p_node2)->step_num;
 
#ifdef dbggph
  if(!(val1-val2)) printf(" %3d<%3d ",val1,val2);
  else if((val1-val2)<0) printf(" %3d<%3d ",val1,val2);
       else printf(" %3d>%3d ",val1,val2);
#endif
  return val1-val2;
}
 
bool sort_graph ()
/* sort the graph.  First conduct a topological sort beginning with the
   nodes sited as imports, numbering nodes as we go.  Then sort step_list
   and all edge lists (in_list and dependers) to reflect this numbering. */
 
/* preconditions: graph initialized and build or read in.  nxt_node pointer
                   in each step points to head of in_list.  imports contains
                   all data items not output by any step.  changed bits on all
                   data items are accurate
   postconditions: if graph is acyclic, all nodes are numbered and step_list,
                   in_lists and dependers lists are ordered by step number.
                   recompute bits on steps are TRUE where required */
 
 /* returns TRUE if graph is acyclic, else FALSE */
 
{
  int next_number;    /* sequence of node numbers */
  list_type proc_Q;   /* queue of nodes whose outputs are to be processed */
  register list_node *cur_data;
                           /* pointer to a list node whose content is data */
  register list_node *cur_step;
                           /* pointer to a list node whose content is step */
  register graph_node *cur_gnode;
                           /* pointer to a step node in a graph */
  int cmp_data (),cmp_step ();
                         /* pointers to functions to compare list elements*/
 
#ifdef dbggph
  printf("sort graph\n");
#endif

  /* initialize node queue and numbering */ 
  new_list(&proc_Q);
  next_number=0;

  /*  initialize cur_dep to num_deps for each step.  Also,
      enqueue any nodes with no input dependencies for processing */

  for (cur_step=step_list.head;cur_step;cur_step=cur_step->next)
    if ((((graph_node *) cur_step->content)->cur_dep=
         ((graph_node *) cur_step->content)->num_deps)==0)
      {
#ifdef dbggph
  printf("assign # %d ",next_number);
#endif
        ((graph_node *) cur_step->content)->step_num= next_number++;
        set_step_addr((graph_node *) cur_step->content);
        enqueue (cur_step->content,&proc_Q);
      }
  
  /* decrement cur_dep for any step that depends on an imported data item.
     enqueue and step whose cur_dep goes to 0. */

  for (cur_data=imports.head;cur_data; cur_data=cur_data->next)
  {
#ifdef dbggph
  printf("from item %d\n",((data_item *) cur_data->content)->data_num);
#endif
 
    for (cur_step=((data_item *) cur_data->content)->dependers.head;cur_step;
           cur_step= cur_step->next)
                                /* for each dependency on imported file, */
    {
#ifdef dbggph
  printf("visit step %d ",((graph_node *) cur_step->content)->step_num);
#endif
      if(((graph_node *) cur_step->content)->cur_dep) /*not all deps found*/
      {  /* if dependency changed, recompute step */
        if(((data_item *) cur_data->content)->changed)
        {
          ((graph_node *) cur_step->content)->calc.recompute = TRUE;
#ifdef dbggph
  printf("recompute ");
#endif
        }
        /* step to next dependency */
        if(!(--((graph_node *) cur_step->content)->cur_dep))
        { /* number dependers=0. assign number and queue for proc. */
#ifdef dbggph
  printf("assign # %d ",next_number);
#endif
          ((graph_node *) cur_step->content)->step_num=
                                                next_number++;
          set_step_addr((graph_node *) cur_step->content);
          enqueue(cur_step->content,&proc_Q);

        }
      }
#ifdef dbggph
  printf("\n");
#endif
    }
  }
  /* now work from process steps in queue */
  for (cur_gnode=((graph_node *) dequeue(&proc_Q));
                        cur_gnode;cur_gnode=((graph_node *)dequeue(&proc_Q)))
  {
    /* no action if step has no output */
    if(cur_gnode->output.head)
    {
      for (cur_data=cur_gnode->output.head;
            cur_data;cur_data=cur_data->next)
      {
#ifdef dbggph
  printf("from item %d at %d\n",((data_item *) cur_data->content)->data_num,
                                      cur_gnode->step_num);
#endif
        for (cur_step=((data_item *) cur_data->content)->dependers.head;
            cur_step;cur_step=cur_step->next)
        {
#ifdef dbggph
  printf("visit step %d ",((graph_node *) cur_step->content)->step_num);
#endif
          /* for each step referenced by output of queue head */
          if(((graph_node *) cur_step->content)->cur_dep)/*not all deps found*/
          {  /* if dependency changed, recompute step */
            if((cur_gnode->calc.recompute)||
              (((data_item *) cur_data->content)->changed))
            {
#ifdef dbggph
  printf("recompute ");
#endif
              ((graph_node *) cur_step->content)->calc.recompute = TRUE;
            }
            /* step to next dependency in node being checked */
            if(!(--((graph_node *) cur_step->content)->cur_dep))
            { /* number dependers=0. assign number and queue for proc. */
#ifdef dbggph
  printf("assign # %d ",next_number);
#endif
              ((graph_node *) cur_step->content)->step_num= next_number++;
              set_step_addr((graph_node *) cur_step->content);
              enqueue(cur_step->content,&proc_Q);

            } /* more dependencies to go */
          } /* node not yet reordered */
        } /* for each step dependent on cur_gnode */
      } /* for each file output by gnode */
#ifdef dbggph
  printf("\n");
#endif
    }  /* if node has output */
  }  /* for each enqueued graph step */
 
  /* having processed all nodes whose indegree went to 0 we will have hit
     all steps unless there was a cycle */
  if (next_number!=num_steps)
    {printf("A cycle was located in the graph");
     graph_err=CYCLE_GRAPH_ERR;
     return FALSE; }
 
  /* time to start sorting lists.  The sort procedure in lst accepts a
      list and a pointer to a 2 parameter function used to compare 2
      list elements. */
  /* first the step list */
  sort_list(&step_list,cmp_step);  

  /* then the input and output dependency lists within steps */
  for(cur_step=step_list.head;cur_step;
         cur_step=cur_step->next)
  {
#ifdef dbggph
  printf("sort step %d\n",((graph_node *) cur_step->content)->step_num);
#endif
   
    sort_list(&(((graph_node *) cur_step->content)->in_list),cmp_data);
    sort_list(&( ((graph_node *)
            cur_step->content)->output),cmp_data);
  }
  /* then the depender list of each data item */
  for(cur_step=data_list.head;cur_step;cur_step=cur_step->next)
    sort_list(&(((data_item *) cur_step->content)->dependers),cmp_step);

  return TRUE;
}
 
void calc_times ()
/*  calculate cpath_time and dep_time for current graph
    preconditions : graph must be read in and sorted.
    postconditions : each step node with recompute set in graph has cpath_time
                         and dep_time set.
 
*/
{
  time_type my_cpath,my_dep;  /* accumulate cpath and dep time */
  list_node *step_node,*dep_node; /* list nodes pointing to step being
                                      evaluated and depender. */
  list_node *data_node;        /* step thru outputs of step_node */
#ifdef dbggph
  printf("calculate ctime,dtime\n");
#endif
 
  for (step_node=step_list.tail;step_node;step_node=step_node->last)
  { graph_node *cur_step = ((graph_node *) step_node->content);
    if (cur_step->calc.recompute==TRUE)
    {
#ifdef dbggph
      printf("step %d",cur_step->step_num);
#endif
      /* step through this step's dependers */
      my_cpath=0;
      my_dep=0;
      for (data_node=cur_step->output.head;data_node;data_node=data_node->next)
        for (dep_node=((data_item *) data_node->content)->dependers.head;
           dep_node;dep_node= dep_node->next)
        { graph_node *visit_step=((graph_node *) dep_node->content);
 
#ifdef dbggph
          printf(" %5d/%5d ",visit_step->calc.cpath_time,
                         visit_step->calc.dep_time);
#endif
 
          my_dep+=visit_step->calc.dep_time+visit_step->calc.duration;
          if (visit_step->calc.cpath_time > my_cpath)
            my_cpath=visit_step->calc.cpath_time;
        }
      cur_step->calc.dep_time=my_dep;
      cur_step->calc.cpath_time=my_cpath+cur_step->calc.duration;
 
#ifdef dbggph
      printf(" yields %5d/%5d\n",cur_step->calc.cpath_time,
                               cur_step->calc.dep_time);
#endif
      cur_step->calc.status=EVAL;
    }
  }
}
 
