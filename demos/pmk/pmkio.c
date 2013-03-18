/*   $RCSfile: pmkio.c,v $ $Revision: 2.2 $ $Date: 90/08/07 16:19:41 $ */

/* intermediate file I/O module
   this module contains routines which writes graph to files from memory
   and read them back.  This allows module testing and cataloging of
   interesting graphs in a partially processed form.
 
   read_graph(name[fname_sz]) - read a graph from the named file
   write_graph(name[fname_sz]) - write a graph to the named file
 
   The file format is described in detail in another document.
   each significant line of the file begins with a keyword which indicates
   the significance of the rest of the line.
 
*/
#include <pmkdat.h>
#include <stdio.h>
#if	defined(HPUX)
char *strcpy(), *strcat();
#else
#include <string.h>
#endif

extern list_type data_list, step_list, imports, servers;

extern num_items, max_items, item_addr, num_steps, max_steps, step_addr;
extern num_servers, max_servers, server_addr;

#define num_keys 20
#define STEP 1
#define TIME 2
#define IN 3
#define OUT 4
#define STEPEND 5
#define CALCS 6
#define RSLT 7
#define PROC 8
#define ENV 9
#define INNUM 10
#define OUTNUM 11
#define STEPNUM 12
#define SIZE 13
#define NEW 14
#define RANK 15
#define SET_DONE 16
#define STDOUT 17
#define SPEED 18
#define DUMDUR 19
#define DUMSPD 20
#define FILEEND 21
typedef int linekey;

typedef struct {
	char str[key_sz];
	linekey num;
} keypair;

keypair keys[num_keys] = {
	"STEP", STEP,
	"TIME", TIME,
	"IN", IN,
	"OUT", OUT,
	"END", STEPEND,
	"CALCS", CALCS,
	"RSLT", RSLT,
	"PROC", PROC,
	"ENV", ENV,
	"IN#", INNUM,
	"OUT#", OUTNUM,
	"STEP#", STEPNUM,
	"SIZE", SIZE,
	"NEW", NEW,
	"RANK", RANK,
	"DONE", SET_DONE,
	"STDO", STDOUT,
	"SPEED", SPEED,
	"DUMDUR", DUMDUR,
	"DUMSPD", DUMSPD
};

char *
itoa(p_int, p_str, p_base)
	int p_int;
	char *p_str;
	int p_base;

/* convert p_int into an ascii string - ignore base, use base 10 */
{
	sprintf(p_str, "%10d", p_int);
	return (p_str);
}

/* format building helper -
   return a string consisting of the first parameter, followed by an
   itoa conversion of the second, followed by the third. */

char *
fmtsis(p_str1, p_int, p_str2)
	char *p_str1, *p_str2;
	int p_int;
{
	static char format[40];
	char scratch[20];

	strcpy(format, p_str1);
	itoa(p_int, scratch, 10);
	strcat(format, scratch);
	strcat(format, p_str2);
	return format;
}

linekey
read_line(p_file, p_string)
	char p_string[line_sz];
	FILE *p_file;

/* read a line from p_file and decipher the key.  return the remainder of the
   string without the key in p_string.
   preconditions: p_file must be opened
   postconditions: the next non-comment, non-blank line is parsed into
                   an enumerated type indicating the line type encountered
                   and a string including the line minus the key.  parse
                   errors are ignored, file errors print to tty and return
                   EOF.
 
*/
{
	char new_line[line_sz];
	char key_string[key_sz];
	char *read_str;
	char *format;
	char scratch[line_sz];
	int items;

	/* build format string to seperate key from rest of string */
	/* format=fmtsis("%s %",line_sz,"c"); */
	do {
		int i;
		char *lineend;

		read_str = fgets(scratch, line_sz, p_file);
		if (read_str) {
			items = sscanf(scratch, "%s %[^\n]", key_string, p_string);

#ifdef dbgio
			printf("read line : key %s  text %s ...", key_string, p_string);
#endif
		}
		/* determine key contents */
		if (feof(p_file)) {
#ifdef dbgio
			printf("eof detected\n");
#endif
			return FILEEND;
		}

		/* look for valid keywords */

		if (read_str && items) {
			if (strcmp(key_string, "*")) {	/* execute body if not comment */
				for (i = 0; i < num_keys; i++) {
					if (!strcmp(key_string, keys[i].str)) {
#ifdef dbgio
						printf("key type %d \n", keys[i].num);
#endif
						return keys[i].num;
					}
				}
				/* no key match - note error */
				if (read_str)
					printf("unrecognised key \n");
			}
		} else
			perror("pmkio read error ");
	}
	while (TRUE);
}

int
add_line(last_key, in_string)
	int last_key;
	char *in_string;

/*  augment the current graph with the information in in_string.
    last_key indicates the type of the line 

    preconditions: in_string is of the format indicated by last_key
                   the graph has been initialized using init_digraph
                   the first key of a new graph should be NEW
    postconditions: The graph is augmented by 1 line.
  */
{
	static graph_node *cur_graph;	/* the graph node from the last STEP keywd. */
	static server_type *cur_server;	/* the server from the last PROC */
	static env_ptr cur_env;		/* the last environment input */
	static list_type env_list;	/* list of environment strings since end of last step.  */
	static data_item *cur_out;	/* last file output with no current step. */
	data_item *cur_data;

	switch (last_key) {
	case NEW:{		/* a new graph is being started */
		cur_graph = NULL;
		cur_server = NULL;
		cur_env = NULL;
		cur_out = NULL;
		new_list(&env_list);
		break;
	}
	case STEP:{		/* define a new command step.  The in_string is the command itself, 
				   also known as the command name.  If there is a current proc,
				   allocate the step to it.  */
		cur_graph = (graph_node *) new_step(in_string);
		cur_graph->env = cur_env;

		if (cur_server)
			allocate(cur_graph, cur_server);
		break;
	}

	case TIME:{
		if (cur_graph)
			cur_graph->calc.duration = atoi(in_string);
		break;
	}

	case DUMDUR:{
		if (cur_graph)
			cur_graph->dummy_duration = atoi(in_string);
		break;
	}
	case IN:{
		char dname[dname_sz];
		char changed_char[2];
		int num_parms, dsize;
		dtype file_type;
		bool dchanged;

		num_parms = sscanf(in_string, "%s %s %d %d",
				   dname, changed_char, &dsize, &file_type);

		if (num_parms < 4)
			file_type = 0;
		if (num_parms < 3)
			dsize = 0;
		if ((num_parms < 2) | (!strcmp(changed_char, "M")))
			dchanged = TRUE;
		else
			dchanged = FALSE;

		if (num_parms >= 1) {
			cur_data = (data_item *) find_data(dname);
			if (!cur_data)
				cur_data = (data_item *)
				    new_data(dname, file_type, dsize, dchanged);
			if (cur_graph)
				add_in_dep(cur_data, cur_graph);
			else if (cur_out)
				add_gen_dep(cur_data, cur_out);

		}
		break;
	}
	case OUT:{
		char dname[dname_sz];
		char changed_char[2];
		int num_parms, dsize;
		dtype file_type;
		bool dchanged;

		num_parms = sscanf(in_string, "%s %s %d %d",
				   dname, changed_char, &dsize, &file_type);
		if (num_parms < 4)
			file_type = 0;
		if (num_parms < 3)
			dsize = 0;
		if ((num_parms < 2) | (!strcmp(changed_char, "M")))
			dchanged = TRUE;
		else
			dchanged = FALSE;

		if (num_parms >= 1) {
			cur_data = (data_item *) find_data(dname);
			if (!cur_data)
				cur_data = (data_item *)
				    new_data(dname, file_type, dsize, dchanged);
			else {
				/* update old data item */
				cur_data->size = dsize;
				cur_data->file_type = file_type;
				cur_data->changed = dchanged;
			}
			if (cur_graph)
				add_out_dep(cur_data, cur_graph);
			else
				cur_out = cur_data;
		}
		break;
	}
	case STDOUT:{
		if (cur_graph)
			cur_graph->stdout_file = (char *) alloc_strcpy(in_string);
		break;
	}
	case STEPEND:{
		/* if there have been any environment strings posted since the end of the last
		   step, convert them to an env_ptr and post them with this step. */
		env_ptr temp_env;	/* temp pointer to new env. */

		/* contsruct the new environment */
		if (env_list.head) {
			temp_env = (env_ptr) build_env(&env_list);

			/* no need to keep new env. if old was same. */
			if (envcmp(cur_env, temp_env))
				cur_graph->env = cur_env = temp_env;
			else {	/* free the temp env structure */
				env_ptr free_env = temp_env;

				for (; *temp_env; temp_env++)
					free(*temp_env);
				free(free_env);
			}
		}

		cur_graph = NULL;
		cur_out = NULL;
		break;
	}

	case CALCS:{
		calc_type *new_calcs;
		char changed_char[2];
		int num_parms, stat_int;

		if (cur_graph) {
			new_calcs = (calc_type *) get_calcs(cur_graph);
			stat_int = (int) new_calcs->status;

			num_parms = sscanf(in_string, "%d %d %s %d %d",
					   &(new_calcs->cpath_time),
					   &(new_calcs->dep_time),
					   changed_char, &stat_int, &(cur_graph->cur_dep));

			if (!strcmp(changed_char, "M"))
				new_calcs->recompute = TRUE;
			else
				new_calcs->recompute = FALSE;

			new_calcs->status = (sstat_type) stat_int;

			set_calcs(new_calcs, cur_graph);
		}
		break;
	}
	case RSLT:{
		if (cur_graph) {
			sscanf(in_string, "%d %d %d %d", &(cur_graph->start_time),
			       &(cur_graph->stop_time), &(cur_graph->exp_stop_time),
			       &(cur_graph->deny_time));
		}
		break;
	}
	case SPEED:{
		if (cur_graph)
			sscanf(in_string, "%f", &(cur_graph->speed_factor));
		else if (cur_server)
			sscanf(in_string, "%f", &(cur_server->speed_factor));
		break;
	}
	case DUMSPD:{
		if (cur_server)
			sscanf(in_string, "%f", &(cur_server->dummy_speed));
		break;
	}
	case PROC:{
		cur_server = (server_type *) new_proc(in_string, &servers);
		cur_graph = NULL;
		break;
	}
	case RANK:{
		if (cur_server)
			sscanf(in_string, "%d", &(cur_server->rank));
		break;
	}
	case SET_DONE:{
		if (cur_server)
			cur_server->done_step = cur_server->queue.tail;
		break;
	}
	case ENV:{		/* allocate a new environment string */
		enqueue((gen_ptr) alloc_strcpy(in_string), &env_list);
		break;
	}
	case INNUM:{		/* add a dependency based on a previously described step, referring 
				   to its step number */
		int item_num;		/* the number of the step referenced */

		if (sscanf(in_string, "%d", &item_num) == 1) {	/* verify that the item indicated
								   by the index has the indicated
								   number */
			if (item_addr && (item_num < max_items))
				if (cur_data = (*((item_addr_type *) item_addr))[item_num])
					if (cur_data->data_num == item_num) {	/* everything
										   checks out - use 
										   the step */
						if (cur_graph)
							add_in_dep(cur_data, cur_graph);
						else if (cur_out)
							add_gen_dep(cur_data, cur_out);
					}
		}
		break;
	}
	case OUTNUM:{		/* add a dependency based on a previously described step, referring 
				   to its step number */
		int item_num;		/* the number of the step referenced */

		if (sscanf(in_string, "%d", &item_num) == 1) {	/* verify that the item indicated
								   by the index has the indicated
								   number */
			if (item_addr && (item_num < max_items))
				if (cur_data = (*((item_addr_type *) item_addr))[item_num])
					if (cur_data->data_num == item_num) {	/* everything
										   checks out - use 
										   the step */
						if (cur_graph)
							add_out_dep(cur_data, cur_graph);
						else
							cur_out = cur_data;
					}
		}
		break;
	}
	case STEPNUM:{		/* allocate the step with the indicated number to the current
				   processor if there is one */
		int step_num;		/* the number of the step referenced */

		if (sscanf(in_string, "%d", &step_num) == 1) {	/* verify that the step indicated
								   by the index has the indicated
								   number */
			if (step_addr && (step_num < max_steps))
				if (cur_graph = (*((step_addr_type *) step_addr))[step_num])
					if (cur_graph->step_num == step_num) {	/* everything
										   checks out - use 
										   the step */
						if (cur_server) {
							enqueue((gen_ptr) cur_graph,
								&(cur_server->queue));
							cur_graph->processor = cur_server;
						}
					}
		}
		break;
	}
	case SIZE:{		/* allocate index arrays of the indicated sizes */
		int in_steps, in_items, in_srvrs;	/* sizes read */

		if (sscanf(in_string, "%d %d %d", &in_steps, &in_items, &in_srvrs) == 3)
			alloc_index(in_items, in_steps, (in_srvrs > MIN_SRVRS ?
							 in_srvrs : MIN_SRVRS));
		break;
	}

	default:;

	}
}

void
read_graph(p_fname)
	char p_fname[fname_sz];

/* initialize and read in a graph from file p_fname */
/* preconditions: p_fname exists
   postconditions: graph specified by file is in memory */
{
	FILE *stream;
	char in_string[line_sz];
	list_node *cur_node;
	int last_key;

	if (!(stream = fopen(p_fname, "r"))) {
		printf("%s: ", p_fname);
		fflush(stdout);
		perror("pmkio open error 1");
		return;
	}
#ifdef dbgio
	printf("open %s \n", p_fname);
#endif

	do {
		last_key = read_line(stream, in_string);
		add_line(last_key, in_string);

	} while (last_key != FILEEND);

	fclose(stream);

	/* add data nodes with NULL sources to imports */
	for (cur_node = data_list.head; cur_node; cur_node = (list_node *) cur_node->next)
		if (!((data_item *) cur_node->content)->source)
			enqueue(cur_node->content, &imports);
	return;
}

void
write_graph(p_fname)
	char p_fname[fname_sz];

/* write a graph to file p_fname */
/* preconditions: graph initialized - STEPS AND ITEMS MUST BE NUMBERED
                    IN THE EXACT ORDER REPRESENTED BY STEP_LIST AND DATA_LIST.
                  server_list must be initialized 
   postconditions: graph copy in file, graph unchanged */
{
	FILE *stream;
	graph_node *cur_graph;
	list_node *cur_data;
	list_node *list_pos;
	env_ptr cur_env;

	cur_env = NULL;
	if ((int) (stream = fopen(p_fname, "w")) < 0) {
		printf("%s: ", p_fname);
		perror("pmkio open error 2");
		return;
	}
#ifdef dbgio
	printf("open %s \n", p_fname);
#endif
	/* preliminaries : new graph, state size to get index. */
	fprintf(stream, "NEW\n");
	fprintf(stream, "SIZE %d %d %d\n", num_steps, num_items, num_servers);

/* put list of data items at start */
	for (list_pos = data_list.head; list_pos; list_pos = list_pos->next) {
		fprintf(stream, "*\n* DATA ITEM NUMBER %d\n*\n",
			((data_item *) list_pos->content)->data_num);

		if (((data_item *) list_pos->content)->changed)
			fprintf(stream, "IN %s M %d %d\n",
				((data_item *) list_pos->content)->dname,
				((data_item *) list_pos->content)->size,
				(int) ((data_item *) list_pos->content)->file_type);
		else
			fprintf(stream, "IN %s U %d %d\n",
				((data_item *) list_pos->content)->dname,
				((data_item *) list_pos->content)->size,
				(int) ((data_item *) list_pos->content)->file_type);
	}
	/* now the steps */
	list_pos = step_list.head;
	for (list_pos = step_list.head; list_pos; list_pos = list_pos->next) {
		int temp_stat;

		cur_graph = (graph_node *) list_pos->content;
		fprintf(stream, "*\n* STEP NUMBER %d\n*\n", cur_graph->step_num);
		fprintf(stream, "STEP %s\n", cur_graph->sname);
		if (cur_graph->stdout_file)
			fprintf(stream, "STDO %f\n", cur_graph->stdout_file);
		fprintf(stream, "TIME %d\n", cur_graph->calc.duration);

		/* supress locked status */
		temp_stat = cur_graph->calc.status;
		if (temp_stat == SCHED_LOCK)
			temp_stat = SCHEDULED;

		if (cur_graph->calc.recompute)
			fprintf(stream, "CALCS %d %d M %d %d\n",
				cur_graph->calc.cpath_time,
				cur_graph->calc.dep_time, temp_stat, cur_graph->cur_dep);
		else
			fprintf(stream, "CALCS %d %d U %d %d\n",
				cur_graph->calc.cpath_time,
				cur_graph->calc.dep_time, temp_stat, cur_graph->cur_dep);

		/* print inputs */
		for (cur_data = cur_graph->in_list.head; cur_data; cur_data = cur_data->next)
			fprintf(stream, "IN# %d\n", ((data_item *) cur_data->content)->data_num);

		/* and outputs */
		for (cur_data = cur_graph->output.head; cur_data; cur_data = cur_data->next)
			fprintf(stream, "OUT# %d\n", ((data_item *) cur_data->content)->data_num);

		/* and environment, if it's different */
		if (cur_graph->env && (cur_graph->env != cur_env)) {
			for (cur_env = cur_graph->env; *cur_env; ++cur_env)
				fprintf(stream, "ENV %s\n", *cur_env);
			cur_env = cur_graph->env;
		}
		/* and finally the time projections and results */
		fprintf(stream, "RSLT %d %d %d %d\n", cur_graph->start_time,
			cur_graph->stop_time, cur_graph->exp_stop_time, cur_graph->deny_time);

		if (cur_graph->speed_factor != 1.0)
			fprintf(stream, "SPEED %f\n", cur_graph->speed_factor);

		if (cur_graph->dummy_duration)
			fprintf(stream, "DUMDUR %d\n", cur_graph->dummy_duration);

		fprintf(stream, "END\n");
	}
	/* and print out allocations of steps to processors */
	for (list_pos = servers.head; list_pos; list_pos = list_pos->next) {
		list_node *cur_step;

		fprintf(stream, "PROC %s\n", ((server_type *) list_pos->content)->name);

		/* if server is UP and executing steps, indicate rank in current view */
		if (((server_type *) list_pos->content)->state != SERVER_DOWN)
			fprintf(stream, "RANK %d\n", ((server_type *) list_pos->content)->rank);

		/* if server is AVAIL_WORK, indicate RANK. */
		if (((server_type *) list_pos->content)->rank == AVAIL_WORK)
			fprintf(stream, "RANK %d\n", ((server_type *) list_pos->content)->rank);

		if (((server_type *) list_pos->content)->speed_factor != 1.0)
			fprintf(stream, "SPEED %f\n",
				((server_type *) list_pos->content)->speed_factor);

		if (((server_type *) list_pos->content)->dummy_speed)
			fprintf(stream, "DUMSPD %f\n",
				((server_type *) list_pos->content)->dummy_speed);

		for (cur_step = ((server_type *) list_pos->content)->queue.head;
		     cur_step; cur_step = cur_step->next) {
			fprintf(stream, "STEP# %d\n", ((graph_node *) cur_step->content)->step_num);
			if (((server_type *) list_pos->content)->done_step == cur_step)
				fprintf(stream, "DONE\n");
		}
	}

	fclose(stream);
}

typedef struct {
	time_type time;
	int step;
	bool stop;
	int procno;
	char proc[pname_sz];
} event_rec;

int
cmp_events(p_node1, p_node2)
	gen_ptr *p_node1, *p_node2;
{
	return (((event_rec *) p_node1)->time - ((event_rec *) p_node2)->time);
}

void
show_run(p_fname, p_slist, p_title)
	char p_fname[fname_sz];
	char p_title[line_sz];
	list_type *p_slist;

/* display the result of running or simulating p_slist
   preconditions: p_slist is initialized
   post_conditions: p_slist unchanged, p_fname written.
*/
{
	FILE *stream;
	list_node *srvr_node;
	char *format;
	list_type event_list;
	event_rec *event;

	int on_off[32];			/* indicates whether each server is running or not */
	int min_time = 0;		/* normalize all times to minimum */
	int srvno;

#ifdef dbgio
	printf("open %s \n", p_fname);
#endif
	if ((int) (stream = fopen(p_fname, "w")) < 0) {
		printf("%s: ", p_fname);
		perror("pmkio open error 3");
		return;
	}
	fprintf(stream, "%s\n", p_title);
	fprintf(stream, "\n");

	new_list(&event_list);

	for (srvr_node = p_slist->head; srvr_node; srvr_node = srvr_node->next) {
		list_node *step_node;

		fprintf(stream, "Total CPU time on %s = %d, speed factor = %f\n",
			((server_type *) srvr_node->content)->name,
			((server_type *) srvr_node->content)->comp_time,
			((server_type *) srvr_node->content)->speed_factor);

		for (step_node = ((server_type *) srvr_node->content)->queue.head;
		     step_node; step_node = step_node->next) {
			event_rec *strt_event = (event_rec *) malloc(sizeof(event_rec));
			event_rec *stop_event = (event_rec *) malloc(sizeof(event_rec));

			strt_event->time = ((graph_node *) step_node->content)->start_time;
			strt_event->step = ((graph_node *) step_node->content)->step_num;
			strt_event->stop = FALSE;
			strt_event->procno = ((server_type *) srvr_node->content)->server_num;
			strcpy(strt_event->proc, ((server_type *) srvr_node->content)->name);

			enqueue(strt_event, &event_list);

			stop_event->time = ((graph_node *) step_node->content)->stop_time;
			stop_event->step = strt_event->step;
			stop_event->stop = TRUE;
			stop_event->procno = strt_event->procno;
			strcpy(stop_event->proc, strt_event->proc);

			if (!min_time || (strt_event->time < min_time))
				min_time = strt_event->time;

#ifdef dbgio
			printf("gen event step %d on %s\n", stop_event->step, stop_event->proc);
#endif

			enqueue(stop_event, &event_list);
		}
	}
	sort_list(&event_list, cmp_events);

	/* init srvr map and print headers */
	fprintf(stream, "                                           ");

	for (srvno = 0; srvno < num_servers; srvno++) {
		fprintf(stream, "%4d", srvno);
		on_off[srvno] = 0;
	}
	fprintf(stream, "\n");

	for (event = (event_rec *) dequeue(&event_list); event;
	     event = (event_rec *) dequeue(&event_list)) {
		if (event->stop == TRUE)
			fprintf(stream, "%6d END %4d ON %24s",
				event->time - min_time, event->step, event->proc);
		else
			fprintf(stream, "%6d BGN %4d ON %24s",
				event->time - min_time, event->step, event->proc);

		for (srvno = 0; srvno < num_servers; srvno++) {
			if (srvno == event->procno) {
				if (event->stop == TRUE) {
					fprintf(stream, "   -");
					--on_off[srvno];
				} else {
					fprintf(stream, "   +");
					++on_off[srvno];
				}
			} else {
				if (on_off[srvno])
					fprintf(stream, "   |");
				else
					fprintf(stream, "    ");
			}
		}
		fprintf(stream, "\n");

		free(event);
	}
	fclose(stream);
}
