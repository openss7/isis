/*  $RCSfile: pr_main.c,v $ $Revision: 2.17 $ $Date: 90/08/12 20:37:42 $  */
/*
 *	Originally coded by Ken Birman
 *
 *
 *      ISIS release V2.0, May 1990
 *      Export restrictions apply
 *
 *      The contents of this file are subject to a joint, non-exclusive
 *      copyright by members of the ISIS Project.  Permission is granted for
 *      use of this material in unmodified form in commercial or research
 *      settings.  Creation of derivative forms of this software may be
 *      subject to restriction; obtain written permission from the ISIS Project
 *      in the event of questions or for special situations.
 *      -- Copyright (c) 1990, The ISIS PROJECT
 *
 *
 */
#include "pr.h"
#include <sys/stat.h>

#define NETLIM          2               /* Take 66% of load from network */

#define NORMAL          1               /* Normal termination */
#define HALTED          2               /* Halt() of a site */
#define PANIC           -1              /* Panic termination */

int     staged_xmit, ISIS_TIME;
int     nsites, append_to_log;
static  running;
static  coredump;			/* Debugging feature */
static  struct  timeval sel_wait ={ 0,  250 }, no_wait = { 0, 0 };
static  struct timeval time_0;
static  char logfile[128];
static  char *sname;
static  long cur_t;
int     FDTIMEOUT = 60;

static char  my_full_name[64];
#define GETHOSTTIMEOUT  5
char *strchr();

static restartchk()
  {
        if(proposed_viewid == 0 && my_site_incarn == RECOVERY_INCARN)
             panic("site-restart sequence failure");
  }

static jmp_buf alarm_env;

void
get_host_alarm()
{
    longjmp(alarm_env, 1);
}

struct hostent*
get_host_with_timeout(name, seconds)
  char *name;
  int seconds;
  /* Does a gethostbyname call with a timeout. If gethostbyname
     doesn't return within "seconds" seconds, NULL is returned.
     Uses alarm signals.
     Must not be called after the isis task mechanism has been 
     initialized since that uses alarms too.
  */
{
    struct hostent *hep;
    signal(SIGALRM, get_host_alarm);
    alarm(seconds);
    if (setjmp(alarm_env) == 0)
    {
        hep = gethostbyname(name);
    }
    else
    {
        print("gethostbyname(\"%s\") timed out.\n", name);
        hep = NULL;
    }
    alarm(0);
    return(hep);
}        

main(argc, argv)
  char **argv;
  {
        register i;
        struct hostent *my_hep;

        for(i = 0; i < MAXBITS; i++)
            bis(&ones, i);
        gethostname(my_host, 64);
        my_hep = get_host_with_timeout(my_host, GETHOSTTIMEOUT*2);
            /* Wait a while when finding my host name. */
        if (my_hep == 0)
        {
            print("gethostbyname(\"%s\") failed, unable to get full hostname.\n",
                  my_host);
            print("Continuing using just the shortname \"%s\" .\n", my_host);
            my_full_name[0] = 0;
        }
        else
        {
            register char *s;
            s = my_hep->h_name;
            while(*s && *s != '.')
                ++s;
            if(*s)
            {
                strncpy(my_full_name, my_hep-> h_name, 64);
                my_full_name[63] = 0;
            }
            else
            {
                print("WARNING: /etc/hosts doesn't list full hostname.\n");
                print("Continuing using just the shortname \"%s\" .\n", my_host);
                my_full_name[0] = 0;
            }
        }

        for (i = 0; i < MAX_SITES; i++)
            current_view.sv_incarn[i] = DOWN_INCARN;
        my_process_id = PROTOCOLS;
        my_site_incarn = RECOVERY_INCARN;
        sname = "./sites";
        isis_dir = "/usr/spool/isis";
        while(--argc)
        {
            register char *arg;
            register code;

            arg = *++argv;
            code = *arg++;
            if(code != '-')
            {
              bum_arg:
                print("Bad argument to <protos>: %s (ignored)\n", --arg);
                continue;
            }
            while(code = *arg++)
                switch(code)
                {
                    extern crash_self;

                  default:
                    arg = (*argv)+1;
                    goto bum_arg;

                  case 'a':
                    ++append_to_log;
                    continue;

                  case 'C':
                    ++crash_self;
                    continue;

		  case 'c':
		    if(*arg == '-')
			coredump = -1;
		    else
		        coredump = atoi(arg);
		    goto next;

                  case 'u':
                    ++msg_usagestats;
                    continue;

                  case 'm':
                    ++msg_tracemsgs;
                    continue;

                  case 't':
                    ++msg_tracecaller;
                    continue;
 
                 case 'f':
                    FDTIMEOUT = atoi(arg);
                    goto next;

                  case 's':
                    ++staged_xmit;
                    continue;

                  case 'N':
                    NSITES = atoi(arg);
                    goto next;

                  case 'd':
                    if(*arg == 0)
                    {
                        --argv;
                        arg = *++argv;
                    }
                    isis_dir = arg;
                    goto next;

                  case 'S':
                    if(*arg == 0)
                    {
                        --argv;
                        arg = *++argv;
                    }
                    sname = arg;
                    goto next;

                  case 'H':
                    if(*arg == 0)
                    {
                        --argv;
                        arg = *++argv;
                    }
                    my_site_no = atoi(arg);
                    goto next;
                }
	  next:
          	continue;
        }

        scan_sfile();

	begin
	{
	    register char *is, *bp;
	    register c;
	    struct stat sbuf;
	    bp = isis_dir;
	    is = isis_dir = (char*)malloc(strlen(isis_dir)+2);
	    while(c = *is++ = *bp++)
	        if(c == '#')
		{
		    sprintf(--is, "%d", my_site_no);
                    while(*++is)
			continue;
		}
                if(stat(isis_dir, &sbuf) == -1)
                {
                    int old_umask = umask(0);
                    int mk_stat;
                    print("mkdir %s\n", isis_dir);
                    mk_stat = mkdir(isis_dir, 0777);
                    umask(old_umask);
                    if (mk_stat != 0)
                    {
                        perror("");
                        panic(" protos can't create %s ", isis_dir);
                    }
                }
	}

        /* Initialize entry points */
        isis_entries();


        if (fail_msgno)
        {
            FILE    *failfile;
            char    failname[16];

            sprintf (failname, "%d.fail", my_site_no);
            if ((failfile = fopen (failname, "r")) == (FILE *) NULL)
                fail_msgno = fail_sender = 0;
            else
            {
                fscanf (failfile, "%d %d", &fail_msgno, &fail_sender);
                fclose (failfile);
            }
        }

        begin
        {
            register old, new;
	    struct timeval rtime;
            int open_mode;

            close(old = fileno(stdout));
            if (append_to_log)
            {
                open_mode = (O_WRONLY|O_CREAT|O_APPEND);
            }
            else
            {
                open_mode = (O_WRONLY|O_CREAT|O_TRUNC);
            }
            sprintf(logfile, "%s/%d.log", isis_dir, my_site_no);

            new = open(logfile, open_mode, 0666);
            if(new == -1)
            {
                /* If the file already exists try to fix its protection
                   or delete it. */
                if (append_to_log && (access(logfile, 0) == 0))
                {
                    chmod(logfile, 0666);
                    new = open(logfile, open_mode, 0666);
                }
                if (new == -1)
                {
                    unlink(logfile);
                    new = open(logfile, open_mode, 0666);
                }
                if (new == -1)
                {
                    fprintf(stderr, "<isis-protos>: Unable to create system log file <%s>!\n", logfile);
                    exit(-1);
                }
            }
            if(new != old)
                dup2(new, old);
	    gettimeofday(&rtime, (struct timezone*)0);
	    print(ctime(&rtime.tv_sec));
        }
        main_loop();
  }

/* rescan the sites file */
pr_rescansfile(mp)
  message *mp;
  {
        message *msg = msg_newmsg();
        BCAST_V(&current_view, PROTOCOLS, PR_DOSCANSITES, msg, 0, 0, 0, 0);
        msg_delete(msg);
        reply(mp, &nsites, FTYPE_LONG, sizeof(long));
  }

pr_doscansfile()
  {
        scan_sfile();
  }

static  qnode *scope_queue;

/* Set bit in scope variable */
set_scope(scope, bno)
  char *scope;
  {
        register qnode *qp;
        register len;
        if(*scope == 0)
            return;
        for(qp = scope_queue->qu_next; qp != scope_queue; qp = qp->qu_next)
            if(strcmp(qp->qu_sname, scope) == 0)
            {
                bis(&qp->qu_bitvec, bno);
                return;
            }
        len = strlen(scope)+1;
        qp = qu_add_bits(scope_queue, malloc(len));
        bcopy(scope, qp->qu_sname, len);
        bis(&qp->qu_bitvec, bno);
  }

/* Set slist from scope */
sl_scope(scope, slist)
  register site_id *slist;
  register char *scope;
  {
        register flag = 0;
        register qnode *qp;
        register site_id *sp;
        bitvec search_scope;
        if(scope == 0)
            scope = "";
        *slist = 0;
        bclr(&search_scope);
        for(qp = scope_queue->qu_next; qp != scope_queue; qp = qp->qu_next)
            if(strcmp(qp->qu_sname, scope) == 0)
                break;
        if(qp != scope_queue)
        {
            bisv(&search_scope, &qp->qu_bitvec);
        }
        else switch(*scope)
        {
          case '*':
            bset(&search_scope);
            break;
          default:
            return(0);
          case 0:
            for(qp = scope_queue->qu_next; qp != scope_queue; qp = qp->qu_next)
                if(bit(&qp->qu_bitvec, my_site_no))
                    bisv(&search_scope, &qp->qu_bitvec);
            if(btst(&search_scope))
                bis(&search_scope, my_site_no);
            else
                bset(&search_scope);
            break;
        }
        for(sp = current_view.sv_slist; *sp; sp++)
            if(bit(&search_scope, SITE_NO(*sp)))
            {
                if(SITE_NO(*sp) == my_site_no)
                    ++flag;
                *slist++ = *sp;
            }
        *slist = 0;
        return(flag);
  }

dump_scopes()
  {
        register qnode *qp;
        if(scope_queue)
            for(qp = scope_queue->qu_next; qp != scope_queue; qp = qp->qu_next)
            {
                print("Scope <%s> = ", qp->qu_sname);
                dump_bitv(&qp->qu_bitvec);
                print("\n");
            }
  }

/* Ultimately used in clib/cl_inter.c in "udp_rpc" */
int	ISIS_PORTNO;

scan_sfile()
  {
        FILE *sfile;
        char str[64], scope[64];
        int port, cport, rport, sid, warn_remote;
        static first_time;
        register qnode *qp;
    
        if(scope_queue == 0)
            scope_queue = qu_null();
        while(qp = qu_head(scope_queue))
        {
            free(qp->qu_sname);
            qu_free(qp);
        }
        if((sfile = fopen(sname, "r")) == (FILE*)NULL)
            panic("%s: cannot open", sname);
        nsites = 0;
	warn_remote = 0;
        forever
        {
            register isup = fgetc(sfile);
            register c;
            if(fscanf(sfile, "%d:%d,%d,%d %s", &sid, &port, &cport, &rport, str) < 5)
                break;
            else if(sid < 0 || sid >= MAX_SITES)
                print("Site number %d out of bounds, ignored!\n", sid);
            else if(isup == '+')
            {
                if(*site_names[sid] && strcmp(site_names[sid], str))
		    print("Warning: site number %d <%s>: redeclared as <%s>!\n", sid, site_names[sid], str);
                strcpy(site_names[sid], str);

                /* Scan scope information */
                c = fgetc(sfile);
                while(c > 0 && c != '\n')
                {
                    register char *sp;
                    while(c == ',' || c == ' ')
                        c = fgetc(sfile);
                    sp = scope;
                    while(c > 0 && c != ',' && c != ' ' && c != '\n')
                    {
                        *sp++ = c;
                        c = fgetc(sfile);
                    }
                    *sp = 0;
                    if(*scope)
                        set_scope(scope, sid);
                }
                udp_port[sid] = port;
                connect_port[sid] = cport;
	        if(ISIS_PORTNO && rport && ISIS_PORTNO != rport)
		    ++warn_remote;
		else if(ISIS_PORTNO == 0)
		    ISIS_PORTNO = rport;
                if(sid > nsites)
                    nsites = sid;
                if(my_site_no == 0)
                {
                    bool found;
                    /* str still contains site_name[sid] at this point. */

                    if (my_full_name[0] == 0) {
                        /* No full name. For compatibility we'll compare
                           the shortname with a prefix of the components
                           in the hostentry.
                        */
                        int len = strlen(my_host);
                        int len1 = strlen(str);
                        if (len1 > len && str[len] == '.')
                        {   /* Truncate name from file only at a dot. */
                            str[len] = 0;
                        }
                        found = (strcmp(my_host, str) == 0);
                    }
                    else
                    {
			char *dot = 0;
			int dot_in_str = (strchr(str, '.') != NULL);
			int dot_in_name = (strchr(my_full_name, '.') != NULL);
                        if(dot_in_str != dot_in_name)
			{
			    if((dot = strchr(str, '.')) == NULL)
				dot = strchr(my_full_name, '.');
			    *dot = 0;
			}
                        found = (strcmp(my_full_name, str) == 0);
			if(dot)
			    *dot = '.';
                    }
                    if (found) 
                    {
                        my_site_no = sid;
                    }
                }
                continue;
            }
            else
                while((c = fgetc(sfile)) > 0 && c != '\n')
                    continue;
        }
        if(my_site_no == 0)
            panic("%s: not found in site-file \"%s\"", my_host, sname);
	if(warn_remote)
	    print("Warning: 'sites' file gives multiple values for bcast port\n... (note that isis_remote uses this port number)\n");
        fclose(sfile);
        if(first_time++)
        {
            message *mp = msg_genmsg(CL_NEW_SNAMES, (char*)site_names, FTYPE_CHAR, nsites * sizeof(*site_names), 0);
            cl_send_all(GENERIC_NEW_SNAMES, mp);
            msg_delete(mp);
        }
  }

/* Causes a site to abort and dump core */
void dump(c)
  {
        pr_dump(DUMP_ALL, "signal DUMP");
  }

void done(c)
  {
        print("Hangup signal received\n");
	exit(0);
  }

#define must_wait()     (runqueue == 0 && tank_status() == TANK_EMPTY)
extern FILE *statfile;
int    cl_ccount, cl_congested;

main_loop()
  {
        int max;
        bitvec in_mask;

        gettimeofday(&time_0, (struct timezone*)0);
        my_site_id = MAKE_SITE_ID(my_site_no, my_site_incarn);
        my_address = ADDRESS(my_site_no, my_site_incarn, PROTOCOLS, GENERIC_RCV_REPLY);
        my_startup_addr = my_address;
        pr_initialize();
        signal(SIG_DUMP, dump);
        signal(SIGPIPE, SIG_IGN);
        signal(SIGINT, done);
        ++running;
        print("%s\n", FULL_VERSION);
	print("Site <%s> is now coming up, site-id %d, isis_dir <%s>\n", my_host, my_site_no, isis_dir);
        print("Detect site-failure after: %d secs\n", FDTIMEOUT);
        net_init();
        client_init();

        bclr(&in_mask);
        max = 32;
        if(NSITES)
        {
            char statname[20];
            int save_stats();
            sprintf(statname, "%d.stats", my_site_no);
            statfile = fopen(statname, "w");
            timeout(5000, save_stats, 0, 0);
        }
        timeout(120000, restartchk, 0, 0);
        forever
        {
            register rv;
            int memuse = memalloc-memfree, msgmem = msg_memused-msg_memfree;
            extern net_xmits;
            int n_net_inputs;

            /* First run tasks for a while */
            run_tasks();

            /* Now deliver input for a while */
            n_net_inputs = 0;
            while(n_net_inputs < NETLIM)
            {
                switch(net_deliver())
                {
                  case TANK_EMPTY:
                        n_net_inputs = NETLIM;
			break;
                  case TANK_ATEONE:
                        ++n_net_inputs;
                  case TANK_OTHER:
                        continue;
                }
            }

            if(cl_congested == 0 && (memuse > MEM_HI || msgmem > MSGMEM_HI || ntasks > TASK_HI || intersite_congested > INTERSITE_HI))
            {
                ++cl_congested;
		EVENT(S_CONGEST);
                cl_send_all(GENERIC_CONGESTED, (message*)0);
                /* Trigger garbage collection */
	        did_delete(1);
            }
            else if(cl_congested && (memuse < MEM_LO && msgmem < MSGMEM_LO && ntasks < TASK_LO && intersite_congested < INTERSITE_LO))
            {
                --cl_congested;
                cl_send_all(GENERIC_DECONGESTED, (message*)0);
            }
            /* Check for input */
            if(btst(&in_mask) == 0)
            {
                bitvec out_mask, ex_mask;
                register mw;
                out_mask = congested;
                in_mask = input_mask;
            again:
                if(runqueue == 0)
                {
                    register qnode *qp;
                    if(qp = qu_head(time_queue))
                    {
                        register time = qp->qu_time-Gettime();
                        if(time < 250)
                            time = 250;
                        sel_wait.tv_sec = time/1000;
                        sel_wait.tv_usec = (time % 1000)*1000;
                    }
                    else
                        panic("net_sweep not listed in timeout qnode!");
                }
                if(tank_status() == TANK_FULL)
                    bic(&in_mask, net_socket);
                mw = must_wait();
		ex_mask = in_mask;
		bisv(&ex_mask, &out_mask);
                if((rv = select(max, &in_mask, &out_mask, &ex_mask, mw? &sel_wait: &no_wait)) == -1)
                {
                    if(errno == EINTR)
                        goto again;
                    if(errno == EBADF)
                    {
                        register b;
			static count;
                        /* Some client died violently */
                        for(b = 0; b < max; b++)
                        {
                            if(bit(&input_mask, b) == 0 && bit(&congested, b) == 0)
                                continue;
                            bclr(&in_mask);
                            bclr(&out_mask);
                            if(bit(&input_mask, b))
                                bis(&in_mask, b);
                            if(bit(&congested, b))
                                bis(&out_mask, b);
                            if(select(max, &in_mask, &out_mask, (int*)0, &no_wait) == -1 && errno == EBADF)
                                if(b == net_socket)
                                    panic("select -- bad net_sock");
                                else
                                    client_failed(b, TRUE);
                        }
                        goto again;
                    }
                    perror("select");
                    panic("select");
                }
		/* Make exceptions look like ready input */
		bisv(&in_mask, &ex_mask);
                if(mw)
		{
		    register qnode *qp;
		    register delta;
                    Gettime();
                    if((qp = qu_head(time_queue)) && (delta = (ISIS_TIME-qp->qu_time)) > 1000)
			/* Detect small time jumps here */
			time_warp(delta);
	        }
                if(rv == 0)
                    bclr(&in_mask);
                if(btst(&out_mask))
                    cl_restart_io(out_mask);
            }
            net_xmits = 0;
            if(bit(&in_mask, net_socket))
            {
                /* Tank all pending network input. */
                net_drain_input();
                bic(&in_mask, net_socket);
            }
            if(runqueue == 0 && btst(&in_mask))
            {
                register b, i = 0;
                if(bit(&in_mask, connect_bit))
                {
                    bic(&in_mask, connect_bit);
                    client_accept();
                    while(max <= max_cl)
                        max += 32;
                }
                for(b = 0; b <= max_cl; b++)
                {
                    if(bit(&in_mask, b))
                    {
                        pr_local_rcv(i);
                        bic(&in_mask, b);
                        break;
                    }
                    ++i;
                }
            }
            cur_t = ISIS_TIME;
            begin
            {
                register qnode *qp;
                while((qp = qu_head(time_queue)) && qp->qu_time <= cur_t)
                {
                    qu_remove(qp);
                    qu_free(qp);
                }
            }
        }
  }


/* Specifies a timeout in milliseconds */
int     TIMEOUT_ID;

timeout(time, routine, arg0, arg1)
  int (*routine)();
  char *arg0, *arg1;
  {
        return(timeout_reschedule(0, time, routine, arg0, arg1));
  }

timeout_reschedule(oldtid, time, routine, arg0, arg1)
  register time, oldtid;
  int (*routine)();
  char *arg0, *arg1;
  {
        register qnode *qp;
        extern net_sweep();

        time += ISIS_TIME;
        if(routine == net_sweep)
        {
            /* Round up to next second */
            time += 999;
            time -= time%1000;
        }
        if(oldtid)
            for(qp = time_queue->qu_next; qp != time_queue; qp = qp->qu_next)
                if(qp->qu_timeargs[2] == (char*)oldtid)
                {
                    if(qp->qu_time <= time)
                        return(oldtid);
                    qp->qu_freeroutine = nullroutine;
                    qu_free(qp);
                    break;
                }
        for(qp = time_queue->qu_next; qp != time_queue; qp = qp->qu_next)
            if(qp->qu_time >= time)
                break;
        qp = qu_add(qp, time, (char*)0, routine);
        qp->qu_timeargs[0] = arg0;
        qp->qu_timeargs[1] = arg1;
        qp->qu_timeargs[2] = (char*)++TIMEOUT_ID;
        return (TIMEOUT_ID);
  }

tsleep(secs) 
  { 
        int t_sig();
        condition sleeping = 0; 
        timeout(secs*1000, t_sig, &sleeping, 0); 
        (void)t_wait(&sleeping, "tsleep"); 
  } 

timeout_cancel(timeout_id)
  register int   timeout_id;
  {
        register qnode *qp;

        for(qp = time_queue->qu_next; qp != time_queue; qp = qp->qu_next)
            if(qp->qu_timeargs[2] == (char*)timeout_id)
            {
                qp->qu_freeroutine = nullroutine;
                qu_free(qp);
                return(1);
            }
        return(0);
   }


panic(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, aa, ab, ac, ad, ae)
  char *fmt;
  {
        if(NSITES && statfile)
            fflush(statfile);
        if(my_site_no)
        {
            if(running)
            {
                fprintf(stderr, "%s (%d/%d): -- panic --\n", my_host, my_site_no, my_site_incarn);
                fprintf(stderr, fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, aa, ab, ac, ad, ae);
                fprintf(stderr, "\n");
            }
            print("%s (%d/%d): -- panic --\n", my_host, my_site_no, my_site_incarn);
        }
        print(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, aa, ab, ac, ad, ae);
        print("\n");
        pr_dump(DUMP_ALL, "aborting in panic...");
        if(coredump < 0 || coredump == my_site_no)
            abort();
#ifndef SUNLWP 
        exit(PANIC); 
#else 
        _exit(PANIC);
#endif 
  }

/* Compute elapsed time since startup but convert to milliseconds */
Gettime()
  {
        static PREV_TIME;
        struct timeval tp;
        register time, delta;
        gettimeofday(&tp, (struct timezone*)0);
        time = (tp.tv_sec-time_0.tv_sec)*1000 + tp.tv_usec/1000;
	PREV_TIME = ISIS_TIME;
        ISIS_TIME = time;
	delta = time-PREV_TIME;
	if(delta < 0 || delta > 180000)
            time_warp(delta);
        return(time);
  }

time_warp(delta)
  {
      register qnode *qp;
      print("Time warp!  Adjusting timers by %d.%.3d secs\n", delta/1000, delta%1000);
      for(qp = time_queue->qu_next; qp != time_queue; qp = qp->qu_next)
	  qp->qu_time += delta;
      net_timewarp(delta);
  }
