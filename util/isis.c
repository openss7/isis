/*  $RCSfile: isis.c,v $ $Revision: 2.24 $ $Date: 90/08/14 10:47:16 $  */
/*
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
 *      ISIS release V1.2, Dec. 1989
 *      Export restrictions apply
 *	Originally written by Ken Birman
 */
/*
 *      Restarts ISIS at a site
 */

#include "isis.h"

# include <sys/ioctl.h>
# include <sys/socket.h>
#if(HPUX)
# include <time.h>
#else
# include <sys/time.h>
#endif
# include <sys/wait.h>
# include <signal.h>
# include <ctype.h>
# include <netdb.h>
# include <errno.h>
# include <stdio.h>
# include <fcntl.h>
# include <signal.h>

#define NCHILD          10
#define NRETRY          2
#define GETHOSTTIMEOUT  5

#define SIGINHIBIT      SIGEMT

static RESTART_PORT, CLIENT_PORT;
struct timeval broad_time = { 3, 0 };
struct timeval recov_time = { 30, 0 };
struct timeval poll = { 0, 0 };

static query_sock, restartmode = -1, isis_is_up, my_index, zap_flag;
static site_id coord;
static char *rname = "isis.rc", *sname = "sites";
static char cnames[NCHILD][20], his_name[64], my_full_name[64];
static rports[MAX_SITES];
static cid[NCHILD], nchild;
static cno[NCHILD], pno[NCHILD], bno[NCHILD], sid[NCHILD], *sp = sid, ninstances, recov_stage;
static char progs[NCHILD][100];
static condition got_query;
static saddr sin, dst;
static inhibit, maxviewid;
static h_flag = FALSE;
static bool auto_restart = FALSE;
static int auto_restart_minutes = 5;
static long restart_times[5] = { 0L, 0L, 0L, 0L, 0L };

static int restart_x = 0;		/* Index of most recent restart time. */
static struct timeval when;
extern message *isis_rcvmsg();
struct hostent *get_host_with_timeout();

inhib()
{
	inhibit ^= 1;
}

FILE *rfile;

#ifdef  FORK_BROKEN

int reply_pipes[2], exec_pipes[2];
void isis_exec();

#endif

char *isis_prog;
char **isis_args;

main(argc, argv)
	char **argv;
{
	int fd_restart(), bc_query(), reaper();
	extern isis_socket;
	FILE *sfile, *vfile;
	register i, c;
	int nsites = 0;
	struct hostent *my_hep;

	isis_prog = argv[0];
	isis_args = argv;
#	if(FORK_BROKEN)
	{
		/* Apollo 10.1 problem: FORK and VFORK broken once ISIS starts tasks up */
		pipe(exec_pipes);
		pipe(reply_pipes);
		if (fork() == 0) {
			close(reply_pipes[0]);
			close(exec_pipes[1]);
			isis_forker(exec_pipes[0], reply_pipes[1]);	/* No return */
		}
		close(reply_pipes[1]);
		close(exec_pipes[0]);
	}
#	endif
	my_process_id = ISIS;
	while (--argc) {
		char *arg = *++argv;

		c = *arg;
		if (c == '-')
			c = *++arg;
		switch (c) {
		default:
			panic
			    ("Usage: isis [-Rrestartfile] [-Ssfile] [-Aminutes] [-H...] [-c#/#] [-t] [-csite/incarn] [port-number]");
			break;
		case 'R':
			if (*++arg == 0)
				arg = *++argv;
			rname = arg;
			break;
		case 'Z':
			++zap_flag;
			break;
		case 'S':
			if (*++arg == 0)
				arg = *++argv;
			sname = arg;
			break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			RESTART_PORT = atoi(arg);
			break;
		case 'W':
			sleep(30);
		case 'A':
			*arg = 'W';
			auto_restart = TRUE;
			if (*++arg != 0) {
				/* Restart wait time in minutes follows. */
				int min = atoi(arg);

				if (min > 0) {
					auto_restart_minutes = min;
				}
			}
			break;
		case 'H':
			h_flag = TRUE;
			my_site_no = atoi(++arg);
			break;
		case 't':
			restartmode = FD_TOTAL;
			break;
		case 'c':
			restartmode = FD_PARTIAL;
			begin {
				register char *s = arg;

				coord = atoi(++s);
				s = arg;
				while (*s && *s++ != '/')
					continue;
				coord = (coord << 8) | atoi(s);
			}
		}
	}
	gethostname(my_host, 64);
	my_hep = get_host_with_timeout(my_host, GETHOSTTIMEOUT * 2);
	/* Wait a while when finding my host name. */
	if (my_hep == 0) {
		print("gethostbyname(\"%s\") failed, unable to get full hostname.\n", my_host);
		print("Continuing using just the shortname \"%s\" .\n", my_host);
		my_full_name[0] = 0;
	} else {
		register char *s;

		s = my_hep->h_name;
		while (*s && *s != '.')
			++s;
		if (*s) {
			strncpy(my_full_name, my_hep->h_name, 64);
			my_full_name[63] = 0;
		} else {
			print("WARNING: /etc/hosts doesn't list full hostname.\n");
			print("Continuing using just the shortname \"%s\" .\n", my_host);
			my_full_name[0] = 0;
		}
	}

	/* Pre-scan the site-id file */
	begin {
		char str[64];
		int port, cport, rport, sno;

		if ((sfile = fopen(sname, "r")) == NULL)
			panic("sitefile %s: Cannot read", sname);
		forever {
			register isup = fgetc(sfile), c;
			static new_format = 0;

			if (fscanf(sfile, "%d:%d,%d,%d %s", &sno, &port, &cport, &rport, str) != 5)
				break;
			while ((c = fgetc(sfile)) > 0 && c != '\n')
				if (isalpha(c))
					++new_format;
			if (isup && !new_format) {
				++new_format;
				print
				    ("Warning: old format sites file (no scope info).  ISIS may run inefficiently\r\n");
			}
			if (!port || !cport || !rport) {
				print("** Sites file contains <0> for a port number.\r\n");
				print("** This is no longer supported\r\n");
				panic("ISIS startup failed");
			}
			if (sno < 0 || sno >= MAX_SITES)
				print("Site number %d out of bounds, ignored!\r\n", sno);
			if (isup == '+') {
				register n;
				bool found;
				char *slash;
				char *strchr();

				if (sno >= nsites)
					nsites = sno + 1;
				bcopy(str, site_names[sno], 64);

				if ((slash = strchr(str, '/')) != NULL) {	/* Truncate
										   sitename at "/"
										   for following
										   comparison. */
					/* Following the slash should be an instance number */
					*slash = 0;
				}
				if (my_full_name[0] == 0) {
					/* No full name. For compatibility we'll compare the
					   shortname with a prefix of the components in the
					   hostentry. */
					int len = strlen(my_host);
					int len1 = strlen(str);

					if (len1 > len && str[len] == '.') {	/* Truncate name
										   from file only
										   at a dot. */
						str[len] = 0;
					}
					found = (strcmp(my_host, str) == 0);
				} else {
					char *dot = 0;
					int dot_in_str = (strchr(str, '.') != NULL);
					int dot_in_name = (strchr(my_full_name, '.') != NULL);

					if (dot_in_str != dot_in_name) {
						if ((dot = strchr(str, '.')) == 0)
							dot = strchr(my_full_name, '.');
						*dot = 0;
					}
					found = (strcmp(my_full_name, str) == 0);
					if (dot)
						*dot = '.';
				}
				if (found) {
					register n;

					cno[ninstances] = cport;
					pno[ninstances] = port;
					bno[ninstances] = rport;
					for (n = 0; n < ninstances; n++) {
						if (cno[ninstances] == cno[n] ||
						    pno[ninstances] == pno[n] ||
						    bno[ninstances] == bno[n])
							panic
							    ("Two instances at same site use same port no.\r\n");
					}
					++ninstances;
					*sp++ = sno;
				}
			}
		}
		fclose(sfile);
	}
	if (coord > 0 && *site_names[SITE_NO(coord)] == 0)
		panic("fd_restart: impossible coordinator site-id %x", coord);
	if (ninstances == 0)
		panic("%s: not listed in %s\r\n", my_host, sname);
	if (zap_flag)
		ninstances = 1;
	if (!h_flag) {
		/* Fork off all sub-instances */
		for (i = ninstances - 1; i; i--)
			if (fork() == 0)
				break;
		if (i) {
			sleep(20);
			restartmode = FD_PARTIAL;
			coord = MAKE_SITE_ID(sid[0], 0);
		}
		my_index = i;
		my_site_no = sid[i];
	} else {
		/* Fork off specified sub-instance */
		for (i = ninstances - 1; i >= 0; i--)
			if (sid[i] == my_site_no)
				break;
		if (i < 0)
			panic("-H%d: not listed in sites table\r\n", my_site_no);
		my_index = i;
	}
	begin {
		char str[64];
		int port, cport, rport, sid;

		if ((sfile = fopen(sname, "r")) == NULL)
			panic("sitefile %s: Cannot read", sname);
		for (i = 0; i < MAX_SITES; i++) {
			register c;

			(void) fgetc(sfile);
			if (fscanf(sfile, "%d:%d,%d,%d %s", &sid, &port, &cport, &rport, str) != 5)
				break;
			while ((c = fgetc(sfile)) > 0 && c != '\n')
				continue;
			strcpy(site_names[sid], str);
			rports[sid] = rport;
			if (sid == my_site_no) {
				CLIENT_PORT = cport;
				if (!RESTART_PORT)
					RESTART_PORT = rport;
			}
		}
		fclose(sfile);
	}

	if (RESTART_PORT == 0) {
		register struct servent *sp = getservbyname("isis", "bcast");

		if (sp == (struct servent *) 0)
			panic("isis.*: service not listed in /etc/services on this host");
		RESTART_PORT = ntohs(sp->s_port);
	}
	query_sock = socket(AF_INET, SOCK_DGRAM, 0);
	sin.sin_family = AF_INET;
	if (zap_flag) {
		static port_no = 1900;

		sin.sin_port = htons(port_no);
		while (bind(query_sock, (struct sockaddr *) &sin, sizeof(sin)) == -1) {
			if (errno != EADDRINUSE)
				panic("Can't allocate UDP port!");
			++port_no;
			sin.sin_port = htons(port_no);
		}
	} else {
		if (ninstances == 1)
			sin.sin_port = htons(RESTART_PORT);
		else
			sin.sin_port = htons(bno[my_index]);
		if (bind(query_sock, (struct sockaddr *) &sin, sizeof(sin)) == -1) {
			print("Can't allocate UDP port %d!  (Is ISIS already running?)\r\n",
			      ntohs(sin.sin_port));
			exit(0);
		}
	}
	set_isis_sockopts(query_sock);
#       ifdef UNIX_DOM
	{
		char pname[64];

		sprintf(pname, "/tmp/Is%d", CLIENT_PORT);
		unlink(pname);
	}
#       endif

	if (auto_restart) {
		/* Record time of initial startup. */
		gettimeofday(&when, (struct timezone *) 0);
		restart_times[0] = when.tv_sec;
	}

	forever			/* Outer loop: executed once for each auto restart. */
	{
		int inflag = 0, tcount = 0;
		register p;
		saddr who;

		if (restartmode == -1) {	/* Query all other sites to find if ISIS is
						   running. This part is skipped if the -c<site-no> 
						   or -t option is used to force a partial or total 
						   restart (respectively). */

			restartmode = FD_TOTAL;
			if (zap_flag == 0)
				print("Site %d (%s): isis is restarting...\r\n",
				      my_site_no, site_names[my_site_no]);
			else
				print("Site %d (%s): isis is zapping everyone else...\r\n",
				      my_site_no, site_names[my_site_no]);

			recov_stage = -2;
		      again:
			maxviewid = -2;
			if (recov_stage++ == 0)
				recov_stage = 0;
			begin {
				register s;
				register message *mp = msg_newmsg();

				if (zap_flag == 0) {
					print("Is anyone there?\r\n");
					msg_put(mp, "%d,%d,%s,%d,%h", ISIS_M_QUERY, my_site_no,
						site_names[my_site_no], recov_stage, -1);
				} else
					msg_put(mp, "%d,%s", ISIS_M_KILL, site_names[my_site_no]);
				for (s = 1; s < nsites && maxviewid <= 0; s++) {
					register struct hostent *hep;
					register char *sp;
					char name[64];

					if (s == my_site_no && zap_flag == 0)
						continue;
					bcopy(site_names[s], name, 64);
					sp = name;
					while (*sp && *sp != '/')
						++sp;
					*sp = 0;
					if (strlen(name) == 0) {
						continue;	/* Don't bother looking up null
								   names. */
					}
					hep = get_host_with_timeout(name, GETHOSTTIMEOUT);
					if (hep == 0) {
						print("Ignoring site %s\n", name);
						continue;
					}
					dst.sin_family = AF_INET;
					bcopy(hep->h_addr, &dst.sin_addr, hep->h_length);
					if (rports[s])
						dst.sin_port = htons(rports[s]);
					else
						dst.sin_port = htons(RESTART_PORT);
					isis_sendmsg(query_sock, &dst, sizeof(dst), mp);
					inflag = 1 << query_sock;
					if (select(32, &inflag, (int *) 0, (int *) 0, &poll) <= 0)
						inflag = 0;
					if (inflag)
						bc_query();
				}
				if (zap_flag)
					exit(0);
			}
			do {
				inflag = 1 << query_sock;
				if (select(32, &inflag, (int *) 0, (int *) 0, &broad_time) <= 0)
					inflag = 0;
				if (inflag)
					bc_query();
			}
			while (inflag);
			switch (maxviewid) {
			case -2:
				if (++tcount < NRETRY) {
					print
					    ("... found no operational sites, checking again just in case\r\n");
					goto again;
				} else
					print("site %d (%s) doing a total restart\r\n", my_site_no,
					      site_names[my_site_no]);
				break;

			case -1:
			case 0:
				print("A total restart is underway at site %s... waiting!\r\n",
				      his_name);
				sleep(20);
				do {
					inflag = 1 << query_sock;
					if (select(32, &inflag, (int *) 0, (int *) 0, &broad_time)
					    <= 0)
						inflag = 0;
					if (inflag)
						bc_drain();
				}
				while (inflag);
				goto again;

			default:
				restartmode = FD_PARTIAL;
				print("site %d (%s) doing a partial restart, coord is %d/%d\r\n",
				      my_site_no, site_names[my_site_no], SITE_NO(coord),
				      SITE_INCARN(coord));
				break;
			}
		}

		/* Now start PROTOCOLS, REXEC, RMGR, etc */
	      skip:
		if ((rfile = fopen(rname, "r")) == NULL) {
			if ((rfile = fopen("restartfile", "r")) == NULL)
				panic("restartfile %s: Cannot read", rname);
			print("Warning: should rename `restartfile' as `%s'\r\n", rname);
		}
		begin {
			char cpname[20], line[200], *args[20];
			register char *sp = line, **ap = args;

			*ap++ = sp;
			c = NEXTC(rfile);
			if (c == '\n' || c <= 0)
				panic("isis.rc formatting error on line 1");
			forever {
				if (c == '\n')
					putchar('\r');
				putchar(c);
				switch (c) {
				case ';':
					while ((c = NEXTC(rfile)) && c != '\n')
						putchar(c);
					putchar(c);
					if (c != '\n')
						panic("isis.rc missing newline");
					break;

				case ' ':
				case '\t':
					if (sp != line && sp[-1]) {
						*sp++ = 0;
						*ap++ = sp;
					}
					break;
				default:
					*sp++ = c;
					break;
				}
				if (((c = NEXTC(rfile)) <= 0 || c == '\n')
				    && (c <= 0 || sp != line))
					break;
			}
			isis_socket = 0;
			if (ninstances > 1) {
				sprintf(cpname, "-H%d", my_site_no);
				*ap++ = cpname;
				print(" %s", cpname);
			}
			putchar(c);
			*sp = 0;
			if (ap < &args[2])
				panic("isis.rc formatting error on line 1");
			*ap = 0;
			strcpy(progs[nchild], args[0]);
			cid[nchild] =
			    isis_fork_execve(args[0], &args[1], (char **) 0, fileno(rfile),
					     query_sock, -1);
			++nchild;
		}
		if (cid[0] <= 0)
			panic("ISIS -- exec failed!");
		sleep(5);
		while (isis_init(CLIENT_PORT) == -1) {
			static count;

			print("... still waiting for protos startup, please be patient\r\n");
			sleep(15);
			if (++count == 4) {
				print
				    ("isis: unable to restart <protos> at this site (check %d.log or for core image)\r\n",
				     my_site_no);
				exit(0);
			}
		}
		isis_task((vfunc *) fd_restart, "fd_restart");
		isis_task((vfunc *) bc_query, "bc_query");
		isis_input(query_sock, (vfunc *) bc_query, "bc_query");
		isis_chwait((vfunc *) reaper, 0);
#	    if(FORK_BROKEN)
		isis_entry(ISIS_EXEC, isis_exec, "isis_exec");
#	    endif
		isis_mainloop((vfunc *) fd_restart, NULLARG);

		/* We get here on auto-restarts. Check the last 5 restarts have happened in the
		   last 24 hours. */
		restart_x = (restart_x + 1) % 5;
		gettimeofday(&when, (struct timezone *) 0);
		if (when.tv_sec - restart_times[restart_x] < 24 * 60 * 60)
			panic("More than 5 crash/restart cycles in 24 hours\r\n");

		restart_times[restart_x] = when.tv_sec;
		print("Sleeping %d minute(s) before ISIS restart...\r\n", auto_restart_minutes);
#           if (THREADS)
		{
			sleep(auto_restart_minutes * 60);
			print("   ...Restarting ISIS\r\n");
		}
#           else       THREADS
		{
			/* Problems shutting down the ISIS tasking system necessitate this hack */
#               undef sleep
			close(query_sock);
			sleep(auto_restart_minutes * 60);
			print("   ...Restarting ISIS\r\n");
			execv(isis_prog, isis_args);
			panic("<%s>: unable to execv\r", isis_prog);
		}
#           endif
	}
}

reaper()
{
	register n, pid;

	pid = wait3(0, WNOHANG, 0);
	if (pid <= 0)
		return;
	for (n = 0; n < NCHILD; n++)
		if (cid[n] == pid) {
			print("isis: detected termination of <%s>\r\n", progs[n]);
			cid[n] = 0;
			return;
		}
	print("isis: detected termination of spawned process %d\r\n", pid);
}

fd_restart()
{
	register message *mp;

	if (restartmode == FD_PARTIAL && SITE_NO(coord) == my_site_no)
		panic("isis restart too soon after failure (wait a minute and then retry)!");
	isis_start_done();
	isis_ctp->task_act = 0;
	sv_init();
	begin {
		register c;

		nchild = 1;
		while ((c = NEXTC(rfile)) > 0) {
			char cpname[20], line[200], *args[20];
			register char *sp = line, **ap = args;

			if (c == '\n')
				continue;
			*ap++ = sp;
			forever {
				putchar(c);
				switch (c) {
				case ' ':
				case '\t':
					if (sp != line && sp[-1]) {
						*sp++ = 0;
						*ap++ = sp;
					}
					break;
				default:
					*sp++ = c;
					break;
				}
				if ((c = NEXTC(rfile)) <= 0 || c == '\n')
					break;
			}
			if (sp == line || line[0] == ';') {
				putchar('\n');
				continue;
			}
			if (CLIENT_PORT) {
				/* Pass port number to override default, if specified */
				sprintf(cpname, "%d", CLIENT_PORT);
				*ap++ = cpname;
				print(" %s", cpname);
			}
			putchar(c);
			*sp = 0;
			if (ap == args)
				continue;
			if (ap == &args[1])
				*ap++ = args[0];
			*ap = 0;
			if (nchild == NCHILD)
				--nchild;
			strcpy(progs[nchild], args[0]);
			cid[nchild] =
			    isis_fork_execve(args[0], &args[1], (char **) 0, fileno(rfile),
					     query_sock, -1);
			nchild++;
		}
		fclose(rfile);
	}
	mp = msg_genmsg(CL_RESTARTMODE, (char *) &restartmode, FTYPE_LONG, sizeof(int), 0);
	msg_insertfield(mp, CL_COORD, (char *) &coord, FTYPE_SITEID, sizeof(site_id));
	if (isis(CL_FDRESTART, mp, (char *) &isis_sv, sizeof(sview)) != sizeof(sview))
		panic("fd_restart: no result returned");
	isis_svmutex = isis_sv;
	my_site_incarn = isis_sv.sv_incarn[my_site_no];
	my_address = ADDRESS(my_site_no, my_site_incarn, my_process_id, 0);
	my_address.addr_portno = my_port_no;
	msg_delete(mp);
	if (isis_sv.sv_viewid == 0)
		panic
		    ("Site restart too soon after failure, please wait a few minutes and try again");
	isis_is_up = 1;
	print("Site %d/%d is up!\r\n", my_site_no, my_site_incarn);
	print("site view has viewid %d/%d\r\n", isis_sv.sv_viewid & 0xFF, isis_sv.sv_viewid >> 8);
	begin {
		register site_id *s;

		for (s = isis_sv.sv_slist; *s; s++)
			print("    %-30s[site_no %d  site_incarn %d]\r\n", site_names[SITE_NO(*s)],
			      SITE_NO(*s), SITE_INCARN(*s));
	}
}

bc_drain()
{
	register message *mp;
	saddr who;
	int len;

	if (mp = isis_rcvmsg(query_sock, &who, &len))
		msg_delete(mp);
}

typedef struct remote_proc remote_proc;

struct remote_proc {
	char his_host_name[64];
	int his_pid;
	remote_proc *rp_link;
};

struct adesc rp_adesc = { sizeof(remote_proc), 0, 8 };

#define rp_alloc()	((remote_proc*)mallocate(&rp_adesc))

remote_proc *rp_root;

static
is_alive(his_host_name, his_pid)
	register char *his_host_name;
	register his_pid;
{
	register remote_proc *rp;

	for (rp = rp_root; rp; rp = rp->rp_link)
		if (strcmp(rp->his_host_name, his_host_name) == 0 && rp->his_pid == his_pid)
			return (1);
	rp = rp_alloc();
	rp->rp_link = rp_root;
	rp_root = rp;
	rp->his_pid = his_pid;
	strcpy(rp->his_host_name, his_host_name);
	return (0);
}

bc_query()
{
	register message *mp, *rmsg;
	sview *site_getview();
	register sview *sv;
	char version[10];
	saddr who;
	int type, sender, hisviewid;
	int len;
	site_id hiscoord;

	if ((mp = isis_rcvmsg(query_sock, &who, &len)) == 0)
		return;
	msg_get(mp, "%d", &type);
	switch (type) {
		char his_host_name[128];
		int n, his_pid;
		static pids;
		address his_address;
		saddr raddr;
		extern condition isis_want_incarn;

	case ISIS_M_KILL:
		msg_get(mp, "%s", his_name);
		term("Zapped by site %s\r\n", his_name);
		return;

	case ISIS_M_REMOTE:
		while (my_site_incarn == RECOVERY_INCARN)
			(void) t_wait(&isis_want_incarn);
		his_address = my_address;
		his_address.addr_process = ++pids | PID_REMOTE;
		his_address.addr_portno = ntohs(who.sin_port);
		msg_get(mp, "%s,%d", his_host_name, &his_pid);
		msg_delete(mp);
		if (is_alive(his_host_name, his_pid) == 0) {
			intercl_isalive((address *) 0, &his_address, &who);
			rmsg = msg_gen("%d,%A[1]%A[1]", ISIS_M_ACK, &my_address, &his_address);
			mother_add_fields(rmsg);
			isis_sendmsg(query_sock, &who, len, rmsg);
			msg_delete(rmsg);
		}
		return;

	case ISIS_M_LOOKUP:
		if (msg_get(mp, "%A[1]", &his_address) == 0
		    || (n = intercl_lookup(&his_address, &raddr)) == 0)
			bzero(&raddr, n = sizeof(raddr));
		msg_delete(mp);
		rmsg = msg_gen("%C", (char *) &raddr, n);
		isis_sendmsg(query_sock, &who, len, rmsg);
		msg_delete(rmsg);
		return;
	}
	msg_get(mp, "%d,%s,%d,%h", &sender, his_name, &hisviewid, &hiscoord);
	msg_delete(mp);
	if (sender == my_site_no)
		return;
	if (type == ISIS_M_REPLY && isis_is_up)
		return;
	if (strcmp(site_names[sender], his_name)) {
		if (type == ISIS_M_REPLY)
			panic("someone is running isis at site %s using a different site-table!",
			      his_name);
		print
		    ("Warning: someone is trying to run isis at site %s using a different site-table!\r\n",
		     his_name);
		return;
	}
	if (hisviewid > maxviewid && hiscoord != 0) {
		if (hisviewid > 0 || hisviewid > recov_stage) {
			maxviewid = hisviewid;
			coord = hiscoord;
		} else if (hisviewid == recov_stage && strcmp(his_name, my_host) > 0) {
			maxviewid = hisviewid;
			coord = hiscoord;
		}
		if (coord > 0 && *site_names[SITE_NO(coord)] == 0)
			panic("Impossible coordinator site-id %d", SITE_NO(coord));
		if (coord == my_site_no)
			panic
			    ("Told to use myself as coordinator (wait a few minutes and then try again)");
	}
	if (type == ISIS_M_REPLY)
		return;
	rmsg = msg_gen("%d,%d,%s", ISIS_M_REPLY, my_site_no, site_names[my_site_no]);
	if (!isis_is_up)
		msg_put(rmsg, "%d,%h", recov_stage, -1);
	else {
		sv = site_getview();
		msg_put(rmsg, "%d,%h", sv->sv_viewid, sv->sv_slist[0]);
	}
	isis_sendmsg(query_sock, &who, len, rmsg);
	msg_delete(rmsg);
}

int
isis_failed()
  /* This is called by clib when ISIS crashes abnormally at this site (i.e. when protos crashes,
     but not when a graceful shutdown is done). We should return zero to indicate that we wish to
     restart (by returning from the isis_mainloop call), and non-zero if we wish to exit. */
{
	register who, n;

	if (!isis_is_up)
		return (1);
#       ifdef UNIX_DOM
	{
		/* Get rid of all the old port names, if any */
		char pname[64];

		sprintf(pname, "/tmp/Is%d", CLIENT_PORT);
		unlink(pname);
		sprintf(pname, "/tmp/Cl%d", my_process_id);
		unlink(pname);
	}
#       endif
	print("\nISIS has crashed at site %s...\r\n", site_names[my_site_no]);
	for (n = 0; n < nchild; n++)
		if (cid[n])
			kill(cid[n], SIGKILL);
	isis_is_up = 0;
	t_sig(&got_query, (void *) 1);
	while ((who = wait(0)) != -1)
		continue;
	if (!auto_restart)
		exit(0);

	nchild = 0;
	return (0);		/* Return from isis_mainloop and try to restart. */
}

term(msg, site)
	char *msg;
{
	print(msg, site);
	kill(0, SIGTERM);
	exit(0);
}

static peekc;

NEXTC(fi)
	register FILE *fi;
{
	register c, white = 0;

	if (c = peekc) {
		peekc = 0;
		return (c);
	}
	do {
		c = fgetc(fi);
		if (c == ' ' || c == '\t')
			++white;
	}
	while (c == ' ' || c == '\t');
	if (white) {
		if (c == '\n')
			return (c);
		peekc = c;
		return (' ');
	}
	return (c);
}

jmp_buf alarm_env;

void
get_host_alarm()
{
	longjmp(alarm_env, 1);
}

struct hostent *
get_host_with_timeout(name, seconds)
	char *name;
	int seconds;

  /* Does a gethostbyname call with a timeout. If gethostbyname doesn't return within "seconds"
     seconds, NULL is returned. Uses alarm signals. Must not be called after the isis task
     mechanism has been initialized since that uses alarms too. */
{
	struct hostent *hep;

	signal(SIGALRM, get_host_alarm);
	alarm(seconds);
	if (setjmp(alarm_env) == 0) {
		hep = gethostbyname(name);
	} else {
		print("gethostbyname(\"%s\") timed out.\n", name);
		hep = NULL;
	}
	alarm(0);
	return (hep);
}

#ifdef  FORK_BROKEN
#include <ctype.h>
getin(in)
{
	char c = 0;

	if (read(in, &c, 1) != 1 || (!isprint(c) && c != '\n')) {
		print("ISIS forker: broken channel (%o)\r\n", (int) c);
		exit(0);
	}
	return ((int) c);
}

isis_forker(in, out)
{
	register char c, *s, **sp;
	char program[256], args[512], env[512], num[20];
	char *argp[20], *envp[20];
	int pid;

	forever {
		s = program;
		while ((*s = getin(in)) != ' ')
			++s;
		*s = 0;
		sp = argp;
		s = args;
		do {
			*sp++ = s;
			while ((c = getin(in)) != ';' && c != ' ')
				if (c <= 0)
					exit(0);
				else
					*s++ = c;
			*s++ = 0;
		}
		while (c != ';');
		if (*sp[-1] == 0)
			--sp;
		*sp = 0;
		sp = envp;
		s = env;
		do {
			*sp++ = s;
			while ((c = getin(in)) != '\n' && c != ' ')
				if (c <= 0)
					exit(0);
				else
					*s++ = c;
			*s++ = 0;
		}
		while (c != '\n');
		if (*sp[-1] == 0)
			--sp;
		*sp = 0;
		if ((pid = fork()) == 0) {
			close(in);
			close(out);
			if (*env)
				execve(program, argp, envp);
			else
				execvp(program, argp, envp);
			perror("execve");
			fprintf(stderr, "ISIS: can't exec <%s>\r\n", program);
			exit(0);
		}
		sprintf(num, "%d\r\n", pid);
		write(out, num, strlen(num));
	}
}

/* Gets string from cl_isis.c: isis_fork_execve */
void
isis_exec(mp)
	message *mp;
{
	char *str;
	int pid;

	msg_get(mp, "%-s", &str);
	if (msg_getsender(mp)->process < 0)
		pid = ISIS_fork_execve(str);
	else
		pid = -1;
	reply(mp, "%d", pid);
	return;
}

int
ISIS_fork_execve(str)
	char *str;
{
	char num[32];
	register char *s;

	write(exec_pipes[1], str, strlen(str));
	s = num;
	while (read(reply_pipes[0], s, 1) == 1)
		if (*s++ == '\n') {
			*--s = 0;
			return (atoi(num));
		}
	panic("ISIS: forker process failed");
	return (-1);
}

/* Dummy, needed for linking */
notify_set_itimer_func(client, infunc, which, value, ovalue)
	ifunc *client, *infunc;
	int which;
	struct itimerval *value, *ovalue;
{
}
#endif				/* FORK_BROKEN */
