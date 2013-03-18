/*  $RCSfile: cl_isis.c,v $ $Revision: 2.119 $ $Date: 90/09/17 14:41:33 $  */
/*
 *	Originally coded by Ken Birman
 *      Sleep reverts to its normal meaning in this module!
 *      Basic client->isis interface
 *
 *      ISIS release V2.0, May 1990
 *      Export restrictions apply
 *
 *      The contents of this file are subject to a joint, non-exclusive
 *      copyright by members of the ISIS Project.  Permission is granted for
 *      use of this material in unmodified form in commerCial or research
 *      settings.  Creation of derivative forms of this software may be
 *      subject to restriction; obtain written permission from the ISIS Project
 *      in the event of questions or for special situations.
 *      -- Copyright (c) 1990, The ISIS PROJECT
 */

#define GENERIC_ENTRIES
#define ISIS_ENTRY_NAMES
#define ISIS_ERRORS
#define ISIS_SYS

# include "isis.h"
# include "spooler.h"
/* Only need spooler.h for definition of sp_init. */

qnode *ADDRS;				/* Storage for addresses */
qnode *GNAMES;				/* Storage for addresses */
int act_blocked;			/* When tasks are blocked, this says who did it */
int bypass_lagging;			/* Internal view lagging behind bypass view */
qnode *bypass_waitq;			/* Waiting for a join to finish */
int intercl_socket;			/* Socket (fdes) for receiving directly client-client */
int isis_created;			/* Tasks created */
char *isis_dir;				/* isis working directory */
int isis_enum;				/* Used when isis_gotmsg() is delivery message */
ginfo *isis_gip;			/* " " */
event_id *isis_eid;			/* " " */
int isis_errno;				/* See pr_errors.h */
int isis_forkcnt;			/* Counts tasks forked off */
ginfo *isis_groups;
char *isis_joining;			/* Group this client is joining, if any */
int isis_dontlogdumps;			/* inhibits dumping, mostly for cmd.c */
int isis_never;
int isis_nreplies;			/* number of replies received */
int isis_nsent;				/* number of destinations sent to */
qnode *isis_pgmon;			/* Pgroup monitor list */
qnode *isis_drainq;			/* on-drain callbacks */
qnode *isis_wlist;			/* Process group member watch list */
qnode *isis_swlist;			/* Site watch list */
qnode *isis_pwlist;			/* Process watch list */
qnode *mother_msg_queue;		/* Messages from mom */
message *mother_rep_msg;		/* From initial connect to mom */
int isis_socket;			/* Socket (fdes) for talking to isis */
int isis_state;				/* State flags */
int isis_ncongest;
int isis_switched;			/* Task switches done */
int isis_msgs_enqueued;
int my_site_no;				/* Info about this site */
int isis_wakeup_sync;			/* Wakeup anyone on ISIS_BLOCK */
int my_site_incarn;			/* Info about this site */
site_id my_site_id;			/* This process's id */
int my_process_id;			/* Info about this site */
int my_port_no;				/* UDP port to talk to me directly */
int my_genid;				/* Used to generate bcast id's */
int isis_mask_bits = 32;		/* Number of bits used in mask bitvec */
int ISIS_PORTNO;			/* For UDP-rpc to bin/isis */
static isis_events;			/* Used to avoid looping too many times */
saddr mother_addr;			/* For tk_remote */
address my_mother;			/* For tk_remote */
static address via_mom[2];		/* For dests in setdests */
static isis_sigcount, sig_count[128];	/* Total and per-signal count of signals received */
static vfunc *cl_entries[MAXENTRIES];	/* isis_entry routines */
static vfunc *cl_routines[MAXENTRIES];	/* isis_task declarations */
static char *cl_rnames[MAXENTRIES];	/* corresponding names */
static condition cl_mwant[MAXENTRIES];	/* tasks waiting in msg_rcv */
static qnode *cl_queues[MAXENTRIES];	/* Message queues for MSG_ENQUEUE */
bitvec cl_xbyref;			/* Fortran callbacks get args by ref */
condition isis_want_incarn;		/* Wants incarnation number */
bitvec Bz;				/* Zeros */
bitvec my_bcastscope;			/* Used in bcast.c */
static bitvec BVEC;			/* Used for temp. purposes only */
sview isis_sv, isis_svmutex;		/* Basic site view */
condition isis_wantleave;
static site_len;			/* Length of site_names in bytes */

#ifdef UNIX_DOM
#include <sys/un.h>

static struct sockaddr_un ux_addr, ux_iaddr;
#endif

int nalloc, nfree, memused, memfree, memalloc;
long msg_namsgs, msg_nfmsgs;
int msg_usagestats, msg_tracecaller, msg_tracemsgs;

typedef struct iw_desc iw_desc;

struct iw_desc {
	iw_desc *iw_next;
	int iw_id;
	int iw_flag;
	int iw_act;
	int iw_signo;
	bitvec *iw_imask;
	bitvec *iw_omask;
	bitvec *iw_xmask;
	bitvec iw_ibits;
	bitvec iw_obits;
	bitvec iw_xbits;
	vfunc *iw_routine;
	union {
		VOID *iwf_arg0;
		struct {		/* if routine is t_sig */
			condition *iwc_cond;
			char *iwc_arg0;
		} iw_s;
	} iw_un;
};

#define iw_arg0         iw_un.iwf_arg0
#define iw_cond         iw_un.iw_s.iwc_cond
#define iw_carg         iw_un.iw_s.iwc_arg0

adesc iw_ad = { sizeof(iw_desc), 0, 8 };

#define iw_alloc()      ((iw_desc*)mallocate(&iw_ad))
#define iw_free(iw)     mdeallocate((VOID*)iw, &iw_ad)

static iw_desc *iw_root;
static IW_ID;
static bitvec ISIS_IMASK, ISIS_OMASK, ISIS_XMASK;
static bitvec *isis_imask, *isis_omask, *isis_xmask;
static bitvec cur_imask, cur_omask, cur_xmask;
static bitvec sel_imask, sel_omask, sel_xmask;
static bitvec want_sig;
static condition accept_events;
static async_accept_count;
static struct timeval sel_poll = { 0, 0 };

#define IW_HASIMASK     0x0001
#define IW_HASOMASK     0x0002
#define IW_HASXMASK     0x0004

struct servent *getservbyname();
message *msg_read();

saddr sys_addr, my_addr;
static vfunc *msg_filter = (vfunc *) cl_local_delivery;
static qnode *msg_queue, *msg_mutexqueue, *time_queue, *protos_spool;
static qnode *work_queue;
static msg_count;			/* Totaled length of message queues */
static void isis_setmax();
static int isis_runtasks();
int ISIS_TIME;
static struct timeval time_0;
struct timeval last_gettimeofday;

/* Forward declarations. */
#if FUN_TYPES
int isis_fork_execve(char *program, char **argp, char **envp, int fd0, int fd1, int df2);
#else
task *isis_fork();
#endif

#define ISIS_MAXEVENTS	50

int
find_act(a)
	address *a;

  /* Inverse of map_act. */
{
	register act;

	for (act = 0; act != MAX_ACT; act++) {
		if (ACT_VALID(act) && act_isequal(&act_map[act].act_id, a))
			return (act);
	}
	return (-1);
}

int act_bits = 1;			/* Actually in use */

void
isis_overflow()
{
	panic("protos ->client channel backlog overflow!");
}

static isis_at;

static void
isis_accept_timeout()
{
	isis_at = 0;
}

void
do_cl_dump()
{
	if ((my_process_id != ISIS && my_process_id < 0 && !(my_process_id & (short) ~PID_REMOTE))
	    || isis_dontlogdumps)
		return;
	print("Received signal CLDUMP.  Spooling dump into %d.log\n", getpid());
	isis_logging(1);
	cl_dump(DUMP_ALL, "received signal CLDUMP");
	isis_logging(0);
}

static sig_mask = -1;

static cinfo ci;

/* Initialize to use ISIS facilities */
int
isis_init_l(CLIENT_PORT, flag)
	int CLIENT_PORT, flag;
{
	int retries = 0;

	static first_time = 0;

	if (isis_state & ISIS_INIT)
		return (0);
	isis_state |= ISIS_INIT | ISIS_STARTUP | (flag & ISIS_REMOTE);
	signal(SIG_DUMP, do_cl_dump);
	isis_state |= ISIS_STARTUP;
	isis_forkcnt = 0;
	isis_ntasks = 0;
	NULLADDRESS.addr_site = 0;
	signal(SIGUSR1, isis_overflow);
      again:if (first_time++ == 0) {
		if (my_process_id == 0)
			my_process_id = getpid();
		gethostname(my_host, 64);
		gettimeofday(&time_0, (struct timezone *) 0);
		last_gettimeofday = time_0;
		my_addr.sin_family = AF_INET;
		if (my_process_id == 0)
			my_process_id = getpid();
		if ((isis_state & ISIS_REMOTE) == 0) {
			saddr true_name;
			static port_no = 2000;
			int size = sizeof(true_name);

			if ((intercl_socket = socket(AF_INET, SOCK_DGRAM, 0)) == -1) {
				perror("socket");
				isis_state &= ~(ISIS_INIT | ISIS_STARTUP);
				goto error;
			}
			/* 
			 * We do this instead of letting UNIX pick because some
			 * versions of UNIX pick numbers that others consider to be
			 * out of range.
			 */
			++port_no;
			my_addr.sin_port = htons(port_no);
			while (bind(intercl_socket, (struct sockaddr *) &my_addr, sizeof(my_addr))
			       == -1) {
				if (errno != EADDRINUSE) {
					perror("bind intercl_socket");
					close(intercl_socket);
					isis_state &= ~(ISIS_INIT | ISIS_STARTUP);
					goto error;
				}
				++port_no;
				my_addr.sin_port = htons(port_no);
			}
			my_port_no = port_no;
			set_isis_sockopts(intercl_socket);

		}
		qu_freelist = qu_null();
		isis_wlist = qu_null();
		isis_pwlist = qu_null();
		isis_pgmon = qu_null();
		isis_drainq = qu_null();
		isis_tasks = qu_null();
		mother_msg_queue = qu_null();
		protos_spool = qu_null();
		msg_queue = qu_null();
		work_queue = qu_null();
		ADDRS = qu_null();
		GNAMES = qu_null();
		msg_mutexqueue = qu_null();
		time_queue = qu_null();
		isis_entry(GENERIC_RCV_REPLY, (vfunc *) cl_rcv_reply, "isis:cl_rcv_reply");
		isis_entry(GENERIC_NEW_VIEW, (vfunc *) cl_new_view, "isis:cl_new_view");
		isis_entry(GENERIC_DEL_PGROUP, (vfunc *) cl_del_pgroup, "isis:cl_del_pgroup");
		isis_task((vfunc *) act_block, "isis:act_block");
		isis_task((vfunc *) BCAST, "isis:bcast");
		isis_task((vfunc *) isis_accept_events_loop, "isis:blocking-listener");
		isis_task((vfunc *) isis_init_l, "<MAIN-TASK>");
		isis_task((vfunc *) isis_accept_timeout, "isis_accept_events:timeout");
	}
	if (flag & ISIS_REMOTE) {
		isis_state |= ISIS_REMOTE;
		isis_socket = intercl_socket;
		goto skip_connect;
	}
#ifdef  UNIX_DOM
	/* Create a UNIX domain socket to connect to ISIS on */
	if ((isis_socket = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
		perror("socket");
		isis_state &= ~(ISIS_INIT | ISIS_STARTUP);
		goto error;
	}
	ux_addr.sun_family = AF_UNIX;
	sprintf(ux_addr.sun_path, "/tmp/Cl%d", my_process_id);
	unlink(ux_addr.sun_path);
	if (bind(isis_socket, (struct sockaddr *) &ux_addr,
		 strlen(ux_addr.sun_path) + sizeof(ux_addr.sun_family)) == -1) {
		perror("bind isis_socket");
		close(isis_socket);
		isis_state &= ~(ISIS_INIT | ISIS_STARTUP);
		goto error;
	}
	if (CLIENT_PORT == 0) {
		register struct servent *sp = getservbyname("isis", "tcp");

		if (sp == (struct servent *) 0)
			panic("isis.*: service not listed in /etc/services on this host");
		CLIENT_PORT = ntohs(sp->s_port);
	}
	ux_iaddr.sun_family = AF_UNIX;
	sprintf(ux_iaddr.sun_path, "/tmp/Is%d", CLIENT_PORT);
	if (connect(isis_socket, (struct sockaddr *) &ux_iaddr,
		    strlen(ux_iaddr.sun_path) + sizeof(ux_iaddr.sun_family)) == -1) {
		unlink(ux_addr.sun_path);
		isis_state &= ~(ISIS_INIT | ISIS_STARTUP);
		if (my_process_id == ISIS)
			return (-1);
		print("isis_init: connect failed (port %d), isis is not currently running\n",
		      CLIENT_PORT);
		goto error;
	}
	unlink(ux_addr.sun_path);
#else				/* UNIX_DOM */
	/* Create a TCP socket to connect to ISIS on */
	if ((isis_socket = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("socket");
		isis_state &= ~(ISIS_INIT | ISIS_STARTUP);
		goto error;
	}
	my_addr = *get_addr_by_address((address *) 0, 0);
	if (bind(isis_socket, (struct sockaddr *) &my_addr, sizeof(my_addr)) == -1) {
		perror("bind isis_socket");
		close(isis_socket);
		isis_state &= ~(ISIS_INIT | ISIS_STARTUP);
		goto error;
	}
	begin {
		extern char *getenv();
		register char *envp = getenv("ISISPORT");

		if (envp)
			CLIENT_PORT = atoi(envp);
	}
	if (CLIENT_PORT == 0) {
		register struct servent *sp;

		sp = getservbyname("isis", "tcp");
		if (sp == (struct servent *) 0)
			panic("isis.*: service not listed in /etc/services on this host");
		CLIENT_PORT = ntohs(sp->s_port);
	}
	sys_addr = my_addr;
	sys_addr.sin_port = htons((short) CLIENT_PORT);
	if (connect(isis_socket, (struct sockaddr *) &sys_addr, sizeof(sys_addr)) == -1) {
		perror("connect");
		close(isis_socket);
		isis_state &= ~(ISIS_INIT | ISIS_STARTUP);
		exit(-1);
	}
#endif				/* UNIX_DOM */
      skip_connect:
	isis_setmax();
	t_init();
	/* Register with ISIS and get various stuff back */
	begin {
		message *msg;
		register cinfo *cip;
		register sview *sv;
		register char *sn, cip_isisdir[64];
		register longvec[5];
		int veclen;

		if (isis_state & ISIS_REMOTE)
			msg = mother_rep_msg;
		else
			msg = msg_read(isis_socket);
		if (msg == NULLMP)
			panic("*** msg_read failed in isis_init ***");

		/* cinfo struct broken out into fields for byte-swapping - tclark 7/23/90 */
		msg_get(msg, "%D, %s", longvec, &veclen, cip_isisdir);

		sv = (sview *) msg_getfield(msg, FLD_ANSW, 1, NULLIARG);
		sn = (char *) msg_getfield(msg, FLD_ANSW, 2, &site_len);

		my_site_no = longvec[0];
		my_site_incarn = longvec[1];
		my_site_id = (site_id) longvec[2];
		my_genid = longvec[3];
		ISIS_PORTNO = longvec[4];

		my_address = ADDRESS(my_site_no, my_site_incarn, my_process_id, 0);
		my_address.addr_portno = my_port_no;
		isis_sv = *sv;

		isis_dir = malloc(strlen(cip_isisdir) + 1);
		strcpy(isis_dir, cip_isisdir);

		bcopy(sn, site_names, site_len);
		msg_delete(msg);
	}
	isis_state |= ISIS_ISUP;
	ISIS_ENTER();
	isis_ctp->task_routine = (vfunc *) isis_init_l;
	isis_ctp->task_arg0 = 0;
	isis_ctp->task_addr = my_address;
	act_begin();
	act_block();
	isis_ctp->task_act = act_blocked;
	bclr(&my_bcastscope);
	coord_init();
	sv_init();
	join_init();
	w_init();
	tk_init();
	intercl_init();
	bypass_init();
	x_init();
	sp_init();
	if (isis_state & ISIS_REMOTE) {
		via_mom[0] = my_mother;
		intercl_isalive((address *) 0, &my_mother, &mother_addr);
		intercl_ismother(&my_mother);
		isis_probe(60, 30);
	}
	cl_register();
	isis_connect_init();
#ifdef LARGE_GROUPS
	if (my_process_id >= 0) {	/* Don't bother joining the large group services if we're
					   an Isis utility. */
		/* Furthermore, if we're the "isis" startup program we can't join groups or do
		   lookups at this point without deadlocking. */
		lg_init();
	}
#endif
	ISIS_EXIT();
	return (0);

      error:
	if (flag & ISIS_AUTOSTART) {
		static char *args[] = { "startisis", "/usr/bin/startisis", 0 };
		if (++retries == 1) {
			print("Auto-restart ISIS by running \"csh /usr/bin/startisis\"...\n");
			isis_fork_execve("/bin/csh", args, (char **) 0, -1, -1, -1);
			/* Give it time to restart */
			sleep(90);
			print("ISIS Auto-restart completed, re-trying connect ...\n");
			goto again;
		}
		print("ISIS Auto-restart attempt unsuccessful\n");
	}
	if (flag & ISIS_PANIC)
		exit(0);
	return (-1);
}

int
isis_init(CLIENT_PORT)
	int CLIENT_PORT;
{
	register rv;

	rv = isis_init_l(CLIENT_PORT, (my_process_id == ISIS) ? 0 : ISIS_PANIC);
	return rv;
}

mother_add_fields(mp)
	register message *mp;
{
	register longvec[5];

	longvec[0] = my_site_no;
	longvec[1] = my_site_incarn;
	longvec[2] = my_site_id;
	longvec[3] = my_genid;
	longvec[4] = ISIS_PORTNO;

	msg_put(mp, "%D, %s", longvec, 5, isis_dir);

	msg_insertref(mp, FLD_ANSW, (char *) &isis_sv, FTYPE_SVIEW, sizeof(isis_sv),
		      (vfunc *) NULLROUTINE);
	msg_insertref(mp, FLD_ANSW, (char *) site_names, FTYPE_CHAR, site_len,
		      (vfunc *) NULLROUTINE);

}

static void
isis_send_hello()
{
	register message *msg;
	address addr;

	msg = msg_newmsg();
	addr = PRO(CL_HELLO);
	isis_send(&addr, msg);
	msg_delete(msg);
}

isis_probe(freq, timeout)
{
	register message *msg;
	address addr;
	static tid;

	ISIS_ENTER();
	msg = msg_gen("%d,%d", freq, timeout);
	addr = PRO(CL_PROBE);
	isis_send(&addr, msg);
	tid = isis_timeout_reschedule(tid, freq * 1000, isis_send_hello, NULLARG, NULLARG);
	msg_delete(msg);
	ISIS_RETURN(0);
}

void
isis_start_done()
{
	if ((isis_state & ISIS_STARTUP) == 0)
		return;
	isis_state &= ~ISIS_STARTUP;
	act_restart();
	act_end(0);
	log_replay_msgs();
}

void
isis_disconnect()
{
	extern FILE *isis_outfile;

	if (intercl_socket)
		close(intercl_socket);
	if (isis_outfile)
		fclose(isis_outfile);
	intercl_socket = 0;
	isis_outfile = (FILE *) 0;
	isis_has_crashed(0);
}

struct cl_watch {
	address clw_gaddr;
	address clw_who;
	int clw_act;
};

/*
 * Special act_block support for joins 
 */
static adesc cl_wad = { sizeof(cl_watch), sizeof(cl_watch), 2 };

#define clw_alloc()     ((cl_watch*)mallocate(&cl_wad))

void
clw_free(clp)
	register cl_watch *clp;
{
	mdeallocate((char *) clp, &cl_wad);
}

int
cl_watch_for(gaddr, who)
	address *gaddr, *who;
{
	static CL_WID;
	register cl_watch *clw;
	register ginfo *gip = map_gaddr(gaddr);

	clw = clw_alloc();
	clw->clw_gaddr = *gaddr;
	clw->clw_who = *who;
	clw->clw_act = isis_ctp->task_act;
	qu_add_clw(gip->gi_clwatchq, ++CL_WID, clw, (vfunc *) clw_free);
	return (CL_WID);
}

void
cl_watch_cancel(gaddr, cl_wid)
	address *gaddr;
	int cl_wid;
{
	register ginfo *gip = map_gaddr(gaddr);
	register qnode *qp;

	if (gip && (qp = qu_find(gip->gi_clwatchq, cl_wid)))
		qu_free(qp);
}

void
dump_cl_watch_queue()
{
	register ginfo *gip;
	register qnode *qp;

	for (gip = isis_groups; gip; gip = gip->gi_next)
		for (qp = gip->gi_clwatchq->qu_next; qp != gip->gi_clwatchq; qp = qp->qu_next) {
			register cl_watch *clw = qp->qu_clw;

			print("  [CLWID %d]: Group ", qp->qu_name);
			paddr(&clw->clw_gaddr);
			print(" waiting for join of client ");
			paddr(&clw->clw_who);
			print("\n");
		}
}

static jbviewid;

/* Trigger join watches before other things get triggered */
void
check_cl_watch_queue(mp)
	register message *mp;
{
	register qnode *qp;
	register sys_groupview *npg = (sys_groupview *) msg_getfield(mp, CL_NEWVIEW, 1, (int *) 0);
	register ginfo *gip = map_gaddr(&npg->pg_gid);

	if (gip == (ginfo *) 0)
		return;
	if (gip->gi_clwatchq->qu_viewid > npg->pg_viewid) {
		panic("check_cl_watch_queue: viewid was %d.0 backed down to %d.0",
		      gip->gi_clwatchq->qu_viewid, npg->pg_viewid);
		return;
	}
	if (gip->gi_clwatchq->qu_viewid == npg->pg_viewid)
		return;
	if (gip->gi_clwatchq->qu_viewid == 0)
		gip->gi_clwatchq->qu_viewid = npg->pg_viewid;
	else if (++gip->gi_clwatchq->qu_viewid != npg->pg_viewid)
		panic("check_cl_watch_queue: expected viewid %d.0 skipped to %d.0\n",
		      gip->gi_clwatchq->qu_viewid, npg->pg_viewid);
	for (qp = gip->gi_clwatchq->qu_next; qp != gip->gi_clwatchq; qp = qp->qu_next) {
		register cl_watch *clw = qp->qu_clw;
		register address *ap;

		for (ap = npg->pg_alist; !aptr_isnull(ap); ap++)
			if (addr_isequal(&clw->clw_who, ap)) {
				int save_act = isis_ctp->task_act;

				isis_ctp->task_act = clw->clw_act;
				qu_free(qp);
				jbviewid = gip->gi_clwatchq->qu_viewid;
				t_fork_urgent((vfunc *) join_block, NULLARG);
				isis_ctp->task_act = save_act;
				return;
			}
	}
}

static void
isis_setmax()
{
	register n = 0;
	register long *ip, *op, *xp;

	if (isis_imask)
		ip = isis_imask->bv_data;
	else
		ip = Bz.bv_data;
	if (isis_omask)
		op = isis_omask->bv_data;
	else
		op = Bz.bv_data;
	if (isis_xmask)
		xp = isis_xmask->bv_data;
	else
		xp = Bz.bv_data;
	while (n < ISIS_BVL && (*ip || *op || *xp))
		++n, ++ip, ++op, ++xp;
	n <<= ISIS_BSHIFT;
	while (n < isis_socket)
		n += ISIS_BS;
	isis_mask_bits = n;
}

bitvec *
Bvec(fdes)
	register fdes;
{
	bzero(&BVEC, sizeof(BVEC));
	bis(&BVEC, fdes);
	return (&BVEC);
}

static int
Bcmp(b1, b2)
	register bitvec *b1, *b2;
{
	bitvec bcmp;

	bcmp = *b1;
	bicv(&bcmp, b2);
	if (btst(&bcmp))
		return (-1);
	bcmp = *b2;
	bicv(&bcmp, b1);
	if (btst(&bcmp))
		return (1);
	return (0);
}

static void
isis_sdispatch(sig)
	int sig;
{
	++sig_count[sig];
	++isis_sigcount;
}

int
isis_gotsig()
{
	return (isis_sigcount > 1);
}

int
isis_do_select(imask, omask, xmask, signo, routine, arg0, arg1)
	register bitvec *imask, *omask, *xmask;
	int signo;
	vfunc *routine;
	VOID *arg0, *arg1;
{
	register iw_desc *iw;

	ISIS_ENTER();
#ifdef  SIGCHLD
	if (signo == -99)
		signo = SIGCHLD;
#else
	if (signo == -99)
		signo = SIGCLD;
#endif
	if (routine == NULLROUTINE
	    || (routine == (vfunc *) t_sig && (condition *) arg0 == (condition *) 0)) {
	      loop:
		/* Cancel all that match */
		for (iw = iw_root; iw; iw = iw->iw_next) {
			if (signo && iw->iw_signo != signo)
				continue;
			if (imask) {
				if ((iw->iw_flag & IW_HASIMASK) == 0 || Bcmp(imask, iw->iw_imask))
					continue;
			} else if (iw->iw_flag & IW_HASIMASK)
				continue;
			if (omask) {
				if ((iw->iw_flag & IW_HASOMASK) == 0 || Bcmp(omask, iw->iw_omask))
					continue;
			} else if (iw->iw_flag & IW_HASOMASK)
				continue;
			if (xmask) {
				if ((iw->iw_flag & IW_HASXMASK) == 0 || Bcmp(xmask, iw->iw_xmask))
					continue;
			} else if (iw->iw_flag & IW_HASXMASK)
				continue;
			isis_wait_cancel(iw->iw_id);
			goto loop;
		}
		return (0);
	}
	iw = iw_alloc();
	iw->iw_flag = 0;
	iw->iw_next = iw_root;
	iw_root = iw;
	if ((iw->iw_signo = signo) && bit(&want_sig, signo) == 0) {
		bis(&want_sig, signo);
		signal(signo, isis_sdispatch);
	}
	if (imask && btst(imask)) {
		if (imask == &Bz || imask == &BVEC) {
			iw->iw_imask = &iw->iw_ibits;
			iw->iw_ibits = *imask;
		} else
			iw->iw_imask = imask;
		iw->iw_flag |= IW_HASIMASK;
		if (isis_imask == (bitvec *) 0)
			isis_imask = &ISIS_IMASK;
		bisv(isis_imask, imask);
	}
	if (omask && btst(omask)) {
		if (omask == &Bz || omask == &BVEC) {
			iw->iw_omask = &iw->iw_obits;
			iw->iw_obits = *omask;
		} else
			iw->iw_omask = omask;
		iw->iw_flag |= IW_HASOMASK;
		if (isis_omask == (bitvec *) 0)
			isis_omask = &ISIS_OMASK;
		bisv(isis_omask, omask);
	}
	if (xmask && btst(xmask)) {
		if (xmask == &Bz || xmask == &BVEC) {
			iw->iw_xmask = &iw->iw_xbits;
			iw->iw_xbits = *xmask;
		} else
			iw->iw_xmask = xmask;
		iw->iw_flag |= IW_HASXMASK;
		if (isis_xmask == (bitvec *) 0)
			isis_xmask = &ISIS_XMASK;
		bisv(isis_xmask, xmask);
	}
	if (imask || omask || xmask)
		isis_setmax();
	if ((iw->iw_routine = routine) != (vfunc *) t_sig)
		iw->iw_arg0 = arg0;
	else {
		iw->iw_cond = (condition *) arg0;
		iw->iw_carg = arg1;
	}
	iw->iw_act = isis_ctp->task_act;
	act_ev(1, isis_ctp->task_act, ACT_WATCH);
	iw->iw_id = ++IW_ID;
	ISIS_RETURN(iw->iw_id);
}

void
isis_wait_cancel(wid)
	register wid;
{
	register iw_desc *iw, *lw = 0;
	register flag, signo;

	ISIS_ENTER();
	for (iw = iw_root; iw; iw = iw->iw_next)
		if (iw->iw_id == wid) {
			if (lw)
				lw->iw_next = iw->iw_next;
			else
				iw_root = iw->iw_next;
			flag = iw->iw_flag;
			signo = iw->iw_signo;
			act_ev(-1, iw->iw_act, ACT_WATCH);
			iw_free(iw);
			if (signo)
				for (iw = iw_root; iw; iw = iw->iw_next)
					if (iw->iw_signo == signo)
						break;
			if (iw == 0) {
				bic(&want_sig, signo);
				signal(signo, SIG_IGN);
				isis_sigcount -= sig_count[signo];
				sig_count[signo] = 0;
			}
			if (flag & IW_HASIMASK) {
				bclr(&ISIS_IMASK);
				for (iw = iw_root; iw; iw = iw->iw_next)
					bisv(&ISIS_IMASK, iw->iw_imask);
			}
			if (flag & IW_HASOMASK) {
				bclr(&ISIS_OMASK);
				for (iw = iw_root; iw; iw = iw->iw_next)
					bisv(&ISIS_OMASK, iw->iw_omask);
			}
			if (flag & IW_HASXMASK) {
				bclr(&ISIS_XMASK);
				for (iw = iw_root; iw; iw = iw->iw_next)
					bisv(&ISIS_XMASK, iw->iw_xmask);
			}
			if (flag)
				isis_setmax();
			break;
		} else
			lw = iw;
	ISIS_EXIT();
}

int
isis_wait(masklen, imask, omask, xmask, tp)
	int masklen;
	register int *imask, *omask, *xmask;
	register struct timeval *tp;
{
	return isis_select(masklen, imask, omask, xmask, tp);
}

int
isis_select(masklen, a_imask, a_omask, a_xmask, tp)
	int masklen;
	int *a_imask, *a_omask, *a_xmask;
	register struct timeval *tp;
{
	register int *imask, *omask, *xmask;
	bitvec in, out, exc;
	register tid = 0, wid = 0;
	condition wake_me_up = 0;
	int nb;

	ISIS_ENTER();
	imask = a_imask;
	omask = a_omask;
	xmask = a_xmask;
	if (masklen > MAXBITS)
		masklen = MAXBITS;
	if (masklen & ISIS_BO)
		panic("isis_select: masklen not multiple of %d", ISIS_BS);
	masklen >>= ISIS_BSHIFT;
	if (masklen != (MAXBITS >> ISIS_BSHIFT)) {
		register n;
		register long *ip;

		if (masklen == 0)
			imask = omask = xmask = (int *) 0;
		if (imask) {
			bclr(&in);
			ip = in.bv_data;
			n = masklen;
			while (n--)
				*ip++ = *imask++;
			imask = (int *) &in;
		}
		if (omask) {
			bclr(&out);
			ip = out.bv_data;
			n = masklen;
			while (n--)
				*ip++ = *omask++;
			omask = (int *) &out;
		}
		if (xmask) {
			bclr(&exc);
			ip = exc.bv_data;
			n = masklen;
			while (n--)
				*ip++ = *xmask++;
			xmask = (int *) &exc;
		}
	}
	if (tp && (tp->tv_sec || tp->tv_usec)) {
		register ms = tp->tv_sec * 1000 + tp->tv_usec / 1000;

		isis_ctp->task_sleep = ms;
		tid = isis_timeout(ms, (vfunc *) t_sig, (VOID *) & wake_me_up, NULLARG);
	}
	if (imask || omask || xmask)
		wid =
		    isis_do_select((bitvec *) imask, (bitvec *) omask, (bitvec *) xmask, 0,
				   (vfunc *) t_sig, (VOID *) & wake_me_up, (VOID *) 1);
	nb = (int) t_wait_l(&wake_me_up, "isis_wait");
	if (wid) {
		register n;
		register long *ip;

		isis_wait_cancel(wid);

		if (nb == 0) {
			masklen <<= 2;
			if (imask)
				bzero(a_imask, masklen);
			if (omask)
				bzero(a_omask, masklen);
			if (xmask)
				bzero(a_xmask, masklen);
		} else if (nb == -1)
			isis_errno = errno;
		else {
			nb = 0;
			if (imask) {
				ip = a_imask;
				n = masklen;
				while (n--) {
					if (*ip++ = *imask++) {
						register m;

						for (m = 1; m; m <<= 1)
							if (imask[-1] & m)
								++nb;
					}
				}
			}
			if (omask) {
				ip = a_omask;
				n = masklen;
				while (n--)
					if (*ip++ = *omask++) {
						register m;

						for (m = 1; m; m <<= 1)
							if (omask[-1] & m)
								++nb;
					}
			}
			if (xmask) {
				ip = a_xmask;
				n = masklen;
				while (n--)
					if (*ip++ = *xmask++) {
						register m;

						for (m = 1; m; m <<= 1)
							if (xmask[-1] & m)
								++nb;
					}
			}
		}
	} else
		nb = 0;
	if (tid) {
		isis_ctp->task_sleep = 0;
		if (nb)
			isis_timeout_cancel(tid);
	}
	ISIS_RETURN(nb);
}

isis_pseudo_io(routine)
	vfunc *routine;
{
	static ids;
	register id = ++ids;

	qu_add(work_queue, id, routine, NULLROUTINE);
	return (id);
}

void
isis_pseudo_io_cancel(id)
{
	register qnode *qp;

	if (qp = qu_find(work_queue, id))
		qu_free(qp);
}

void
fork_sighandlers()
{
	register save = isis_ctp->task_act;
	register iw_desc *iw;
	register n, sigged = 0;

	for (iw = iw_root; iw; iw = iw->iw_next) {
		register i = iw->iw_signo;

		n = sig_count[i];
		while (n-- > 0) {
			++sigged;
			if (iw->iw_routine != (vfunc *) t_sig) {
				isis_ctp->task_act = 0;
				t_fork(iw->iw_routine, iw->iw_arg0);
			} else if (iw->iw_cond && t_waiting(iw->iw_cond))
				t_sig(iw->iw_cond, iw->iw_carg);
			else if (iw->iw_cond)
				panic("isis_signal_sig: nobody waiting on cond 0x%x\n",
				      iw->iw_cond);
		}
	}
	if (sigged)
		for (iw = iw_root; iw; iw = iw->iw_next) {
			register i = iw->iw_signo;

			isis_sigcount -= sig_count[i];
			sig_count[i] = 0;
		}
	isis_ctp->task_act = save;
}

static condition for_ever = 0;

void
isis_mainloop(routine, arg0)
	vfunc *routine;
	VOID *arg0;
{
	ISIS_ENTER();
	if (routine)
		t_fork(routine, arg0)->task_flag |= TASK_START;
	else
		isis_start_done();
	isis_ctp->task_act = 0;
	while (isis_scheduler == 0 || isis_scheduler == isis_ctp)
		run_isis(ISIS_BLOCK);
	forever {
		isis_accept_events(ISIS_BLOCK);
	}
	ISIS_EXIT();
}

void
run_tasks()
{
	ISIS_ENTER();
	isis_accept_events(0);
	ISIS_EXIT();
}

void
run_isis(block_flag)
	int block_flag;
{
	extern intercl_xmits;
	register bitvec *ip, *op, *xp;
	register i = 0, n, looped = 0;
	struct timeval sel_wait, *tp;
	int did_pseudo_io;
	static running, skip_count;

#ifdef  THREADS
	extern thread_wants_entry;
#endif
	int set_scheduler = 0, ran = 0;
	static count;

	if (++running != 1)
		panic("recursive call to isis_accept_events");
	count = 0;
	if (isis_scheduler == 0) {
		isis_scheduler = isis_ctp;
		++set_scheduler;
	}
#ifdef  THREADS
	if (thread_wants_entry) {
		THREAD_YIELD(isis_mutex);
	}
#endif				/* THREADS */
	if (isis_runtasks())
		ran++;
      again:
	if ((isis_state & ISIS_INIT) == 0) {
		if (qu_head(time_queue) == 0)
			goto done;
	}
	did_pseudo_io = 0;
	ip = &cur_imask;
	if (isis_imask)
		cur_imask = ISIS_IMASK;
	else
		bclr(&cur_imask);
	if (isis_omask) {
		cur_omask = ISIS_OMASK;
		op = &cur_omask;
	} else
		op = (bitvec *) 0;
	if (isis_xmask)
		cur_xmask = ISIS_XMASK;
	else
		bclr(&cur_xmask);
	xp = &cur_xmask;
	/* Always check for input on protos and private sockets */
	ip->bv_data[0] |= 1 << isis_socket;
	ip->bv_data[0] |= 1 << intercl_socket;
	/* RACE CONDITION HYPOTHESIZED BY FRANK HERE! */
	tp = &sel_poll;
	if (block_flag && async_accept_count == 0 && !ran && isis_runqueue == 0) {
		register qnode *qp;

		if (act_blocked == 0 && isis_sigcount)
			tp = &sel_poll;
		else if (qp = qu_head(time_queue)) {
			register time;

			set_isis_time();
			time = qp->qu_time - ISIS_TIME;
			if (time < 250)
				time = 250;
			sel_wait.tv_sec = time / 1000;
			sel_wait.tv_usec = (time % 1000) * 1000;
			tp = &sel_wait;
		} else
			tp = (struct timeval *) 0;
	}
	if (tp != &sel_poll /* || ++skip_count == 10 */ ) {
		register qnode *qp;

		skip_count = 0;
		if ((isis_state & ISIS_WANTACK) && (isis_joining || (isis_state & ISIS_WJOIN)))
			intercl_flushacks();
		for (qp = work_queue->qu_next; qp != work_queue; qp = qp->qu_next)
			if ((*qp->qu_proc) (0)) {
				tp = &sel_poll;
				break;
			}
	}
	bisv(xp, ip);
	if (op)
		bisv(xp, op);
	if ((n = SELECT(isis_mask_bits, (int *) ip, (int *) op, (int *) xp, tp))
	    == -1) {
		if (errno != EINTR) {
			register iw_desc *iw;

			if (errno == EBADF) {
				register save = isis_ctp->task_act;
				register iw_desc *iw, *niw;

				for (iw = iw_root; iw; iw = niw) {
					niw = iw->iw_next;
					if (((iw->iw_flag & IW_HASIMASK) && bitv(iw->iw_imask, xp))
					    || ((iw->iw_flag & IW_HASOMASK)
						&& bitv(iw->iw_omask, xp))
					    || ((iw->iw_flag & IW_HASXMASK)
						&& bitv(iw->iw_xmask, xp))) {
						isis_ctp->task_act = iw->iw_act;
						if (iw->iw_routine != (vfunc *) t_sig)
							t_fork(iw->iw_routine, iw->iw_arg0);
						else if (iw->iw_cond && t_waiting(iw->iw_cond))
							t_sig(iw->iw_cond, iw->iw_carg);
						else if (iw->iw_cond)
							panic
							    ("no task waiting for input on fdes %d (cond 0x%x arg %x)\n",
							     i, iw->iw_cond, iw->iw_carg);
						isis_wait_cancel(iw->iw_id);
					}
				}
				isis_ctp->task_act = save;
			} else
				perror("select");
		}
		bclr(&cur_imask);
		bclr(&cur_omask);
		bclr(&cur_xmask);
		n = 0;
	}
	set_isis_time();
	sel_imask = cur_imask;
	sel_omask = cur_omask;
	if (isis_sigcount && act_blocked == 0)
		fork_sighandlers();
	if (n) {
		register save = isis_ctp->task_act;
		register iw_desc *iw;

		for (iw = iw_root; iw; iw = iw->iw_next)
			if (((iw->iw_flag & IW_HASIMASK) && bitv(iw->iw_imask, ip)) ||
			    ((iw->iw_flag & IW_HASOMASK) && bitv(iw->iw_omask, op)) ||
			    ((iw->iw_flag & IW_HASXMASK) && bitv(iw->iw_xmask, xp))) {
				isis_ctp->task_act = iw->iw_act;
				if (iw->iw_flag & IW_HASIMASK) {
					bandv(iw->iw_imask, ip);
					bicv(ip, iw->iw_imask);
				}
				if (iw->iw_flag & IW_HASOMASK) {
					bandv(iw->iw_omask, op);
					bicv(op, iw->iw_omask);
				}
				if (iw->iw_flag & IW_HASXMASK) {
					bandv(iw->iw_xmask, xp);
					bicv(xp, iw->iw_xmask);
				}
				if (iw->iw_routine != (vfunc *) t_sig)
					t_fork(iw->iw_routine, iw->iw_arg0);
				else if (iw->iw_cond && t_waiting(iw->iw_cond))
					t_sig(iw->iw_cond, iw->iw_carg);
				else if (iw->iw_cond)
					panic
					    ("no task waiting for input on fdes %d (cond 0x%x arg %x)\n",
					     i, iw->iw_cond, iw->iw_carg);
			}
		isis_ctp->task_act = save;
	}
	/* Intercl input will go first into the intercl "tank" */
	intercl_xmits = 0;
	if (ip->bv_data[0] & (1 << intercl_socket))
		intercl_do_input();
	if (!(isis_state & ISIS_REMOTE) && (ip->bv_data[0] & (1 << isis_socket))) {
		static rmask;

		/* This order biases in favor of input from protos */
		do {
			rmask = 1 << isis_socket;
			if (isis_read() == 0)
				isis_has_crashed(-1);
			while (SELECT(32, &rmask, NULLIARG, NULLIARG, &sel_poll) == -1)
				if (errno != EINTR) {
					if (errno != EBADF)
						perror("select failed");
					else
						isis_has_crashed(-1);
					goto done;
				}
		}
		while (rmask);
	}
	if (isis_state & ISIS_REMOTE) {
		register qnode *qp;

		while (qp = qu_head(mother_msg_queue)) {
			register message *mp = qp->qu_msg;

			msg_increfcount(mp);
			qu_free(qp);
			isis_sys_accept(mp);
		}
	}
	begin {
		register qnode *qp;
		register printed = 0;

		while (qp = qu_head(time_queue))
			if (qp->qu_time <= ISIS_TIME) {
				register dt;

				if (!printed && (dt = ISIS_TIME - qp->qu_time) > 5000) {
					++printed;
					print("ISIS client pid %d: time warp (%d.%.3d secs)!\n",
					      my_process_id, dt / 1000, dt % 1000);
				}
				++isis_wakeup_sync, ++did_pseudo_io, ++ran;
				qu_remove(qp);
				qu_free(qp);
				continue;
			} else
				break;
	}
	begin {
		register qnode *qp;

		for (qp = work_queue->qu_next; qp != work_queue; qp = qp->qu_next)
			if ((*qp->qu_proc) (1))
				++isis_wakeup_sync, ++ran, ++did_pseudo_io;
	}
      done:
	i = isis_runtasks();
	if (i == -1 && did_pseudo_io == 0 && ++isis_events < ISIS_MAXEVENTS) {
		ran++;
		goto again;
	} else if (i)
		ran++;
	if (async_accept_count || isis_wakeup_sync)
		t_sig_all(&accept_events, 0);
	if (set_scheduler)
		isis_scheduler = (task *) 0;
	--running;
	isis_events = 0;
}

/* Accept any pending events, run corresponding tasks... return to caller when done */
void
isis_accept_events(flag, tp)
	int flag;
	struct timeval *tp;
{
	ISIS_ENTER();
	if (isis_at == 0 && flag == ISIS_TIMEOUT) {
		++isis_at;
		isis_timeout(tp->tv_sec * 1000 + tp->tv_usec / 1000, isis_accept_timeout,
			     NULLARG, NULLARG);
	}
	if (flag)
		flag = ISIS_BLOCK;
	if (isis_scheduler && isis_ctp != isis_scheduler) {
		if (flag == 0)
			++async_accept_count;
		isis_wakeup_sync = 0;
		(void) t_wait_l(&accept_events, "isis system: suspended in isis_accept_events");
		if (flag == 0)
			--async_accept_count;
	} else
		run_isis(flag);
	ISIS_EXIT();
}

isis_work_to_do()
{
	register qnode *mq;

	if (t_waiting(&isis_runqueue))
		return (1);
	mq = act_blocked ? msg_mutexqueue : msg_queue;
	if (qu_head(mq))
		return (1);
	return (0);
}

isis_ondrain(routine, arg)
	vfunc *routine;
	VOID *arg;
{
	static id;

	qu_add(isis_drainq, ++id, arg, routine);
	return (id);
}

/*
 *      Basic algorithm:
 *              Input one message
 *              Run it, and anything it choses to t_fork off
 *              Loop
 */
static int
isis_runtasks()
{
	register found_one, ran_one, save_act;
	register qnode *qp, *mq;
	register task *tp;
	int count;
	static running_tasks;

	if (running_tasks)
		return (0);
	/* Places a limit on how long ISIS will run before noticing input from protos */
	count = msg_count + 250;
	running_tasks++;
	ran_one = 0;
	do {
		extern int tank_status;

		if ((tp = task_dequeue(&isis_runqueue)) != (task *) - 1) {
			do {
				++ran_one;
				isis_wakeup_sync = 1;
				task_swtch(tp, NULLROUTINE, NULLARG);
			}
			while ((tp = task_dequeue(&isis_runqueue)) != (task *) - 1);
		}
		/* No tasks left to run... scan message qnode and see if anything can be processed */
		found_one = 0;
		mq = act_blocked ? msg_mutexqueue : msg_queue;
		if (qp = qu_head(mq)) {
			register message *mp = qp->qu_msg;
			register address *ap;
			register entry = 0;

			isis_wakeup_sync = 1;
			++found_one;
			++ran_one;
			++isis_events;
			save_act = isis_ctp->task_act;
			if ((isis_ctp->task_act = qp->qu_act) == 0 && act_blocked)
				isis_ctp->task_act = act_blocked;
			for (ap = msg_getdests(mp); !aptr_isnull(ap); ap++)
				if (addr_ismine(ap)) {
					entry = ap->addr_entry;
					break;
				}
			if (act_blocked == 0 && entry == GENERIC_NEW_VIEW) {
				check_cl_watch_queue(mp);
				if (act_blocked)
					continue;
			}
			qu_remove(qp);
			--msg_count;
			if (entry == GENERIC_RCV_REPLY)
				cl_rcv_reply(mp);
			else
				ISISCALL1(msg_filter, mp);
			act_ev(-1, qp->qu_act, ACT_MSG);
			isis_ctp->task_act = save_act;
			qu_free(qp);
		}
		if ((!found_one || count == 1) && !isis_work_to_do())
			if (qp = qu_head(isis_drainq)) {
				qu_remove(qp);
				qu_free(qp);
				++found_one;
			}

		/* Now check the message ``tank'' for cl-cl messages */
		if (tank_status != TANK_EMPTY) {
			++found_one;
			++ran_one;
			++isis_events;
			intercl_deliver();
		}
	}
	while (found_one && --count > 0);
	--running_tasks;
	if (found_one && count == 0)
		return (-1);
	return (ran_one);
}

void
cl_register()
{
	int pid[2];
	register message *msg;

	pid[0] = my_process_id;
	pid[1] = my_port_no;
	msg = msg_genmsg(CL_PID, (char *) pid, FTYPE_LONG, sizeof(pid), 0);
	isis(CL_REGISTER, msg, &my_site_incarn, sizeof(int));
	my_address = ADDRESS(my_site_no, my_site_incarn, my_process_id, 0);
	my_address.addr_portno = my_port_no;
	ci.ci_my_site_incarn = my_site_incarn;
	msg_delete(msg);
	t_sig_all(&isis_want_incarn, 0);
}

int ISIS_MSGID;

/* Do a system call and copy the answer to the designated place */
int
isis(ent, msg, answ, alen)
	int ent;
	message *msg;
	VOID *answ;
	int alen;
{
	register message *rmsg;
	message *collect_reply();
	register char *ptr;
	int len, msgid = isis_ctp->task_msgid;
	address addr;

	if (my_site_incarn == RECOVERY_INCARN && ent != CL_REGISTER && ent != CL_FDRESTART)
		t_wait_l(&isis_want_incarn, "clib: waiting for site-incarn no");
	if (msgid == 0)
		msgid = (ISIS_MSGID += 2) + 1;
	msg_getid(msg) = msgid;
	addr = PRO(ent);
	isis_dosend(&addr, msg);
	rmsg = collect_reply(msgid);
	isis_ctp->task_msgid = 0;
	if (alen && (ptr = msg_getfield(rmsg, FLD_ANSW, 1, &len))) {
		if (len)
			if (alen == AMALLOC)
				bcopy(ptr, (*(char **) answ) = (char *) malloc(len), len);
			else if (len <= alen)
				bcopy(ptr, answ, len);
			else
				bcopy(ptr, answ, alen);
		msg_delete(rmsg);
		return (len);
	}
	msg_delete(rmsg);
	if (alen == 0)
		return (0);
	return (-1);
}

/* Send a message and wait for the response.  */
message *
protos_rpc(dest, msg, pause_flag)
	address *dest;
	message *msg;
	int pause_flag;
{
	message *collect_reply();
	int msgid = isis_ctp->task_msgid;

	if (msgid == 0)
		msgid = (ISIS_MSGID += 2) + 1;
	if (my_site_incarn == RECOVERY_INCARN)
		t_wait_l(&isis_want_incarn, "clib: waiting for site-incarn no");
	if ((isis_state & ISIS_INIT) == 0)
		return (msg_newmsg());
	msg_getid(msg) = msgid;
	isis_dosend(dest, msg);
	if (pause_flag)
		t_wait_l(&isis_runqueue, "isis system: pausing before collecting syscall reply");
	return (collect_reply(msgid));
}

/* Send this message to the designated entry point */
int
isis_send(dest, msg)
	address *dest;
	message *msg;
{
	int msgid = isis_ctp->task_msgid;

	if (msgid == 0)
		msgid = (ISIS_MSGID += 2);
	if (my_site_incarn == RECOVERY_INCARN)
		t_wait_l(&isis_want_incarn, "clib: waiting for site-incarn no");
	if ((isis_state & ISIS_INIT) == 0)
		return (-1);
	msg_getid(msg) = msgid;
	isis_dosend(dest, msg);
	return (0);
}

void
isis_dosend(dest, msg)
	address *dest;
	message *msg;
{
	msg_setdest(msg, dest);
	if (isis_ctp->task_act)
		msg_setact(msg, &act_map[isis_ctp->task_act].act_id);
	isis_ctp->task_sentto = *dest;
	if (addr_ismine(dest))
		isis_gotmsg(msg, BYP_DONTCHECK, 0);
	else if (isis_state & ISIS_REMOTE)
		net_send((address *) 0, 0, msg, via_mom, NULLROUTINE, NULLARG, NULLARG);
	else
		msg_write(isis_socket, msg);
}

/*
 * Drain any messages for this process... and
 * notice if ISIS is congested while I'm at it
 */
void
isis_input_drain()
{
	bitvec imask;

	if (my_process_id < -10)
		return;
	ISIS_ENTER();
	bclr(&imask);
	forever {
		bis(&imask, isis_socket);
		if (SELECT(isis_mask_bits, (int *) &imask, NULLIARG, NULLIARG, &sel_poll) != 1)
			break;
		if (isis_read() == 0)
			break;
	}
	ISIS_EXIT();
}

condition isis_decongested;

void
isis_decon_wait(nwant)
	int nwant;
{
	ISIS_ENTER();
	if (isis_state & ISIS_CONGESTED) {
		isis_ctp->task_flag |= TASK_CONGESTED;
		t_wait_l(&isis_decongested, "isis system: congested; waiting until drained");
		isis_ctp->task_flag &= ~TASK_CONGESTED;
	}
	ISIS_EXIT();
}

int
isis_read()
{
	register message *mp;
	register rval;

	ISIS_ENTER();
	if (mp = msg_read(isis_socket)) {
		if (my_process_id == ISIS && msg_getdests(mp)->addr_process != ISIS)
			net_send((address *) 0, -1, mp, msg_getdests(mp), NULLROUTINE, NULLARG,
				 NULLARG);
		else
			isis_sys_accept(mp);
		rval = 1;
	} else
		rval = 0;
	ISIS_RETURN(rval);
}

isis_sys_accept(mp)
	register message *mp;
{
	register entry;

	entry = msg_getdests(mp)->addr_entry;
	if (isis_state & ISIS_TRACE) {
		print("%d (act %d): RCV ", my_process_id, act_blocked);
		pmsg(mp);
	}
	switch (entry) {
	case GENERIC_WANTDUMP:
		if (my_process_id > 0 && !isis_dontlogdumps) {
			print("Received \"snapshot\" dump request.  Spooling dump into %d.log\n",
			      getpid());
			isis_logging(1);
			cl_dump(DUMP_ALL, "received snapshot request");
			isis_logging(0);
		}
		break;

	case GENERIC_CONGESTED:
		isis_state |= ISIS_CONGESTED;
		isis_ncongest++;
		break;

	case GENERIC_DECONGESTED:
		if (--isis_ncongest == 0) {
			isis_state &= ~ISIS_CONGESTED;
			t_sig_all(&isis_decongested, 0);
		}
		break;

	default:
		/* First check for bypass related stuff */
		bypass_precheck(mp, entry);
		if (isis_state & ISIS_FBLOCK) {
			msg_increfcount(mp);
			qu_add_mp(protos_spool, 0, mp, (vfunc *) MSG_DELETE);
		} else
			protos_gotmsg(mp);
	}
	msg_delete(mp);
}

message *protos_blockmsg;		/* Message that caused the excitement */

void
protos_despool()
{
	register qnode *qp;
	register message *mp;

	if (mp = protos_blockmsg) {
		protos_blockmsg = 0;
		isis_gotmsg(mp, BYP_DONTCHECK, 0);
	}
	while ((isis_state & ISIS_FBLOCK) == 0 && (qp = qu_head(protos_spool))) {
		qu_remove(qp);
		protos_gotmsg(qp->qu_msg);
		qu_free(qp);
	}
}

/* This is the normal, unblocked route in from protos.  Catch flush here */
void
protos_gotmsg(mp)
	register message *mp;
{
	if (msg_getdests(mp)->addr_entry == GENERIC_NEW_VIEW) {
		t_fork_urgent_msg((vfunc *) bypass_checkview, mp);
		return;
	}
	if (msg_getfield(mp, SYSFLD_DOFLUSH, 1, (int *) 0)) {
		void by_flushfirst();

		t_fork_urgent_msg((vfunc *) by_flushfirst, mp);
		return;
	}
	isis_gotmsg(mp, BYP_DONTCHECK, 0);
}

/*
 * In bypass mode, called with flag=BYP_CHECK and pn >= 0 
 */
void
isis_gotmsg(mp, flag, pn)
	register message *mp;
	int flag, pn;
{
	register address *ap;
	register act = -1;

	msg_increfcount(mp);
	if (flag == BYP_CHECK) {
		register by_info *byi;

		if (byi = (by_info *) msg_getbyi(mp)) {
			bypass_recv(byi, mp, pn);
			msg_delete(mp);
			return;
		}
	}
	if (msg_getid(mp) > ISIS_MSGID)
		ISIS_MSGID = (msg_getid(mp) & ~1);
	mp->msg_transport = pn;
	if (ap = msg_getact(mp)) {
		register i;

		for (i = 0; i != MAX_ACT; i++)
			if (ACT_VALID(i) && act_isequal(&act_map[i].act_id, ap)) {
				act = i;
				break;
			}
		if (act == -1)
			act = act_start(ap, 0);
	} else
		act = 0;

	if (ap = msg_getforwarder(mp))
		msg_setsender(mp, &ap[FWI_SENDER]);

	/* Note: <ap> is a list... safe to switch on <entry> only for certain generic entries! */
	ap = msg_getdests(mp);
	if (ap == 0) {
		print("Received message with no destinations!  ");
		pmsg(mp);
		msg_delete(mp);
		return;
	}
	switch (ap->addr_entry) {
	case GENERIC_NEW_SNAMES:
	begin {
		register char *sn;
		register i;

		sn = msg_getfield(mp, CL_NEW_SNAMES, 1, &site_len);
		bzero((char *) site_names, sizeof(site_names));
		bcopy(sn, (char *) site_names, site_len);
	}
		break;

	case GENERIC_BYORDER:
		bypass_aborder(mp);
		break;

	case GENERIC_BYFLSH:
		bypass_flush(mp);
		break;

	case GENERIC_BYWAKEUP:
	case GENERIC_NEW_VIEW:
	case GENERIC_DEL_PGROUP:
	case GENERIC_PROC_FAILED:
	case GENERIC_NEW_SVIEW:
		/* Except for BYWAKEUP, adds to both message queues */
		if (act_blocked) {
			act_ev(1, act, ACT_MSG);
			msg_increfcount(mp);
			qu_add_mp(msg_mutexqueue, act, mp, (vfunc *) MSG_DELETE);
			++msg_count;
			if (ap->addr_entry == GENERIC_BYWAKEUP)
				break;
		}
		act_ev(1, act, ACT_MSG);
		msg_increfcount(mp);
		qu_add_mp(msg_queue, act, mp, (vfunc *) MSG_DELETE);
		++msg_count;
		break;

	default:
		act_ev(1, act, ACT_MSG);
		msg_increfcount(mp);
		++msg_count;
		if (act_blocked && act == act_blocked)
			qu_add_mp(msg_mutexqueue, act, mp, (vfunc *) MSG_DELETE);
		else
			qu_add_mp(msg_queue, act, mp, (vfunc *) MSG_DELETE);
	}
	msg_delete(mp);
}

static ACT_IDS;

int
act_begin()
{
	register old_act = isis_ctp->task_act;
	address aname;

	aname = my_address;
	aname.addr_portno = ISACT;
	aname.addr_entry = ++ACT_IDS;
	ACT_IDS &= 0xFF;
	isis_ctp->task_act = act_start(&aname, old_act);
	return (old_act);
}

void
act_end(old_act)
	int old_act;
{
	isis_ctp->task_act = old_act;
}

int
act_start(aname, old_act)
	address *aname;
	int old_act;
{
	register act;

	act_scan();
	for (act = 1; act < MAX_ACT; act++)
		if (ACT_VALID(act) == 0)
			break;
	if (act == MAX_ACT)
		panic("can't find an act_map slot");
	act_bits |= 1 << act;
	act_map[act].act_id = *aname;
	act_map[act].act_evcount = 0;
	act_map[act].act_gotres[0] = act_map[act].act_gotres[1] = 0;
	return (act);
}

void
act_scan()
{
	register act, bno;

	bno = 2;
	for (act = 1; act < MAX_ACT; act++, (bno <<= 1))
		if ((act_bits & bno) && act_map[act].act_evcount == 0) {
			register qnode *qp;

			for (qp = isis_tasks->qu_next; qp != isis_tasks; qp = qp->qu_next)
				if (qp->qu_task->task_act == act)
					break;
			if (qp == isis_tasks)
				act_bits &= ~bno;
		}
}

void
act_ev(inc, act, type)
	int inc;
	register act;
	int type;
{
	act_map[act].act_evcount += inc;
}

void
cc_gotres(where)
	int where;
{
	act_map[isis_ctp->task_act].act_gotres[where]++;
}

void
dump_act()
{
	register qnode *qp;
	register act;

	act_scan();
	if (act_blocked) {
		print("\nActivity %d ", act_blocked);
		paddr(&act_map[act_blocked].act_id);
		if (isis_state & ISIS_STARTUP)
			print(" is doing startup\n");
		else
			print(" holds Mutex (entered in viewid %d.%d)\n", VMM(jbviewid));
	} else
		print("\nActivities:\n");
	for (act = 0; act < MAX_ACT; act++)
		if (ACT_VALID(act)) {
			print("  Slot %d = ", act);
			if (act)
				paddr(&act_map[act].act_id);
			else
				print("(system activity)");
			print(", %d pending watch/monitor requests and messages",
			      act_map[act].act_evcount);
			if (act_map[act].act_gotres)
				print(", got cc_result %d/%d", act_map[act].act_gotres[0],
				      act_map[act].act_gotres[1]);
			print("\n");
		}
	if (msg_mutexqueue || msg_queue)
		print("\nMessage queues:\n");
	if (msg_mutexqueue)
		for (qp = msg_mutexqueue->qu_next; qp != msg_mutexqueue; qp = qp->qu_next) {
			print(qp->qu_act ? "  [act %d" : "  [act *", qp->qu_act);
			print(" (* holds mutex *)] ");
			pmsg(qp->qu_msg);
		}
	if (msg_queue)
		for (qp = msg_queue->qu_next; qp != msg_queue; qp = qp->qu_next) {
			print(qp->qu_act ? "  [act %d] " : "  [act *] ", qp->qu_act);
			pmsg(qp->qu_msg);
		}
	if (isis_msgs_enqueued) {
		register entry;
		register qnode *eq;

		print("\nEnqueued using MSG_ENQUEUE:\n");
		for (entry = 0; entry < MAXENTRIES; entry++)
			if (eq = cl_queues[entry])
				for (qp = eq->qu_next; qp != eq; qp = qp->qu_next) {
					print("  [entry %d] ", entry);
					pmsg(qp->qu_msg);
				}
	}
	if (qu_head(protos_spool)) {
		print("\nBlocked from protos:\n");
		for (qp = protos_spool->qu_next; qp != protos_spool; qp = qp->qu_next) {
			print("  ");
			pmsg(qp->qu_msg);
		}
	}
	if (qu_head(bypass_waitq)) {
		print("\nBlocked on bypass_waitq:\n");
		for (qp = bypass_waitq->qu_next; qp != bypass_waitq; qp = qp->qu_next) {
			print("  ");
			pmsg(qp->qu_msg);
		}
	}
}

static block_count;

void
act_block()
{
	register qnode *qp, *nqp;
	register ginfo *gip;

	if (act_blocked && isis_ctp->task_act != act_blocked)
		panic("act_block");
	++block_count;
	if (act_blocked)
		return;
	if (isis_ctp->task_act == 0)
		act_begin();
	act_blocked = isis_ctp->task_act;
	for (gip = isis_groups; gip; gip = gip->gi_next) {
		if (gip->gi_mutexview = gip->gi_view)
			++gip->gi_mutexview->gv_refcount;
	}
	for (qp = msg_queue->qu_next; qp != msg_queue; qp = nqp) {
		register message *mp;
		register entry;

		nqp = qp->qu_next;
		if (qp->qu_act == act_blocked) {
			qu_remove(qp);
			qu_append(msg_mutexqueue, qp);
		} else
			switch (entry = msg_getdests(mp = qp->qu_msg)->addr_entry) {
			case GENERIC_NEW_VIEW:
			case GENERIC_BYWAKEUP:
			case GENERIC_DEL_PGROUP:
			case GENERIC_PROC_FAILED:
			case GENERIC_NEW_SVIEW:
				act_ev(1, 0, ACT_MSG);
				msg_increfcount(mp);
				++msg_count;
				qu_add_mp(msg_mutexqueue, 0, mp, (vfunc *) MSG_DELETE);
				if (entry == GENERIC_BYWAKEUP)
					qu_free(qp);
			}
	}
	isis_svmutex = isis_sv;
}

void
act_restart()
{
	register qnode *qp, *lqp;
	register ginfo *gip;

	if (act_blocked == 0) {
		print("act_restart\n");
		return;
	}
	if (--block_count)
		return;
	for (gip = isis_groups; gip; gip = gip->gi_next) {
		if (gip->gi_mutexview) {
			isis_gv_free(gip->gi_mutexview);
			gip->gi_mutexview = 0;
		}
	}
	act_blocked = 0;
	for (qp = msg_mutexqueue->qu_last; qp != msg_mutexqueue; qp = lqp) {
		lqp = qp->qu_last;
		if (qp->qu_act) {
			register qnode *nqp;

			qu_remove(qp);
			nqp = msg_queue->qu_next;
			qu_append(nqp, qp);
		} else {
			--msg_count;
			qu_free(qp);
		}
	}
}

/* Filters messages through a settable routine */
#ifdef CONVEX
void *
isis_setfilter(proc)
#else
vfunc *
isis_setfilter(proc)
#endif
	vfunc *proc;
{
	register vfunc *old = msg_filter;

	if ((msg_filter = (vfunc *) proc) == NULLROUTINE) {
		msg_filter = old;
		old = NULLROUTINE;
	}
	return (old);
}

static shutting_down;

void
isis_has_crashed(how)
	int how;
{
	register state = isis_state;

	if ((state & ISIS_ISUP) == 0)
		return;
	if (my_process_id < 0 && my_process_id != ISIS)
		exit(0);
	if (isis_socket)
		close(isis_socket);
	isis_state = ISIS_TINIT;
	isis_socket = 0;
	if (shutting_down)
		exit(0);
	if (how == -1 && isis_failed()) {
		if ((state & ISIS_ISUP) == 0)
			panic("<isis-protos> is not running on this machine");
		else
			panic("<isis-protos> shutdown detected");
		exit(0);
	}
	t_sig_all(&for_ever, 0);
	t_sig_all(&accept_events, 0);
}

void
MSG_ENQUEUE(entry, mp)
	register entry;
	register message *mp;
{
	++isis_msgs_enqueued;
	msg_increfcount(mp);
	if (t_waiting(&cl_mwant[entry]))
		t_sig(&cl_mwant[entry], (VOID *) mp);
	else {
		msg_increfcount(mp);
		qu_add_mp(cl_queues[entry], 0, mp, (vfunc *) MSG_DELETE);
	}
}

/* Pull next message from this entry? */
message *
msg_rcv(entry)
	register entry;
{
	register qnode *qp;

	--isis_msgs_enqueued;
	if (qp = qu_head(cl_queues[entry])) {
		register message *mp;

		mp = qp->qu_msg;
		qu_free(qp);
		return (mp);
	}
	return ((message *) t_wait_l(&cl_mwant[entry], "clib:msg_rcv"));
}

/* Is their a message waiting on this entry? */
msg_ready(entry)
	register entry;
{
	register qnode *xp, *qp;
	register n = 0;

	if (qp = cl_queues[entry])
		for (xp = qp->qu_next; xp != qp; xp = xp->qu_next)
			++n;
	return (n);
}

void
t_printable_name(str, arg)
	register char *str;
	VOID *arg;
{
	isis_ctp->task_flag |= TASK_PRNAME;
	isis_ctp->task_routine = (vfunc *) str;
	isis_ctp->task_arg0 = arg;
}

void
isis_entry(entry, routine, rname)
	int entry;
	vfunc *routine;
	char *rname;
{
	char *cl_getrname();

	if (entry < 0 || entry > MAXENTRIES)
		panic("isis_entry: entry %d out of range 0-%d", entry, MAXENTRIES);
	if (cl_entries[entry] && cl_entries[entry] != routine)
		print("Redefinition of entry %s as %s\n", cl_getrname(entry), rname);
	cl_entries[entry] = routine;
	if (entry < SYS_BASE && (isis_state & ISIS_XBYREF))
		bis(&cl_xbyref, entry);
	isis_task(routine, rname);
	if (routine == (vfunc *) MSG_ENQUEUE)
		cl_queues[entry] = qu_null();
}

void
isis_logentry(gaddr, entry)
	address *gaddr;
	int entry;
{
	register ginfo *gip = map_gaddr(gaddr);

	if (gip == (ginfo *) 0 || entry < 0 || entry > MAXBITS)
		panic("isis_logentry: illegal argument");
	bis(&gip->gi_lentries, entry);
}

void
pentry(addr, entry)
	address *addr;
	int entry;
{
	if (entry == 0 & !addr_ismine(addr))
		print("0");
	else if (addr_ispid(addr)) {
		if (entry >= SYS_BASE && entry <= MAXENTRIES)
			print(generic_entries[entry - SYS_BASE]);
		else if (addr->addr_process == my_process_id && cl_entries[entry])
			cl_rname(cl_entries[entry]);
		else if (addr->addr_process == PROTOCOLS && isis_entry_names[entry])
			print(isis_entry_names[entry]);
		else
			print("%d", entry);
	} else
		print("%d", entry);
}

void
isis_task(routine, rname)
	vfunc *routine;
	char *rname;
{
	register n;

	for (n = 0; n < MAXENTRIES; n++)
		if (cl_routines[n] == 0) {
			cl_routines[n] = routine;
			cl_rnames[n] = rname;
			return;
		}
	panic("isis_task: table overflow");
}

void
cl_rname(routine)
	vfunc *routine;
{
	register n;

	for (n = 0; n < MAXENTRIES; n++)
		if (cl_routines[n] == routine) {
			print("%s", cl_rnames[n]);
			return;
		}
	print("0x%x", routine);
}

char *
cl_getrname(entry)
	int entry;
{
	static char num[16];

	if (cl_entries[entry]) {
		register n;

		for (n = 0; n < MAXENTRIES; n++)
			if (cl_routines[n] == cl_entries[entry])
				return (cl_rnames[n]);
	}
	sprintf(num, "%d", entry);
	return (num);
}

/* Guarded op lookup */
int
gop_lookup(gopname)
	register char *gopname;
{
	register n;

	if (gopname == (char *) 0)
		return (-1);
	for (n = 0; n < MAXENTRIES; n++)
		if (cl_rnames[n] && strcmp(cl_rnames[n], gopname) == 0)
			return (n);
	if (strcmp("term", gopname))
		return (MAXENTRIES);
	print("Guard reference to op <%s>: entry undeclared\n", gopname);
	return (-1);
}

/* Invoke guarded operation */
void
isis_invoke(entry, mp)
	int entry;
	register message *mp;
{
	register address *alist;

	alist = msg_getdests(mp);
	while (!aptr_isnull(alist))
		alist++->addr_entry = entry;
	ISISCALL1(msg_filter, mp);
}

void
cl_local_delivery(mp)
	register message *mp;
{
	register address *alist;

	alist = msg_getdests(mp);
	if (alist == 0)
		panic("cl_local_delivery");
	if (isis_state & ISIS_TRACE) {
		print("%d (act %d): DELIVER ", my_process_id, act_blocked);
		pmsg(mp);
	}
	while (alist->addr_site) {
		register entry = alist->addr_entry;

		if (alist->addr_process == my_process_id && alist->addr_site == my_site_no &&
		    (alist->addr_incarn == my_site_incarn
		     || (alist->addr_incarn == RECOVERY_INCARN
			 || my_site_incarn == RECOVERY_INCARN))) {
			if (cl_entries[entry]) {
				register ginfo *gip;

				for (gip = isis_groups; gip; gip = gip->gi_next)
					if (gip->gi_lnode && bit(&gip->gi_lentries, entry)) {
						log_write_msg(gip, mp, entry);
						break;
					}
				isis_state |= ISIS_RECEIVING;
				isis_enum = entry;
				isis_gip = gip;
				if (entry > 0 && entry < SYS_BASE && bit(&cl_xbyref, entry))
					isis_state |= ISIS_XBYREF;
				if (cl_entries[entry] == (vfunc *) MSG_ENQUEUE)
					MSG_ENQUEUE(entry, mp);
				else
					isis_fork_urgent(cl_entries[entry], (char *) mp, mp);
				if (bit(&cl_xbyref, entry))
					isis_state &= ~ISIS_XBYREF;
			} else
				print
				    ("cl_local_delivery: message to entry %d discarded -- entry not defined\n",
				     entry);
		}
		++alist;
	}
}

message *
collect_reply(msgid)
	int msgid;
{
	register message *mp;

	isis_ctp->task_msgid = msgid;
	mp = (message *) t_wait_l(&isis_ctp->task_mwant, "isis system: collecting reply");
	if (mp == NULLMP)
		panic("collect reply: no reply");
	isis_ctp->task_msgid = 0;
	return (mp);
}

void
cl_new_view(mp)
	register message *mp;
{
	register ginfo *gip;
	register sys_groupview *npg;
	register groupview *gv;
	groupview *isis_gv_alloc();

	npg = (sys_groupview *) msg_getfield(mp, CL_NEWVIEW, 1, NULLIARG);
	gip = add_group(npg->pg_name, &npg->pg_gid);
	gv = act_blocked ? gip->gi_mutexview : gip->gi_view;
	if (gv == 0) {
		gv = isis_gv_alloc();
		if (act_blocked)
			gip->gi_mutexview = gv;
		else
			gip->gi_view = gv;
	} else if (gv->gv_refcount != 1) {
		groupview *ngv;

		--gv->gv_refcount;
		gv->gv_flag |= PG_STALE;
		ngv = isis_gv_alloc();
		bcopy(gv, ngv, sizeof(groupview));
		gv = ngv;
		if (act_blocked)
			gip->gi_mutexview = gv;
		else
			gip->gi_view = gv;
	}
	(void) isis_pg_copy(npg, gv);
	cl_setscope();
	pg_new_view(gv);
	if (act_blocked && gip->gi_view == 0) {
		gip->gi_view = gip->gi_mutexview;
		++gip->gi_view->gv_refcount;
	}
	if (bypass_waitq) {
		register qnode *qp, *nqp, *bp = bypass_waitq;
		extern void waitq_free();

		bypass_waitq = qu_null();
		for (qp = time_queue->qu_next; qp != time_queue; qp = nqp) {
			nqp = qp->qu_next;
			if (qp->qu_freeroutine == (vfunc *) waitq_free) {
				qp->qu_freeroutine = NULLROUTINE;
				qu_free(qp);
			}
		}
		while (qp = qu_head(bp)) {
			bypass_recv(msg_getbyi(qp->qu_msg), qp->qu_msg, qp->qu_pn);
			qu_free(qp);
		}
		qu_free(bp);
	}
	if (act_blocked == 0 && gv->gv_members == 0)
		group_unmap(gip);
}

void
cl_setscope()
{
	register ginfo *gip;

	/* Scope is currently cummulative, but is this a good idea? */
	for (gip = isis_groups; gip; gip = gip->gi_next) {
		register address *ap;
		register groupview *gv;

		gv = act_blocked ? gip->gi_mutexview : gip->gi_view;
		if (gv == 0)
			continue;
		for (ap = gv->gv_members; !aptr_isnull(ap); ap++)
			bis(&my_bcastscope, ap->addr_site);
		for (ap = gv->gv_clients; !aptr_isnull(ap); ap++)
			bis(&my_bcastscope, ap->addr_site);
	}
}

void
pgroups_dump()
{
	register qnode *qp;
	register ginfo *gip, *pip;
	register iw_desc *iw;
	register n;

	print("\nProcess group views:\n");
	for (gip = isis_groups; gip; gip = gip->gi_next) {
		register groupview *gv = gip->gi_view;

		print("Group %s, ", gip->gi_gname);
		paddr(gip->gi_gaddr);
		if (pip = gip->gi_parent) {
			print("... plist of %s, ", pip->gi_gname);
			paddr(pip->gi_gaddr);
		}
		print("\n");
		if (gv && (gv->gv_flag & PG_VALID)) {
			print("    View %d.%d, %d members = ", VMM(gv->gv_viewid), gv->gv_nmemb);
			paddrs(gv->gv_members);
			if (gv->gv_nclient) {
				print(", %d clients = ", gv->gv_nclient);
				paddrs(gv->gv_clients);
			}
			print(", seqn: mine %d/vid %d.%d [", gip->gi_myseqn,
			      VMM(gip->gi_flushviewid));
			for (n = 0; n < gv->gv_nmemb; n++)
				print(" %d", gip->gi_bseqns[n]);
			print(" ]\n");
			if (gv = gip->gi_bypassview) {
				print
				    ("... bypass viewid %d.%d nmemb %d msgcnt %d bsndcnt %d brcvcnt %d",
				     VMM(gv->gv_viewid), gv->gv_nmemb, gip->gi_activemsg,
				     gip->gi_activebsnd, gip->gi_activebrcv);
				print("\n");
			}
			if (gv = gip->gi_bypassnew) {
				print("... next bypass viewid %d.%d nmemb %d", VMM(gv->gv_viewid),
				      gv->gv_nmemb);
				print(", flushes counted %d want %d", gip->gi_flushcount,
				      gip->gi_flushwant);
				print("\n");
			}
			if (gv = gip->gi_protosview) {
				print("... current protos viewid %d.%d nmemb %d\n",
				      VMM(gv->gv_viewid), gv->gv_nmemb);
			}
		}
		if (act_blocked == 0)
			continue;
		gv = gip->gi_mutexview;
		if (gv && (gv->gv_flag & PG_VALID)) {
			print("[act %d] %d.%d, %d members = ", act_blocked, VMM(gv->gv_viewid),
			      gv->gv_nmemb);
			paddrs(gv->gv_members);
			if (gv->gv_nclient) {
				print(", %d clients = ", gv->gv_nclient);
				paddrs(gv->gv_clients);
			}
			print("\n");
		}
	}
	for (qp = GNAMES->qu_next; qp != GNAMES; qp = qp->qu_next) {
		if (map_gaddr((address *) qp->qu_data))
			continue;
		print("Cached group address %s, ", qp->qu_string);
		paddr((address *) qp->qu_data);
		print("\n");
	}
	for (iw = iw_root; iw; iw = iw->iw_next) {
		print("isis_wait: imask [%x... ] omask [%x... ] xmask [%x... ]. Call ",
		      iw->iw_imask->bv_data[0], iw->iw_omask->bv_data[0], iw->iw_xmask->bv_data[0]);
		if (iw->iw_routine != (vfunc *) t_sig) {
			cl_rname(iw->iw_routine);
			print("(%x)\n", iw->iw_arg0);
		} else
			print("t_sig(&0x%x,0x%x)\n", iw->iw_cond, iw->iw_carg);
	}
	for (qp = isis_drainq->qu_next; qp != isis_drainq; qp = qp->qu_next) {
		print("On drain: call ");
		cl_rname(qp->qu_freeroutine);
		print("(%x)\n", qp->qu_data);
	}
	if (qu_head(time_queue)) {
		print("Timeout queue: current time %d... \n", ISIS_TIME / 1000);
		for (qp = time_queue->qu_next; qp != time_queue; qp = qp->qu_next) {
			print(" at time +%d secs, call: ", (qp->qu_time - ISIS_TIME) / 1000);
			cl_rname(qp->qu_freeroutine);
			print("(%x)\n", qp->qu_data);
		}
	}
	by_dump();
}

void
cl_del_pgroup(mp)
	register message *mp;
{
	register address *gaddr = (address *) msg_getfield(mp, CL_GID, 1, NULLIARG);

	cl_do_del_pgroup(gaddr);
}

void
cl_do_del_pgroup(gaddr)
	address *gaddr;
{
	register ginfo *gip;
	register groupview *gv;

	if ((gip = map_gaddr(gaddr)) == 0)
		return;
	if (act_blocked) {
		gv = gip->gi_mutexview;
		gip->gi_mutexview->gv_gaddr = *gaddr;
	} else
		gv = gip->gi_view;
	if (gv == 0)
		return;
	if (gv->gv_refcount != 1) {
		groupview *isis_gv_alloc();
		groupview *new_gv = gv;

		--gv->gv_refcount;
		gv->gv_flag |= PG_STALE;
		new_gv = isis_gv_alloc();
		bcopy(gv, new_gv, sizeof(groupview));
		gv = new_gv;
		if (act_blocked)
			gip->gi_mutexview = gv;
		else
			gip->gi_view = gv;
	}
	if (gv->gv_nmemb == 1)
		gv->gv_departed = gv->gv_members[0];
	else
		gv->gv_departed = NULLADDRESS;
	gv->gv_joined = NULLADDRESS;
	gv->gv_nmemb = gv->gv_nclient = 0;
	gv->gv_members[0] = NULLADDRESS;
	gv->gv_clients[0] = NULLADDRESS;
	++gv->gv_viewid;
	bypass_del_pgroup(gip);
	cl_setscope();
	pg_new_view(gv);
	if (act_blocked == 0) {
		group_unmap(gip);
		pg_unmonitor(gaddr);
	}
}

address *
pg_local_lookup(gname)
	char *gname;
{
	register ginfo *gip;

	ISIS_ENTER();
	gip = map_gname(gname);
	if (gip)
		ISIS_RETURN(gip->gi_gaddr);
	ISIS_RETURN(&NULLADDRESS);
}

/* Get view only if known locally */
groupview *
pg_getlocalview(gaddr)
	address *gaddr;
{
	register ginfo *gip;

	ISIS_ENTER();
	gip = map_gaddr(gaddr);
	if (gip) {
		register groupview *gv = act_blocked ? gip->gi_mutexview : gip->gi_view;

		if (gv && (gv->gv_flag & PG_VALID))
			ISIS_RETURN(gv);
	}
	ISIS_RETURN((groupview *) 0);
}

static sys_groupview sys_pg;

groupview *
pg_getview(gaddrp)
	address *gaddrp;
{
	register message *msg;
	static groupview pgview, *gv = 0;
	address gaddr;

	ISIS_ENTER();
	gaddr = *gaddrp;
	gaddr.addr_entry = 0;
	if (gv = pg_getlocalview(&gaddr))
		ISIS_RETURN(gv);
	if (addr_isplist(gaddrp))
		ISIS_RETURN(gv);
	msg = msg_genmsg(CL_GID, (char *) &gaddr, FTYPE_ADDRESS, sizeof(address), 0);
	if (isis(CL_GETVIEW, msg, &sys_pg, sizeof(sys_groupview)) > 0) {
		gv = &pgview;
		gv->gv_flag = 0;
		(void) isis_pg_copy(&sys_pg, gv);
		gv->gv_flag &= ~(PG_VALID | PG_FIRST);
		gv->gv_flag |= PG_CACHED;
	}
	msg_delete(msg);
	ISIS_RETURN(gv);
}

int
_pg_rank(gaddr, who)
	address *gaddr, *who;
{
	register groupview *gv;
	register address *ap;

	ISIS_ENTER();
	if ((gv = pg_getview(gaddr)) == 0) {
		isis_errno = IE_UNKNOWN;
		ISIS_RETURN(-1);
	}
	for (ap = gv->gv_members; !aptr_isnull(ap); ap++)
		if (addr_cmp(ap, who) == 0)
			ISIS_RETURN(ap - gv->gv_members);
	isis_errno = IE_NOTMEMB;
	ISIS_RETURN(-1);
}

int
_pg_rank_all(gaddr, who)
	address *gaddr, *who;
{
	register groupview *gv;
	register address *ap;

	ISIS_ENTER();
	if ((gv = pg_getview(gaddr)) == 0) {
		isis_errno = IE_UNKNOWN;
		ISIS_RETURN(-1);
	}
	for (ap = gv->gv_members; !aptr_isnull(ap); ap++)
		if (addr_cmp(ap, who) == 0)
			ISIS_RETURN(ap - gv->gv_members);
	for (ap = gv->gv_clients; !aptr_isnull(ap); ap++)
		if (addr_cmp(ap, who) == 0)
			ISIS_RETURN(gv->gv_nmemb + (ap - gv->gv_clients));
	isis_errno = IE_NOTMEMB;
	ISIS_RETURN(-1);
}

/* Build a groupview from this pgroup, using old information if any */
int
isis_pg_copy(pg, gv)
	register sys_groupview *pg;
	register groupview *gv;
{
	register address *ap, *bp;
	register rv = -1;
	register vid = pg->pg_viewid;

	vid = VIEWID(vid, 0);
	if (gv->gv_flag & PG_VALID) {
		if (gv->gv_viewid >= vid)
			return (-1);
		gv->gv_departed = NULLADDRESS;
		gv->gv_joined = NULLADDRESS;
		ap = pg->pg_alist;
		bp = gv->gv_members;
		while (!aptr_isnull(bp) && !aptr_isnull(ap) && addr_cmp(ap, bp) == 0) {
			++ap;
			++bp;
			continue;
		}
		if (aptr_isnull(bp))
			gv->gv_joined = *ap;
		else {
			gv->gv_departed = *bp;
			rv = bp - gv->gv_members;
		}
	} else {
		gv->gv_gaddr = pg->pg_gid;
		bcopy(pg->pg_name, gv->gv_name, PG_GLEN);
		gv->gv_departed = NULLADDRESS;
		gv->gv_joined = NULLADDRESS;
		if (addr_ismine(&pg->pg_alist[pg->pg_nmemb - 1]))
			gv->gv_joined = my_address;
		else
			gv->gv_joined = NULLADDRESS;
		gv->gv_flag |= PG_VALID | PG_FIRST;
	}
	gv->gv_nmemb = pg->pg_nmemb;
	gv->gv_nclient = pg->pg_nclient;
	gv->gv_incarn = pg->pg_incarn;
	gv->gv_viewid = vid;
	ap = pg->pg_alist - 1;
	bp = gv->gv_members;
	do
		*bp++ = *++ap;
	while (!aptr_isnull(ap));
	bp = gv->gv_clients;
	do
		*bp++ = *++ap;
	while (!aptr_isnull(ap));
	return (rv);
}

void
cl_rcv_reply(mp)
	register message *mp;
{
	register msgid = msg_getid(mp);
	register qnode *qp;
	extern isis_ignored_replies, isis_ignored_nullreplies;

	if (isis_state & ISIS_TRACE)
		print("cl_rcv_reply: received reply to message %d\n", msgid);
	if (isis_tasks->qu_next != isis_tasks)
		for (qp = isis_tasks->qu_next; qp != isis_tasks; qp = qp->qu_next) {
			register task *tp = qp->qu_task;

			if (tp->task_msgid == msgid && tp->task_mwant) {
				msg_increfcount(mp);
				t_sig_urgent(&tp->task_mwant, (char *) mp);
				return;
			}
		}
	if (msg_getfield(mp, FLD_ISNULLREP, 1, (int *) 0) == 0)
		++isis_ignored_replies;
	else {
		++isis_ignored_replies;
		++isis_ignored_nullreplies;
	}
}

address *
_PRO(entry)
	int entry;
{
	static address addr;

	addr.addr_site = my_site_no;
	addr.addr_incarn = my_site_incarn;
	addr.addr_process = PROTOCOLS;
	addr.addr_entry = entry;
	addr.addr_portno = 0;
	return (&addr);
}

void
panic(fmt, a0, a1, a2, a3, a4, a5)
	char *fmt;
	int a0, a1, a2, a3, a4, a5;
{
#ifdef  UNIX_DOM
	char clfile[32];

	sprintf(clfile, "/tmp/Cl%d", my_process_id);
	unlink(clfile);
#endif

#if ALLEGRO_CL || LUCID_CL
	{
		char mess[100];

		sprintf(mess, fmt, a0, a1, a2, a3, a4, a5);
#if ALLEGRO_CL
		call_allegro(cl->panic, mess);
#endif
#if   LUCID_CL
		cl_panic(mess);
#endif
		return;
	}
#endif				/* ALLEGRO_CL || LUCID_CL */

	switch (my_process_id) {
	default:
		if (my_process_id & PID_REMOTE)
			print("ISIS client pid=%d: ", my_process_id & (short) ~PID_REMOTE);
		else
			print("ISIS client pid=%d: ", my_process_id);
		break;
	case REXEC:
		print("ISIS rexec service: ");
		break;
	case ISIS:
		print("ISIS site-monitor service: ");
		break;
	}
	print(fmt, a0, a1, a2, a3, a4, a5);
	print("\n");
	if (!isis_dontlogdumps
	    && (isis_state & (ISIS_ISUP | ISIS_TRACE)) == (ISIS_ISUP | ISIS_TRACE))
		cl_dump(DUMP_ALL, "PANIC");
	exit(1);
}

#if ALLEGRO_CL || LUCID_CL
lisp_exit(n)
	int n;
{
	t_final();
	/* Probably lots of other things to do... */
}
#endif				/* ALLEGRO_CL || LUCID_CL */

adesc iad = { sizeof(address), 0, 16 };

#define iaddr_alloc    (address)mallocate(&iad)

/* Returns struct-by-ptr: GCC differs in how it does by value from CC */
address *
_ADDRESS(s, i, p, e)
	site_id s;
	int i, p, e;
{
	static address d;

	d.addr_site = s;
	d.addr_incarn = i;
	d.addr_process = p;
	d.addr_entry = e;
	d.addr_portno = 0;
	return (&d);
}

int
_isnull(a)
	register address *a;
{
	return (a == 0 || a->addr_site == 0);
}

int
_cmp(a, b)
	register address *a, *b;
{
	if (a == 0)
		return (!addr_isnull(b));
	else if (b == 0)
		return (!addr_isnull(a));
	if (a->addr_site != b->addr_site)
		return (a->addr_site - b->addr_site);
	if (!addr_ispid(a) || !addr_ispid(b)) {
		if (a->addr_groupid != b->addr_groupid)
			return (a->addr_groupid - b->addr_groupid);
	} else if (a->addr_process != b->addr_process)
		return (a->addr_process - b->addr_process);
	if (a->addr_portno != b->addr_portno)
		return (a->addr_portno - b->addr_portno);
	if (a->addr_incarn != b->addr_incarn)
		return (a->addr_incarn - b->addr_incarn);
	if (a->addr_entry == 0 || b->addr_entry == 0)
		return (0);
	return (a->addr_entry - b->addr_entry);
}

void
pr_dump(how)
	int how;
{
	int answ;
	register message *mp;

	ISIS_ENTER();
	mp = msg_genmsg(CL_HOW, (char *) &how, FTYPE_LONG, sizeof(how), 0);
	isis(CL_WANTDUMP, mp, &answ, 1);
	msg_delete(mp);
	ISIS_EXIT();
}

void
pr_rescan()
{
	register message *mp = msg_newmsg();
	int answ;

	isis(PR_RESCANSITES, mp, &answ, 1);
	msg_delete(mp);
}

void
pr_shutdown()
{
	register message *mp = msg_newmsg();
	int answ;

	++shutting_down;
	isis(CL_SHUTDOWN, mp, &answ, 1);
	msg_delete(mp);
}

void
pr_makedump(file)
	char *file;
{
	register message *mp;
	int answ, how = 0;

	mp = msg_genmsg(CL_FILE, file, FTYPE_CHAR, strlen(file) + 1, 0);
	msg_insertfield(mp, CL_HOW, (char *) &how, FTYPE_LONG, sizeof(how));
	isis(CL_WANTDUMP, mp, &answ, 1);
	msg_delete(mp);
}

address *
set_entry(alist, entry)
	register address *alist;
	register entry;
{
	while (!aptr_isnull(alist))
		alist++->addr_entry = entry;
	return (alist);
}

void
isis_perror(str)
	char *str;
{
	register e = -isis_errno;

	if (e <= 0 || e >= sizeof(IE_errors) / sizeof(*IE_errors))
		print("%s: isis errno %d not defined!", str, e);
	else
		print("%s: %s\n", str, IE_errors[e]);
}

#undef  sleep
int sleep();

unsigned
isis_sleep(n)
	unsigned n;
{
	if ((isis_state & ISIS_ISUP) == 0 || isis_ctp == isis_scheduler || isis_ctp == 0
	    || isis_scheduler == 0) {
		sleep(n);
		return (0);
	}
	return (isis_sleep_ms(n * 1000));
}

unsigned
isis_sleep_ms(ms)
	int ms;
{
	condition *wake_me_up;

	ISIS_ENTER();
	if (ms <= 0)
		ISIS_RETURN(-1);
	/* Sleep from a task */
	isis_ctp->task_sleep = ms;
	wake_me_up = (condition *) malloc(sizeof(condition));
	*wake_me_up = 0;
	/* Sleep from a task */
	isis_timeout(ms, (vfunc *) t_sig, (VOID *) wake_me_up, NULLARG);
	t_wait_l(wake_me_up, "isis system: sleep");
	free(wake_me_up);
	isis_ctp->task_sleep = 0;
	ISIS_RETURN(0);
}

int isis_gt_calls;

set_isis_time()
{
	register ns;

	++isis_gt_calls;
	gettimeofday(&time_0, (struct timezone *) 0);
	if (ns = time_0.tv_sec - last_gettimeofday.tv_sec) {
		ISIS_TIME += 1000 * ns;
		last_gettimeofday = time_0;
		return (1);
	}
	return (0);

}

/* Specifies a timeout in milliseconds */
int TIMEOUT_ID;

int
isis_timeout(time, routine, arg0, arg1)
	int time;
	void (*routine) ();
	VOID *arg0, *arg1;
{
	return (isis_timeout_reschedule(0, time, routine, arg0, arg1));
}

int
isis_timeout_reschedule(oldtid, time, routine, arg0, arg1)
	register time, oldtid;
	void (*routine) ();
	VOID *arg0, *arg1;
{
	register qnode *qp;

	ISIS_ENTER();
	time += ISIS_TIME;
	if ((vfunc *) routine == (vfunc *) intercl_sweep) {
		/* Round up to next second */
		time += 999;
		time -= time % 1000;
	}
	if (oldtid)
		for (qp = time_queue->qu_next; qp != time_queue; qp = qp->qu_next)
			if (qp->qu_timeargs[2] == (char *) oldtid) {
				if (qp->qu_time <= time)
					ISIS_RETURN(oldtid);
				qp->qu_freeroutine = NULLROUTINE;
				qu_free(qp);
				break;
			}
	for (qp = time_queue->qu_next; qp != time_queue; qp = qp->qu_next)
		if (qp->qu_time >= time)
			break;
	qp = qu_add(qp, time, NULLARG, routine);
	qp->qu_timeargs[0] = arg0;
	qp->qu_timeargs[1] = arg1;
	qp->qu_timeargs[2] = (char *) ++TIMEOUT_ID;
	ISIS_RETURN(TIMEOUT_ID);
}

void
isis_timeout_cancel(oldtid)
	int oldtid;
{
	register qnode *qp;

	ISIS_ENTER();
	for (qp = time_queue->qu_next; qp != time_queue; qp = qp->qu_next)
		if (qp->qu_timeargs[2] == (char *) oldtid) {
			qp->qu_freeroutine = NULLROUTINE;
			qu_free(qp);
			break;
		}
	ISIS_EXIT();
}

/* User callable address manipulation routines which make addresses
   "persistent"
*/
address *
save_address(addr)
	address *addr;
{
	register qnode *qp;
	extern qnode *ADDRS;

	if ((qp = pg_find(ADDRS, addr)) == 0) {
		/* Add to canonical list */
		qp = pg_add(ADDRS, addr, NULLARG, NULLROUTINE);
		qp->qu_pname.addr_entry = addr->addr_entry;
		/* "Feature" of pg_add is that it zeros entry! */
	} else {
		/* Check that we didn't compare "equal" to an entry no of zero */
		if (qp->qu_pname.addr_entry != addr->addr_entry) {
			address *addr1;

			addr1 = (address *) malloc(sizeof(address));
			*addr1 = *addr;
			return (addr1);
		}
	}
	return &qp->qu_pname;
}

address *
make_address(site, inc, pro, ent)
	int site, inc, pro, ent;
{
	address addr;

	addr = ADDRESS(site, inc, pro, ent);
	return save_address(&addr);
}

/* Real routine versions of some address macros for calls from LISP */
#undef	aptr_isnull
#undef  addr_ismine
#undef  addr_isequal

int
aptr_isnull(ad)
	address *ad;
{
	return (ad == 0 || ad->addr_site == 0);
}

int
addr_isequal(a1, a2)
	address *a1, *a2;
{
	return (addr_cmp(a1, a2) == 0);
}

int
addr_ismine(addr)
	address *addr;
{
	if (addr == 0)
		return (0);
	return (addr_cmp(addr, &my_address) == 0);
}

#if ALLEGRO_CL || LUCID_CL

static long mask_bits;
static bitvec in_mask, out_mask, exc_mask;
static long sel_in_use, sel_errno, sel_result;
struct timeval sel_timer;

int
lisp_select(m_bits, in_m, out_m, exc_m, timer)
	register long m_bits;
	register bitvec *in_m, *out_m, *exc_m;
	register struct timeval *timer;
{
	static in_use;

	/* If we're just polling then do it directly instead of switching. */
	if (timer && timer->tv_sec == 0 && timer->tv_usec == 0) {
		return (select(m_bits, in_m, out_m, exc_m, timer));
	}

	if (in_use) {
		panic("lisp_wait_with_select already in use!");
	}
	in_use = TRUE;

	/* Copy arguments to static variables to avoid stack references */
	mask_bits = m_bits;
	if (in_m) {
		in_mask = *in_m;
	} else {
		bclr(&in_mask);
	}

	/* Set up time we want to return from select. */
	gettimeofday(&sel_timer, (struct timezone *) 0);
	last_gettimeofday = sel_timer;
	if (timer) {
		sel_timer.tv_sec += timer->tv_sec;
		sel_timer.tv_usec += timer->tv_usec;
	} else
		sel_timer.tv_sec += 100000;

#if ALLEGRO_CL
	call_allegro(cl->thread_wait_fun, 0);
#endif
#if   LUCID_CL
	cl_thread_wait_fun();
#endif
	if (in_m) {
		*in_m = in_mask;
	}
	if (out_m) {
		*out_m = out_mask;
	}
	if (exc_m) {
		*exc_m = exc_mask;
	}

	if (sel_result < 0)
		errno = sel_errno;
	in_use = FALSE;
	return (sel_result);
}

/* TRUE if runnable, FALSE if not */
isis_select_from_lisp()
{
	extern thread_wants_entry;
	struct timeval timer;
	bitvec in1, out1, exc1;

	/* Use local copy of masks since they get modified */
	/* by the select call. */

	/* First check whether a thread is waiting to enter ISIS. */
	if (thread_wants_entry) {
		/* Effectively this looks like a timeout on the select. */
		bclr(&in_mask);
		bclr(&out_mask);
		bclr(&exc_mask);
		sel_result = 0;
		return (TRUE);
	}

	in1 = in_mask;
	out1 = out_mask;
	exc1 = exc_mask;
	sel_result = select(mask_bits, &in1, &out1, &exc1, &sel_poll);
	if (sel_result < 0) {
		perror("lisp select ");
		sel_errno = errno;
	}

	if (sel_result > 0 || sel_result == -1) {
		in_mask = in1;
		out_mask = out1;
		exc_mask = exc1;
		return (TRUE);
	}

	gettimeofday(&timer, (struct timezone *) 0);
	last_gettimeofday = timer;
	if (timer.tv_sec > sel_timer.tv_sec ||
	    (timer.tv_sec == sel_timer.tv_sec && timer.tv_usec >= sel_timer.tv_usec)) {
		bclr(&in_mask);
		return (TRUE);
	}

	return (FALSE);
}
#endif

#ifdef  FORK_BROKEN
#define ISIS_EXEC     1

/* Specifically for Apollo; see util/isis.c */
int
isis_fork_execve(program, argp, envp, fdes, fd1, fd2)
	char *program;
	char **argp, **envp;
	int fdes, fd1, fd2;
{
	register char *s, *as;
	char str[2048];
	address iaddr;
	int pid;

	s = str;
	while (*s++ = *program++)
		continue;
	s[-1] = ' ';
	while (as = *argp++) {
		while (*s++ = *as++)
			continue;
		s[-1] = ' ';
	}
	s[-1] = ';';
	if (envp && *envp) {
		while (as = *envp++) {
			while (*s++ = *as++)
				continue;
			s[-1] = ' ';
		}
		s[-1] = '\n';
	} else
		*s++ = '\n';
	*s = 0;
	if (my_process_id != ISIS) {
		iaddr = my_address;
		iaddr.addr_process = ISIS;
		cbcast(&iaddr, ISIS_EXEC, "%s", str, 1, "%d", &pid);
	} else
		pid = ISIS_fork_execve(str);
	return (pid);
}
#else
/* For everyone else */
/* VARARGS */
int
isis_fork_execve(program, argp, envp, fd0, fd1, fd2)
	char *program, **argp, **envp;
	int fd0, fd1, fd2;
{
	register pid;

	if (pid = isis_dofork())
		return (pid);
	if (fd0 != -1) {
		if (fd0)
			close(fd0);
		if (fd1 != -1) {
			if (fd1)
				close(fd1);
			if (fd2 > 0)
				close(fd2);
		}
	}
	if (envp != (char **) 0)
		execve(program, argp, envp);
	else
		execvp(program, argp);
	fprintf(stderr, "ISIS: unable to exec <%s>\n", program);
	exit(0);
}
#endif

set_isis_sockopts(sock)
	register sock;
{
	int retval, ioarg;

#       ifdef HPUX
	{
#           ifdef SO_SNDBUF
		{
			int size = 8 * 1024;

			setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &size, sizeof(size));
			size = 8 * 1024;
			setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &size, sizeof(size));
		}
#           endif SO_SNDBUF
#           ifdef SO_BURST_OUT
		{
			int val = 7;

			setsockopt(sock, SOL_SOCKET, SO_BURST_OUT, &val, sizeof(val));
		}
#           endif SO_BURST_OUT

		/* This ioctl call eliminates the blocking done by HPUX sendto command */
		ioarg = 1;
		retval = ioctl(sock, FIOSNBIO, &ioarg);
		if (retval == -1)
			perror("ioctl:");
	}
#       else HPUX
	{
#           ifdef SO_SNDBUF
		{
			int size = 16 * 1024;

			setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &size, sizeof(size));
			size = 16 * 1024;
			setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &size, sizeof(size));
		}
#           endif SO_SNDBUF
	}
#       endif HPUX
}
