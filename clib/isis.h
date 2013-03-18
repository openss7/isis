/*  $RCSfile: isis.h,v $ $Revision: 2.104 $ $Date: 90/09/12 13:25:55 $  */
/*
 *	OriGinally coded by Ken Birman
 *      Generic include file for client code
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
 */

#ifndef CL_ISIS
#define CL_ISIS

/* If you plan to include <stdarg.h>, must appear BEFORE isis.h... */
#if hp9000s800
#ifndef va_start
     /* Missing on some releases of HPUX */
typedef double *va_list;

#    define va_dcl long va_alist;
#    define va_start(ap) ap = (va_list)((long)(&va_alist)+sizeof(va_alist))

#    define va_arg(ap,type) ((type *)(ap =              \
            (va_list) ((long)((char *)ap-sizeof(type)) & ~(sizeof(type)-1))))[0]
#    define va_end(ap)
#endif				/* va_start */
#else
#    ifndef va_start
#        include    <varargs.h>
#    endif
#endif

#include <errno.h>
#include <stdio.h>

/* Use builtin defines to infer machine types for some machines. */
#if (SUN || SUN3 || SUN4 || APOLLO || VAX || MACH || HPUX || GOULD || RT43 || AUX)
#else
#  ifdef  hpux
#    define HPUX               1
#  endif
#  if     (m68k || mac2)
#    define AUX                1
#  endif
#  ifdef   sun
#    ifdef sparc
#      define SUN4             1
#    else
#      define SUN3             1
#    endif
#  endif
#endif

/* Decide whether to use function prototypes (ANSI C and C++) or 
   old style function declarations. Not all "ansi" compilers are the same,
   especially when it comes to mixing new function prototypes with old-style
   function definitions, which is not defined in the standard. 
   Should match same defines done in msg.h and pr.h.
*/
#ifdef __STDC__
#  ifndef APOLLO
#    define FUN_TYPES  1
#  endif
#endif				/* __STDC__ */

#if ( __cplusplus || c_plusplus )
#  define FUN_TYPES    1
#endif

#if ( __cplusplus || c_plusplus )
#ifdef __cplusplus
extern "C" {
	void perror(const char *);
}
#else
void perror(const char *);
#endif
typedef void (*v_routine) ();
typedef void (*v_routine_vs) (void *);
typedef int (*i_routine_vs) (void *);
#endif

/* UNIX_DOM must match with pr.h! */
#ifdef  SUN3
# define SUN                1
#endif				/* SUN3 */

#ifdef  SUN4
# define SUN                1
#endif				/* SUN4 */

#ifdef SUN
#  define UNIX_DOM          1

/* Which task mechanism? */
#  if ALLEGRO_CL || LUCID_CL
#      define exit(n)        lisp_exit(n)
#  else
#    ifdef SUNLWP
#      define exit(n)        pod_exit(n)
#      if __GNUC__
volatile pod_exit(int n);
#      endif
#    endif SUNLWP
#  endif
#endif				/* SUN */

#ifdef  AUX
# define UNIX_DOM           1
#endif				/* AUX */

#ifdef  APOLLO
# define UNIX_DOM           1
# define exit(n)            kill(getpid(), 9)
#endif				/* APOLLO */

#ifdef  VAX
# define UNIX_DOM           1
#endif				/* VAX */

#ifdef  MACH
# define CTHREADS           1
# define UNIX_DOM           1
#endif				/* MACH */

#ifdef MIPS
# define UNIX_DOM           1
/* JB_SP  defined in jmpbuf.h */
#endif				/* MIPS */

#ifdef CONVEX
# define UNIX_DOM	1
#endif				/* CONVEX */

#ifdef SGI
# define UNIX_DOM           1
/* JB_SP defined in setjmp.h */
#endif				/* SGI */

#ifdef RT43
# define UNIX_DOM           1
# define JB_SP              0
#endif				/* MIPS */

#ifdef AIX			/* AIX on old PC/RT */
# define JB_SP              10
# define SIMWRITEV           1
#endif				/* AIX */

#ifdef AIXRS			/* AIX on RS/6000 */
# define JB_SP              3
# define UNIX_DOM           1
#endif				/* AIXRS */

#ifdef NO_UNIX_DOM
#  ifdef UNIX_DOM
#    undef UNIX_DOM
#  endif UNIX_DOM
#endif				/* NO_UNIX_DOM */

#ifdef HPUX
#include <memory.h>
#define bcopy(a,b,c) memcpy(b,a,c)
#define bzero(a,b)   memset(a,0,b)
#endif				/* HPUX */

#if    	(AIX|AIXRS|HPUX|SUN|VAX|SGI|MIPS|AUX)
#   define isis_dofork()    fork()
#else
#   define isis_dofork()    vfork()
#endif				/* (AIX|AIXRS|HPUX|SUN|VAX|SGI|MIPS|AUX) */

#define MAX_PROCS       64
#define MAX_SITES       127	/* Also change in pr.h! */

/* This line is to make SITE_IS_UP work in client code... */
#define current_view    isis_sv

#include "cl_typedefs.h"
#ifdef LARGE_GROUPS
#include "cl_hashtab.h"
#endif
#include "msg.h"
#include "isis_alloc.h"
#include "bits.h"
#include "pr_fdect.h"
#include "pr_msgfields.h"
#include "pr_address.h"
#include "tk_xaction.h"
#include "cl_queues.h"
#include "cl_task.h"
#include "pr_client.h"
#include "pr_pgroups.h"
#include "cl_groupview.h"
#include "pr_errors.h"
#include "tk_news.h"
#include "tk_rmgr.h"
#include "cl_cmd.h"
#include "tk_lmgr.h"
#include "cl_inter.h"
#include "cl_bypass.h"
#include "cl_coord.h"
#include "cl_token.h"
#include "cl_plist.h"
#include "cl_bcast.h"
#include "cl_dump.h"
#include "cl_sview.h"
#include "tk_connect.h"
#ifdef LARGE_GROUPS
#include "cl_lgroup.h"
#include "cl_gvs.h"
#include "cl_lglookup.h"
#endif

#if ( __cplusplus || c_plusplus )
typedef void (*v_routine_msgs) (message * m);
#endif
typedef void (*v_proc) ();

#ifndef GLOBAL
#define GLOBAL
/* Client's version of various global definitions */

#       define  begin
#       define  forever         for(;;)

/* Used in conjunction with varargs */
#       define  BEGINFROMC            { int state = isis_state; isis_state &= ~ISIS_XBYREF;
#       define  ENDFROMC              isis_state |= state&ISIS_XBYREF; }
#       define  BEGINFROMFORTRAN      { int state = isis_state; isis_state |= ISIS_XBYREF;
#       define  ENDFROMFORTRAN        isis_state &= ~ISIS_XBYREF; isis_state |= state&ISIS_XBYREF; }

/* VA_REF: always comes in as a ref.  VA_ARG: comes in as a ref only if from fortran */
#       define  VA_REF(ap, type)      va_arg(ap,type)
#       define  VA_ARG(ap, type)      ((isis_state&ISIS_XBYREF)?*va_arg(ap,type*):va_arg(ap,type))

#define DUMP_ALL        -1
#define DUMP_MEM        0x1

/* Argument types */
#define PG_INIT         1	/* Init routine to call on creates */
#define PG_XFER         2	/* Xfer routines, can specify several */
#define PG_JOIN_AUTHEN  3	/* Routine to authenticate join requests */
#define PG_MONITOR      4	/* Routine that monitors this group */
#define PG_CREDENTIALS  5	/* Credentials for this join */
extern PG_LOGGED;			/* Used for tricky linking! */

#define _PG_LOGGED      6	/* Special case... Log this group on stable storage */
#define PG_DONTCREATE   7	/* Don't create if it doesn't exist */
#define PG_BIGXFER      8	/* Expect a big state */
#define PG_INCARN       9	/* Specify incarnation number */
#define PG_CLIENT_AUTHEN 10	/* Client authentication */
#define PG_REPLAYSPOOL  11	/* Says that we want any spooled messages replayed */
#define PG_LOGPARAMS    12	/* Specify extra logmanager parameters */
#define PG_DIFFUSION    13	/* Diffusion group (clients recv. only) */

#define NULLROUTINE     (vfunc*)        0
#define NULLARG         (void*)         0
#define NULLTASK        (task*)         0
#define NULLIARG        (int*)          0
#define NULLQP          (qnode*)        0
#define NULLMP          (message*)      0
#define NULLSID         (site_id)       0

#define VIEWID(a,b)     ((a<<16)|b)
#define VMAJOR(a)       ((int)(((short)(a>>16))&(short)0xFFFF))
#define VMINOR(a)       ((int)(((short)a)&(short)0xFFFF))
#define VMM(a)          VMAJOR(a),VMINOR(a)

extern int my_site_no;			/* Info about this site */
extern int my_site_incarn;		/* Info about this site */
extern site_id my_site_id;		/* This process's id */
extern int my_process_id;		/* Info about this site */
extern int my_port_no;			/* UDP port to talk to me directly */
extern int my_genid;			/* Used to generate bcast id's */
address my_address;			/* Info about this site */
char my_host[64];			/* My host name */
extern bitvec my_bcastscope;		/* Scope, for piggybacking */
char site_names[MAX_SITES][64];		/* All known host names */
extern address NULLADDRESS;
extern int errno;			/* System version */
extern char *isis_dir;			/* isis working directory */
extern int isis_errno;			/* See pr_errors.h */
extern int isis_nsent;			/* number of destinations sent to */
extern int isis_nreplies;		/* number of replies received */
extern int act_blocked;			/* When tasks are blocked, this says who did it */
extern int isis_forkcnt;		/* Counts tasks forked off */
extern int isis_socket;			/* Socket (fdes) for talking to isis */
extern int intercl_socket;		/* Socket (fdes) for receiving directly client-client */
extern int isis_state;			/* State flags */
extern int isis_created;		/* Tasks created */
extern int isis_switched;		/* Task switches done */
extern int isis_nblock;
extern int isis_ncongest;
extern int bypass_lagging;		/* Internal view lagging behind bypass view */
extern qnode *bypass_waitq;		/* Waiting for a join to finish */

extern qnode *ADDRS;			/* Storage for addresses */
extern qnode *GNAMES;			/* Storage for addresses */
extern ISIS_TIME;			/* relative timer in ms */

/* For talking to the ISIS monitoring program */
#define ISIS_M_QUERY         1	/* Hello? */
#define ISIS_M_KILL          2	/* Kill yourself */
#define ISIS_M_REPLY         3	/* Reply to QUERY */
#define ISIS_M_REMOTE	     4	/* Remote connection request */
#define ISIS_M_ACK	     5	/* Reply to ISIS_M_REMOTE */
#define ISIS_M_LOOKUP        6	/* Lookup net addr */

/* Entry points */
#define ISIS_EXEC          1	/* Only if fork is broken */

/* State bits, some used as arguments to isis_init_l */
#define ISIS_INIT       0x00000001	/* Initialization complete */
#define ISIS_ISUP       0x00000002	/* Connected to a live protos */
#define ISIS_CONGESTED  0x00000004	/* ISIS is congested at this site */
#define ISIS_STARTUP    0x00000008	/* Doing startup */
#define ISIS_XJOINS     0x00000010	/* Joins inhibited */
#define ISIS_TRACE      0x00000020	/* Trace enabled */
#define ISIS_WJOIN      0x00000040	/* Someone wants to join */
#define ISIS_PBUFDIRTY  0x00000080	/* Pbuf believed to be nonempty */
#define ISIS_RECEIVING  0x00000100	/* Used from isis_gotmsg() only */
#define ISIS_FBLOCK     0x00000200	/* Blocked while flushing bypass code */
#define ISIS_XBYREF     0x00000400	/* Pushes everything by ref.  KNOWN BY VALUE IN MSG.C */
#define ISIS_DRAINPBUF  0x00000800	/* Waiting to drain pbuf during noblocking bcast */
#define ISIS_TINIT      0x00001000	/* Task package init was done */
#define ISIS_LEAVING    0x00002000	/* Leaving a process group */
#define ISIS_COORD      0x00004000	/* Coordinator is active (inhibits pg_leave) */
#define ISIS_WANTACK    0x00008000	/* I have an ACK to send */
#define ISIS_REMOTE     0x00010000	/* isis_remote */
#define ISIS_PANIC      0x00020000	/* Panic if restart fails */
#define ISIS_AUTOSTART  0x00040000	/* Start ISIS if connection fails */
#define ISIS_OPTIMISTIC 0x00080000	/* Don't wait for bypass termination */

#ifdef	ISIS_SNAMES
char *isis_snames[32] = {
	0, "isup", "congested", "startup", "xjoins",
	0, "wjoin", "pbuf-dirty", "receiving", "block",
	"xbyref", "drainpbuf", 0, "leaving", "coord-active",
	"wantack", "remote-client", 0, 0, "optimistic",
};
#endif

/* For isis_accept_events */
#define ISIS_ASYNC      0	/* Don't block waiting for an event */
#define ISIS_BLOCK      1	/* Wait for an event */
#define ISIS_TIMEOUT    2	/* Wait with timeout */

extern sview isis_sv;			/* Site-views */
extern sview isis_svmutex;		/* Site-views */
extern qnode *isis_wlist;		/* Process group member watch list */
extern qnode *isis_swlist;		/* Site watch list */
extern qnode *isis_pwlist;		/* Process watch list */
extern qnode *isis_pgmon;		/* Pgroup monitor list */
extern char *isis_joining;		/* Group this client is joining, if any */
extern int isis_enum;			/* Used when isis_gotmsg() is delivery message */
extern ginfo *isis_gip;			/* " " */
extern event_id *isis_eid;		/* " " */
extern task *isis_scheduler;		/* Task for scheduling tasks */
extern bitvec inhibit_entries;		/* Entries for which joins should be inhibited */

typedef address *afunc();

#define XD_MAX          10	/* Upper limit */

struct ginfo {
	address *gi_gaddr;		/* Group address */
	ginfo *gi_next;			/* Forward link */
	char gi_gname[PG_GLEN];		/* Printable name */
	vfunc *gi_xfer_rcv[XD_MAX];	/* For state xfer */
	vfunc *gi_xfer_gen[XD_MAX];	/* For state xfer */
	ifunc *gi_join_verifier;	/* For verifying joins */
	ifunc *gi_client_verifier;	/* For verifying client */
	long gi_xbyref;			/* For each domain: XBYREF? */
	groupview *gi_view;		/* Current view */
	groupview *gi_mutexview;	/* Current view when act_blocked set */
	groupview *gi_bypassnew;	/* View we are switching to in bypass code */
	groupview *gi_bypassview;	/* View used in bypass code */
	groupview *gi_protosview;	/* View used in bypass code */
	condition gi_bypasswait;	/* Used when waiting in bypass_checkview */
	log_nod *gi_lnode;		/* Logging information for this group, if any */
	bitvec gi_lentries;		/* Logged entries */
	int gi_myseqn;			/* My seqn number for gcollect of sends */
	int gi_bcseqn;			/* My seqn number for avoiding extra terminate msgs */
	int gi_nextseqn;		/* My next sequence number */
	int gi_bseqns[PG_ALEN];		/* seqn for direct communication */
	int gi_flushviewid;		/* Viewid during flush */
	short gi_flushcount;		/* Countdown during flush */
	short gi_flushwant;		/* Counts flush messages wanted */
	short gi_activemsg;		/* Counts active point-to-point messages */
	short gi_activebsnd;		/* Counts active bcasts */
	short gi_activebrcv;		/* Counts active bcasts received */
	qnode *gi_bypassq;		/* Bypass qnode */
	qnode *gi_clwatchq;		/* Join watch qnode */
	qnode *gi_aborder;		/* Waiting for abcast delivery order info */
	qnode *gi_byprocs;		/* Retransmission qnode */
	int gi_transport;		/* Transport protocol to use for this group */
	ginfo *gi_parent;		/* For use with plists */
	int gi_mid;			/* Plist monitoring id */
	int gi_flag;			/* Set if ab_flushorder() already forked */
	qnode *gi_watching;		/* Processes watching this process in this group */
	message *gi_blockmsg;		/* Message that triggered flush */
};

#define	GI_ABFORKED    0x0001	/* Set if ab_flushorder() already forked */
#define	GI_FLUSHING    0x0002	/* Set if flushing this group */
#define GI_DIFFUSION   0x0004	/* Set for diffusion groups */
#define GI_INUSE       0x0008	/* Set when group structure is in use */

extern ginfo *isis_groups;
vfunc *def_xfer_rcv[XD_MAX];		/* Default */
vfunc *def_xfer_gen[XD_MAX];		/* Default */

/* For coordinator-cohort computations */
#define ORIGINAL        0	/* You are the original coordinator */
#define TAKEOVER        1	/* You are a cohort taking over */

#define XD_TOKEN        1	/* For token tool */
#define XD_RESERV2      2	/* For future use by system */
#define XD_RESERV3      3	/* For future use by system */
#define XD_USER         4	/* User ``domain'' */

#define W_FAIL          0	/* Watch for failure */
#define W_RECOVER       1	/* Watch for recovery */
#define W_JOIN          2	/* Process joins */
#define W_LEAVE         3	/* Process leaves */
#define W_MONITOR       -1	/* Monitor */

#define ACT_MSG         1	/* One more or fewer messages */
#define ACT_WATCH       2	/* One more or fewer watch/monitor */

#define BYP_DONTCHECK   0	/* Don't check for bypass info */
#define BYP_CHECK       1	/* Check for bypass info */
#define BYP_REPEAT      2	/* For call-backs from cl_bypass */

extern act_bits;			/* Activities currently in use (initially 1 in cl_isis) */

#define ACT_VALID(act)  (act_bits & (1<<act))

#define ACT_WILDCARD    0x8000000
#define ACT_BITS        ((1<<MAX_ACT)-1)
#define MAX_ACT         32	/* Limited by bits in integer */

struct {
	address act_id;
	int act_evcount;
	int act_gotres[2];
} act_map[MAX_ACT];

#define map_act(act)    (act_map[act].act_id)

#define NTRANSPORT     4

ifunc *by_transport[NTRANSPORT];
ifunc *by_newview[NTRANSPORT];
ifunc *by_physdead[NTRANSPORT];

#define sleep(n)                     isis_sleep(n)	/* Sleep special in ISIS tasks */
#define isis_inhibit_joins(entry)    bis(&inhibit_entries, entry)

#if ( __cplusplus || c_plusplus )
#define t_wait(cond)                 t_wait_l(cond, (char*)0)
#else
#define t_wait(cond)                 t_wait_l(cond, NULLARG)
#endif

#define pg_rank(a,b)                 _pg_rank((address*)a,(address*)b)
#define pg_rank_all(a,b)             _pg_rank_all((address*)a,(address*)b)

#ifndef print
#define print                        isis_print
#endif				/* print */

extern int isis_never;
extern condition isis_wantleave;	/* Wants to leave */
extern condition isis_decongested;

/*** Interface Routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif					/* __cplusplus */

#if ( __cplusplus || c_plusplus )
	int cl_rname(v_routine);
	void isis_mainloop(v_routine_vs ...);
	int isis_entry(int entry, v_routine_msgs, char *rname);
	int isis_task(v_routine_vs, char *rname);
	int isis_timeout(int time, v_routine_vs, VOID * arg0, VOID * arg1);
	int isis_ondrain(v_routine, VOID * arg0);
	void isis_print(char *fmt ...);
#else
	void cl_rname(void (*routine) ());
	void isis_mainloop(void (*routine) (VOID *), VOID * arg0);
	void isis_entry(int entry, void (*routine) (message * msg), char *rname);
	void isis_task(void (*routine) (VOID * arg), char *rname);
	int isis_timeout(int time, void (*routine) (VOID * arg), VOID * arg0, VOID * arg1);
	int isis_ondrain(void (*routine) (), VOID * arg0);
	void isis_print( /* char *fmt, ... */ );
#endif

	void MSG_ENQUEUE(int entry, message * mp);
	message *msg_rcv(int entry);
	void t_printable_name(char *str, VOID * arg);

	void isis_timeout_cancel(int tid);
	int alist_len(address * alist);
	char *cl_getrname(int entry);
	int isis_init(int CLIENT_PORT);
	void isis_logentry(address * gaddr, int entry);
	void isis_perror(char *str);
	unsigned isis_sleep(unsigned n);
	unsigned isis_sleep_ms(int ms);
	void isis_start_done();
	int isis_rexec(int nwanted, address * gid, site_id * sites,
		       char *prog, char **args, char **env,
		       char *user, char *passwd, address * addrs);
#if  ( __cplusplus || c_plusplus )
	void panic(char *fmt ...);
#else
#   if    __GNUC__
	volatile void panic( /* fmt, ... */ );
	volatile void exit(int n);
#   else
	void panic( /* fmt, ... */ );
#   endif __GNUC__
#endif

	void pentry(address * addr, int entry);
	void pr_dump(int how);
#ifdef __cplusplus
}
#endif
#else

void isis_timeout_cancel();
event_id *bc_getevent();
sview *site_getview();			/* Gets a site-view */

#endif

/*** Internal Routines ***/

#if FUN_TYPES

#ifdef __cplusplus
extern "C" {
#endif
	address *make_address(int site, int inc, int pro, int ent);
	address *save_address(address * addr);
	address *_ADDRESS(int s, int i, int p, int e);
	address *_PRO(int entry);
	int _addr_cmp(address * a, address * b);
	int _addr_isnull(address * a);
#if     ( __cplusplus || c_plusplus )
	void isis_accept_events(int flag ...);
#else					/* ( __cplusplus || c_plusplus ) */
	void isis_accept_events();
#endif					/* ( __cplusplus || c_plusplus ) */
	int _pg_rank(address * gaddr, address * who);
	int _pg_rank_all(address * gaddr, address * who);
	int act_begin();
	void act_block();
	void act_end(int old_act);
	void act_ev(int inc, int act, int type);
	void act_restart();
	void act_scan();
	int act_start(address * aname, int old_act);
	void cc_gotres(int where);
	void check_cl_watch_queue(struct message *mp);
	void cl_del_pgroup(struct message *mp);
	void cl_do_del_pgroup(address * gaddr);
	void cl_local_delivery(message * mp);
	void cl_new_view(message * mp);
	void cl_rcv_reply(message * mp);
	void cl_register();
	void cl_setscope();
	void cl_watch_cancel(address * gaddr, int cl_wid);
	int cl_watch_for(address * gaddr, address * who);
	void clw_free(cl_watch * clp);
	message *collect_reply(int msgid);
	void do_cl_dump();
	void dump_act();
	void dump_cl_watch_queue();
	int find_act(address * a);
	void fork_sighandlers();
	int gop_lookup(char *gopname);
	int isis(int ent, message * msg, VOID * answ, int alen);
	void isis_decon_wait(int nwant);
	void isis_disconnect();
	void isis_gotmsg(struct message *mp, int flag, int pn);
	void protos_gotmsg(struct message *mp);
	void isis_has_crashed(int how);
	void isis_input_drain();
	int isis_pseudo_io(vfunc * routine);
	void isis_pseudo_io_cancel(int id);
	void isis_invoke(int entry, message * mp);
	void isis_overflow();
	int isis_pg_copy(sys_groupview * pg, groupview * gv);
	int isis_read();
	int isis_send(address * dest, message * msg);
	void isis_dosend(address * dest, message * msg);
	message *protos_rpc(address * dest, message * msg, int pause_flag);
#if ( __cplusplus || c_plusplus )
	int isis_rpc(address * addr ...);
	void (*isis_setfilter(v_proc)) ();
#else
	int isis_rpc( /* addr, <smsg>, <rfmt> */ );
	void (*isis_setfilter(void (*proc) ())) ();
#endif
	int isis_timeout_reschedule(int oldtid, int time,
				    void (*routine) (VOID * arg), VOID * arg0, VOID * arg1);
	void itimeout_event();
	int msg_trace_dump();
	void pgroups_dump();
	void pr_makedump(char *file);
	void pr_rescan();
	void pr_shutdown();
	void protos_despool();
	void run_isis(int block_flag);
	void run_tasks();
	address *set_entry(address * alist, int entry);
	int isis_failed();
#ifdef __cplusplus
}					/* matches extern "C" */
#endif					/* __cplusplus */
#else

address *make_address(), *save_address();
address *_ADDRESS(), *_PRO();
message *protos_rpc();
task *isis_fork();
ginfo *map_gname(), *map_gaddr(), *add_gname(), *add_group();
void cl_rcv_reply(), cl_new_view(), cl_del_pgroup(), cl_do_del_pgroup();
void act_block(), run_tasks(), run_isis();
void cl_local_delivery(), isis_has_crashed();

#ifdef CONVEX
void *isis_setfilter();
#else
vfunc *isis_setfilter();
#endif
void isis_accept_events();
void cl_register();
void isis_dosend();
void isis_gotmsg();
void protos_gotmsg();
void act_end();
void act_scan();
void act_ev();
void act_restart();
void isis_entry();
void MSG_ENQUEUE();
message *msg_rcv();
void t_printable_name();
void isis_task();
int isis_pseudo_io();
void isis_pseudo_io_cancel();
void cl_rname();
void cl_setscope();
void panic();
unsigned isis_sleep();
unsigned isis_sleep_ms();

#endif				/* FUN_TYPES */

#define ADDRESS(s,i,p,e) *_ADDRESS(s,i,p,e)
#define PRO(e)           *_PRO(e)

#define isis_init_suntools(p)                                {  \
    extern int isis_socket, suntools_loaded;                    \
    extern void run_tasks();                                    \
    suntools_loaded = 1;                                        \
    isis_init(p);                                               \
    (void)notify_set_prioritizer_func(run_tasks, NULLROUTINE);  \
    notify_set_input_func(run_tasks, run_tasks, intercl_socket);\
    notify_set_input_func(run_tasks, run_tasks, isis_socket); }

#ifndef	NEXT
extern char *malloc();
#endif				/* NEXT */
#endif				/* FUN_TYPES */

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	typedef struct timeval timeval_t;
	int isis_do_select(bitvec * in, bitvec * out, bitvec * except, int signo, vfunc * routine,
			   VOID * arg0, VOID * arg1);
	int isis_select(int bits, int *in, int *out, int *except, timeval_t * timeout);
	void isis_wait_cancel(int wid);
	int isis_gotsignal();
	bitvec *Bvec(int bno);
#ifdef __cplusplus
}
#endif
#else
bitvec *Bvec();
void t_sig();
void isis_wait_cancel();
#endif				/* FUN_TYPES */

extern bitvec Bz;

#define isis_input(fd,func,arg)      isis_do_select(Bvec(fd),&Bz,&Bz,0,(vfunc*)func,(VOID*)arg,NULLARG)
#define isis_output(fd,func,arg)     isis_do_select(&Bz,Bvec(fd),&Bz,0,(vfunc*)func,(VOID*)arg,NULLARG)
#define isis_except(fd,func,arg)     isis_do_select(&Bz,&Bz,Bvec(fd),0,(vfunc*)func,(VOID*)arg,NULLARG)
#define isis_signal(sg,func,arg)     isis_do_select(&Bz,&Bz,&Bz,sg,(vfunc*)func,(VOID*)arg,NULLARG)
#define isis_chwait(func,arg)        isis_signal(-99,func,arg)
#define isis_input_sig(fd,cond,arg)  isis_do_select(Bvec(fd),&Bz,&Bz,0,(v_proc)t_sig,cond,(VOID*)arg)
#define isis_output_sig(fd,cond,arg) isis_do_select(&Bz,Bvec(fd),&Bz,0,(v_proc)t_sig,cond,(VOID*)arg)
#define isis_except_sig(fd,cond,arg) isis_do_select(&Bz,&Bz,Bvec(fd),0,(v_proc)t_sig,cond,(VOID*)arg)
#define isis_signal_sig(sg,cond,arg) isis_do_select(&Bz,&Bz,&Bz,sg,(v_proc)t_sig,cond,(VOID*)arg)
#define isis_chwait_sig(cond,arg)    isis_signal_sig(-99,cond,arg)

#endif				/* CL_ISIS */
