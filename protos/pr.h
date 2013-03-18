/*  $RCSfile: pr.h,v $ $Revision: 2.24 $ $Date: 90/09/12 13:27:56 $  */
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
 *
 *
 *
 */
#ifndef PR
#define PR

/* Decide whether to use function prototypes (ANSI C and C++) or 
   old style function declarations. Not all "ansi" compilers are the same,
   especially when it comes to mixing new function prototypes with old-style
   function definitions, which is not defined in the standard. 
   Should match same defines done in isis.h and msg.h.
*/
#ifdef __STDC__ 
#  ifndef APOLLO
#    define FUN_TYPES      1
#  endif
#endif

/* Must match with definition in clib/isis.h */
#ifdef  SUN3
# define SUN                1
#endif

#ifdef  SUN4
# define SUN                1
#endif

#ifdef  SUN
# define UNIX_DOM            1
# ifdef   SUNLWP
#   define exit(n)           pod_exit(n)
# endif   SUNLWP
#endif SUN

#ifdef  AUX
# define UNIX_DOM           1
#endif

#ifdef  MACH
# define UNIX_DOM           1
# define CTHREADS           1  
#endif

#ifdef  APOLLO
# define UNIX_DOM           1
# define SIMWRITEV          1
# define exit(n)            kill(getpid(), SIGKILL)
#endif

#ifdef  VAX
# define UNIX_DOM           1
#endif

#ifdef  MIPS
# define UNIX_DOM           1
/* JB_SP defined in jmpbuf.h */
#endif

#ifdef  SGI
# define UNIX_DOM           1
/* JB_SP defined in jmpbuf.h */
#endif

#ifdef  RT43
# define UNIX_DOM           1
# define JB_SP              0
#endif


#ifdef  AIX
# define JB_SP              10
# define SIMWRITEV           1
#endif

#ifdef  AIXRS
# define JB_SP              3
# define UNIX_DOM           1
#endif

#ifdef NO_UNIX_DOM
#  ifdef UNIX_DOM
#    undef UNIX_DOM
#  endif UNIX_DOM
#endif NO_UNIX_DOM

#ifndef TRUE
#define TRUE                1
#define FALSE               0
#endif  TRUE

#define INCR_PRIO(prio)     (prio = (((((prio >> 8) + 1) & 0x7fffff) << 8) | \
                            (my_site_no & 0xff)))
#define PR_CBLAZY           -1
#define PR_CBURGENT         0

#define AB_DELIVERABLE      0x1
#define AB_UNDELIVERABLE    0x0
#define AB_GBCAST           0x2
#define AB_DELIV_GBCAST     (AB_DELIVERABLE | AB_GBCAST)
#define AB_UNDELIV_GBCAST   (AB_UNDELIVERABLE | AB_GBCAST)

#define GB_ABORT            -1
#define CHANGE_MODE          1
#define READ_MODE            2

/* Dump alternatives */
#define DUMP_ALL            0xFFFF
#define DUMP_MEM            0x0001
#define DUMP_VIEWS          0x0002
#define DUMP_INTERSITE      0x0004
#define DUMP_ASTORE         0x0008
#define DUMP_BCAST          0x0010
#define DUMP_CLIENT         0x0020
#define DUMP_MESSAGES       0x0040

/* For tracing */
#define PN_CBCAST           0
#define PN_ICBCAST          1
#define PN_ABCAST           2
#define PN_IABCAST          3
#define PN_GBCAST           4
#define PN_IGBCAST          5
#define PN_BCAST            6
#define PN_IBCAST           7
#define PN_REPLY            8
#define PN_CLIENT           9
#define PN_FBCAST           10
#define PN_IFBCAST          11

int     collect_answ();
extern char *proto_names[];

#define MC_ALLSITES         -1

#define MAX_PROCS           64
#define MAX_SITES           127         /* Also change in isis.h! */
#define MAXCQUEUE           32*1024     /* Max client backlog: 32kbytes */

/*
 *      Per-processor options:
 *          UNIX_DOM: TRUE if this system supports unix-domain IPC, which is
 *              faster than TCP in a single-site (but used to have a bug causing crashes
 *              if a process with a pending "connect" exited without doing the accept).
 *          SCATTER_SEND: Currently always false: seems to slow things down!
 */

#if (HPUX || AUX)
#include "sys/types.h"
#include "sys/file.h"
#include <memory.h>
#define random()        rand()
#define srandom(s)      srand(s)
#define bcopy(a,b,c)    memcpy(b,a,c)
#define bzero(a,b)      memset(a,0,b)
#endif

/* Congestion related parameters */
#define MEM_HI              500000      /* Memory usage is high */
#define MSGMEM_HI           500000      /* Message memory usage is high */
#define INTERSITE_HI       2000000      /* Intersite backlog is developing */
#define TASK_HI                 15      /* Number of active tasks is high */
#define MEM_LO              250000      /* Memory usage is ok */
#define MSGMEM_LO           250000      /* Message memory usage is ok */
#define TASK_LO                  6      /* Number of tasks is ok */
#define INTERSITE_LO        250000      /* Intersite backlog is clear */

#include "pr_typedefs.h"
#include "cl_typedefs.h"
#include "stdio.h"
#include "isis_alloc.h"
#include "bits.h"
#include "msg.h"
#include "pr_fdect.h"
#include "pr_msgfields.h"
#include "pr_inter.h"
#include "pr_pgroups.h"
#include "pr_wqueues.h"
#include "pr_queues.h"
#include "pr_qnode.h"
#include "pr_address.h"
#include "pr_astore.h"
#include "pr_errors.h"
#include "pr_task.h"
#include "pr_client.h"
#include "pr_global.h"
#include "pr_glocks.h"
#include "pr_stats.h"
#include "pr_msgtank.h"

#define forever             for(;;)
#define begin
#define NULLPRIO            0x80000000

typedef	int		IFUNC();
#define nullroutine     ((IFUNC*)0)

int     pr_lastmsg, pr_lastbcast;
extern  long    ab_priority;

struct wait_struct
{
    int         n_events;
    condition   cond;
};
#define W_INIT(w_struct)    (w_struct).n_events = 0; \
                            (w_struct).cond = 0
#define W_WAIT(w_struct)    if ((w_struct).n_events > 0) \
                                t_wait (&((w_struct).cond), "W_WAIT")

adesc   *qu_adescp;

#ifndef print
#define print                        isis_print
#endif print

#ifndef	NEXT
char	*malloc();
#endif
#endif  PR
