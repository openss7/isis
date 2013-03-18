/* History file for clib */
char clib_rcsid[] = "$Revision: 2.122 $$Date: 90/09/19 14:28:44 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/clib/RCS/history.c,v $";

/* $Log:	history.c,v $
 * Revision 2.122  90/09/19  14:28:44  ken
 * Oops
 * 
 * ci -u2.122 cl_bcast.c history.c

 * Revision 2.121  90/09/18  14:03:55  ken
 * Avoid problem with gbcast_l("X")
 * 
 * ci -u2.121 cl_bcast.c history.c

 * Revision 2.120  90/09/18  09:55:31  ken
 * *** empty log message ***
 * 
 * ci -u2.120 cl_bypass.c history.c

 * Revision 2.119  90/09/17  14:41:40  ken
 * Well, too many &
 * 
 * ci -u2.119 cl_isis.c history.c

 * Revision 2.118  90/09/17  14:39:43  ken
 * Missing &.
 * 
 * ci -u2.118 cl_isis.c history.c

 * Revision 2.117  90/09/17  14:37:03  ken
 * Oops
 * 
 * ci -u2.117 cl_isis.c history.c

 * Revision 2.116  90/09/16  21:25:19  ken
 * Change isis_input to pass argument even if line breaks
 * 
 * ci -u2.116 cl_isis.c history.c

 * Revision 2.115  90/09/15  21:28:59  ken
 * Oops, my fix to isis_input broke isis_select
 * 
 * ci -u2.115 cl_isis.c history.c

 * Revision 2.114  90/09/15  20:42:51  ken
 * Memory rule for congestion
 * 
 * ci -u2.114 cl_inter.c history.c

 * Revision 2.113  90/09/15  20:33:34  ken
 * Otherwise congestion could break reply causality
 * 
 * ci -u2.113 cl_bcast.c history.c

 * Revision 2.112  90/09/15  14:46:55  ken
 * Eliminate screwy congestion rule
 * 
 * ci -u2.112 cl_bcast.c history.c

 * Revision 2.111  90/09/15  14:23:33  ken
 * Fix the return value in isis_select, again...
 * 
 * ci -u2.111 cl_isis.c history.c

 * Revision 2.110  90/09/15  08:49:23  ken
 * Improve bypass handling of congestion on send
 * 
 * ci -u2.110 cl_bcast.c history.c

 * Revision 2.109  90/09/14  14:22:18  ken
 * add "bypass in use"
 * 
 * ci -u2.109 cl_bypass.c history.c

 * Revision 2.108  90/09/14  13:19:34  ken
 * Inhibits check for congestion on state xfer
 * 
 * ci -u2.108 cl_join.c history.c

 * Revision 2.107  90/09/13  22:40:17  ken
 * These changes are supposed to fix abcast in BYPASS mode
 * 
 * ci -u2.107 cl_task.c cl_bypass.c cl_isis.c history.c

 * Revision 2.106  90/09/13  20:45:15  ken
 * Fixes one bypass problem...
 * 
 * ci -u2.106 cl_bcast.c history.c

 * Revision 2.105  90/09/13  14:35:13  ken
 * Fixc isis_select bug
 * 
 * ci -u2.105 cl_isis.c history.c

 * Revision 2.104  90/09/12  13:26:05  ken
 * SGI changes
 * 
 * ci -u2.104 cl_isis.c cl_task.c isis.h makefile tk_connect.c tk_connect.h tk_xaction.c history.c

 * Revision 2.103  90/09/11  15:37:10  ken
 * Fix my fix
 * 
 * ci -u2.103 cl_bcast.c cl_bcast.h cl_join.c cl_pgroup.c history.c

 * Revision 2.102  90/09/11  11:17:50  rcbc
 * Fixed include file error in cl_hashtab.c
 * 
 * ci -u2.102 cl_hashtab.c history.c

 * Revision 2.101  90/09/10  17:28:11  rcbc
 * Fixed makefile (again!)
 * 
 * ci -u2.101 makefile history.c

 * Revision 2.100  90/09/10  10:16:30  rcbc
 * Fixed makefile rule for cl_tasks.o
 * 
 * ci -u2.100 makefile history.c

 * Revision 2.99  90/09/10  08:52:09  ken
 * Disables -O in HPUNX case
 * 
 * ci -u2.99 makefile history.c

 * Revision 2.98  90/09/10  08:42:49  ken
 * Minor stuff
 * 
 * ci -u2.98 cl_bcast.c history.c

 * Revision 2.97  90/09/07  16:54:26  ken
 * ...
 * 
 * ci -u2.97 spooler.h history.c

 * Revision 2.96  90/09/07  16:48:45  ken
 * checkin makefile
 * Don't optimize HPUX cl_task.c
 * 
 * ci -u2.96 makefiler history.c

 * Revision 2.95  90/09/07  16:34:15  ken
 * not used in clib
 * 
 * ci -u2.95 cl_task.c cl_dump.c history.c

 * Revision 2.94  90/09/07  15:57:18  ken
 * Delete old sysfields in a repeated bcast
 * 
 * ci -u2.94 cl_bcast.c history.c

 * Revision 2.93  90/09/07  15:45:51  ken
 * Minor change: catch "mixtures" of bypass and non-bypass processes
 * 
 * ci -u2.93 cl_bypass.c history.c

 * Revision 2.92  90/08/27  13:29:15  rcbc
 * Lisp fixes for 2.1
 * 
 * ci -u2.92 isis-c-load.lisp isis-c-load.cl isis_c_refs.c cl_isis.c history.c

 * Revision 2.91  90/08/27  10:22:08  rcbc
 * Fixes for lisp.
 * 
 * ci -u2.91 cl_task.c isis-c-load.lisp isis-c-load.cl history.c

 * Revision 2.90  90/08/24  11:18:09  ken
 * 110us mistake
 * 
 * ci -u2.90 cl_bcast.c history.c

 * Revision 2.89  90/08/21  20:35:01  patrick
 * 
 * 
 * ci -u2.89 isis.h history.c

 * Revision 2.88  90/08/17  14:03:32  patrick
 * minor bugs...
 * 
 * ci -u2.88 tk_long_haul.c history.c

 * Revision 2.87  90/08/15  16:10:55  ken
 * Fix isis_accept_events(ISIS_TIMEOUT)
 * 
 * ci -u2.87 cl_isis.c history.c

 * Revision 2.86  90/08/15  16:05:45  ken
 * Oops
 * 
 * ci -u2.86 cl_bypass.c history.c

 * Revision 2.85  90/08/15  09:55:49  ken
 * Define options here
 * 
 * ci -u2.85 cl_bcast.h history.c

 * Revision 2.84  90/08/15  09:55:25  ken
 * Oops! Option B was disabling virtual synchrony
 * 
 * ci -u2.84 cl_bypass.c cl_bypass.h cl_bcast.c history.c

 * Revision 2.83  90/08/14  16:58:19  ken
 * Must reset sin_family -- could be byte swapped!
 * 
 * ci -u2.83 cl_inter.c history.c

 * Revision 2.82  90/08/14  16:43:08  ken
 * Improve failure detection for isis_remote case
 * 
 * ci -u2.82 cl_inter.c history.c

 * Revision 2.81  90/08/14  16:18:16  ken
 * Oops, incomplete thought...
 * 
 * ci -u2.81 cl_inter.c history.c

 * Revision 2.80  90/08/14  14:25:57  ken
 * quiet down!
 * 
 * ci -u2.80 cl_isis.c history.c

 * Revision 2.79  90/08/14  10:46:50  ken
 * Set socket options in a consistent way
 * 
 * ci -u2.79 cl_inter.c cl_inter.h cl_isis.c tk_connect.c tk_remote.c history.c

 * Revision 2.78  90/08/13  20:21:32  ken
 * Checkin the fortran fixes we made
 * 
 * ci -u2.78 cl_bcast.c history.c

 * Revision 2.77  90/08/13  16:04:26  ken
 * More of the same problem with ACK seq.
 * 
 * ci -u2.77 cl_inter.h cl_inter.c history.c

 * Revision 2.76  90/08/13  15:05:54  ken
 * One-line change to make SITE_IS_UP work
 * 
 * ci -u2.76 isis.h history.c

 * Revision 2.75  90/08/10  16:23:55  ken
 * had an ntohs problem
 * 
 * ci -u2.75 tk_connect.c history.c

 * Revision 2.74  90/08/10  16:01:56  ken
 * Fixes to get t_on_sys_stack working again
 * 
 * ci -u2.74 cl_task.c cl_isis.c history.c

 * Revision 2.73  90/08/10  14:11:18  ken
 * ic_seqn is a short not a char!
 * 
 * ci -u2.73 cl_inter.c history.c

 * Revision 2.72  90/08/09  15:31:51  ken
 * Yuck
 * 
 * ci -u2.72 cl_isis.c history.c

 * Revision 2.71  90/08/09  15:29:04  ken
 * Use pre-picked port numbers to avoid HPUX tricks
 * 
 * ci -u2.71 cl_isis.c tk_remote.c tk_connect.c history.c

 * Revision 2.70  90/08/09  15:24:41  ken
 * Just in case htons() is a macro...
 * 
 * ci -u2.70 cl_isis.c history.c

 * Revision 2.69  90/08/09  15:18:26  ken
 * Force HPUX to pick port numbers in a "sane" range
 * 
 * ci -u2.69 cl_isis.c history.c

 * Revision 2.68  90/08/09  13:45:25  ken
 * Upgrade to V2.1, incompatible with prior versions
 * 
 * ci -u2.68 cl_typedefs.h history.c

 * Revision 2.67  90/08/09  13:42:03  ken
 * Path one last problem
 * 
 * ci -u2.67 tk_connect.c history.c

 * Revision 2.66  90/08/09  11:19:08  ken
 * Change to deal with concurrent connects
 * 
 * ci -u2.66 tk_connect.c history.c

 * Revision 2.65  90/08/09  09:53:42  ken
 * Remote duplicate def of cl_typedefs.h
 * 
 * ci -u2.65 isis.h history.c

 * Revision 2.64  90/08/08  12:53:00  rcbc
 * Bug fixes from Brad Glade.
 * 
 * ci -u2.64 cl_hashtab.c history.c

 * Revision 2.63  90/08/08  11:14:46  tclark
 * Idiotic CPP complaints.
 * 
 * ci -u2.63 cl_setjmp.s makefile history.c

 * Revision 2.62  90/08/08  11:06:48  tclark
 * Needed a cast
 * 
 * ci -u2.62 cl_groups.c history.c

 * Revision 2.61  90/08/08  11:04:10  tclark
 * pmsg has a bad call to paddr9)
 * 
 * ci -u2.61 cl_dump.c history.c

 * Revision 2.60  90/08/08  10:52:00  tclark
 * Move varargs.h include
 * 
 * ci -u2.60 isis.h history.c

 * Revision 2.59  90/08/07  21:48:30  ken
 * Oops again
 * 
 * ci -u2.59 cl_bypass.c history.c

 * Revision 2.58  90/08/07  21:43:13  ken
 * Oops
 * 
 * ci -u2.58 cl_bypass.c history.c

 * Revision 2.57  90/08/07  14:18:00  ken
 * improve "gateway" check on isis_joining
 * 
 * ci -u2.57 cl_join.c history.c

 * Revision 2.56  90/08/07  13:12:18  ken
 * Fix two syntax errors
 * 
 * ci -u2.56 cl_isis.c history.c

 * Revision 2.55  90/08/07  11:53:50  ken
 * Garbage collect bypass_waitq timeout nodes
 * 
 * ci -u2.55 cl_bypass.c cl_isis.c history.c

 * Revision 2.54  90/08/07  10:31:47  ken
 * Minor bypass changes to support xmgr better
 * 
 * ci -u2.54 cl_isis.c cl_bypass.c history.c

 * Revision 2.53  90/08/06  14:30:16  ken
 * Better way to manage the "deleted" queue
 * 
 * ci -u2.53 cl_bypass.c history.c

 * Revision 2.52  90/08/06  13:54:35  ken
 * AUX changes
 * 
 * ci -u2.52 isis.h makefile tk_xaction.c unix_lib.h history.c

 * Revision 2.51  90/08/06  13:53:45  ken
 * AUX changes, Two BYPASS bugs with pt-to-pt message ordering
 * 
 * ci -u2.51 cl_bypass.c cl_bypass.h cl_dump.c cl_dump.h cl_isis.c cl_join.c cl_pgroup.c cl_queues.c cl_queues.h cl_task.c cl_task.h cl_isis.c cl_setjmp.c cl_setjmp.s history.c

 * Revision 2.50  90/08/03  15:18:47  ken
 * Avoid xfer_where if no state xfer was done
 * 
 * ci -u2.50 cl_join.c history.c

 * Revision 2.49  90/08/03  15:09:31  rcbc
 * Reinstalled change lost a subsequent checking (sigh!).
 * 
 * ci -u2.49 isis.h history.c

 * Revision 2.48  90/08/03  13:09:19  rcbc
 * Fixed bug concerning partially initialized groupview in cl_do_del_pgroup.
 * Also improved printing out of protocols entry numbers.
 * 
 * ci -u2.48 cl_isis.c cl_watch.c history.c

 * Revision 2.47  90/08/03  09:29:18  ken
 * Improve clib printout of protos entry names
 * 
 * ci -u2.47 cl_isis.c cl_dump.c history.c

 * Revision 2.46  90/08/02  20:12:37  ken
 * Catch case where group vanishes while terminate is scheduled on it
 * 
 * ci -u2.46 cl_bypass.c history.c

 * Revision 2.45  90/08/02  20:08:54  ken
 * Deleted cached name if member executes pg_leave to exit
 * 
 * ci -u2.45 cl_pgroup.c history.c

 * Revision 2.44  90/08/02  17:57:40  rcbc
 * Type cast needed for DECStation.
 * 
 * ci -u2.44 cl_isis.c history.c

 * Revision 2.43  90/08/02  14:59:36  ken
 * make it work for non-members
 * 
 * ci -u2.43 cl_isis.c history.c

 * Revision 2.42  90/08/02  13:26:45  rcbc
 * Further fixes to pg_detect_failure processing upon pg_leaves.
 * Also x_begin Now retries after IE_AGAIN errors from pg_join.
 * 
 * ci -u2.42 cl_watch.c tk_xaction.c history.c

 * Revision 2.41  90/08/01  21:06:50  ken
 * No need to use cache refresh mechanism during pg_join calls
 * 
 * ci -u2.41 cl_pgroup.c history.c

 * Revision 2.40  90/08/01  17:05:29  ken
 * RT43 changes
 * 
 * ci -u2.40 cl_task.c cl_typedefs.h isis.h makefile history.c

 * Revision 2.39  90/08/01  15:21:43  ken
 * Fix isis_inhibit_joins
 * 
 * ci -u2.39 isis.h history.c

 * Revision 2.38  90/08/01  15:19:19  ken
 * correct problem with MAJORITY in bcast calls
 * 
 * ci -u2.38 cl_bcast.c history.c

 * Revision 2.37  90/08/01  15:13:30  rcbc
 * pg_detect_failure wasn't noticing when all the member pg_leave'd a group
 * (as opposed to simply leaving by failing).
 * 
 * ci -u2.37 cl_watch.c history.c

 * Revision 2.36  90/08/01  14:57:26  ken
 * Add bcast 'T' (timeout) option
 * 
 * ci -u2.36 cl_bcast.c history.c

 * Revision 2.35  90/08/01  11:56:57  ken
 * Fix pg_signal
 * 
 * ci -u2.35 cl_pgroup.c history.c

 * Revision 2.34  90/08/01  11:27:26  ken
 * BYPASS bug: reply delayed for causality was then treated as a dup
 * 
 * ci -u2.34 cl_isis.c cl_inter.c cl_bypass.c history.c

 * Revision 2.33  90/07/31  13:03:04  ken
 * AIXRS changes
 * 
 * ci -u2.33 cl_bypass.c cl_cmd.h cl_inter.c cl_setjmp.s cl_task.c isis.h spooler.h tk_lmgr.h tk_xaction.c history.c

 * Revision 2.32  90/07/31  11:31:47  ken
 * Changes to handle group delete while someone is in the bypass code
 * (mostly by panic!)
 * 
 * ci -u2.32 cl_bypass.c cl_groups.c isis.h history.c

 * Revision 2.31  90/07/31  11:14:12  rcbc
 * Fixed bug in initializing site view.
 * 
 * ci -u2.31 cl_sview.c history.c

 * Revision 2.30  90/07/31  10:41:44  ken
 * V2.1 bug fixes (eliminates the hang case caused by intercl_waitq)
 * 
 * ci -u2.30 isis.h cl_join.c cl_isis.c cl_inter.c cl_groupview.h cl_dump.c cl_bypass.c history.c

 * Revision 2.29  90/07/30  10:47:54  rcbc
 * Checked in hashtab into main trunk.
 * 
 * ci -u2.29 cl_hashtab.c cl_hashtab.h history.c

 * Revision 2.28  90/07/26  17:30:00  rcbc
 * Added some large groups stuff. Shouldn't affect anyone since
 * its ifdef'ed out.
 * 
 * ci -u2.28 isis.h history.c

 * Revision 2.27  90/07/26  17:28:08  rcbc
 * Fixed a number of type casts for gcc.
 * Also included some ifdef'ed out stuff for large groups.
 * Should affect anyone.
 * ...oops, shouldn't affect anyone.
 * 
 * ci -u2.27 cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_task.c cl_task.h cl_watch.c isis.h tk_xaction.c history.c

 * Revision 2.26  90/07/26  14:01:07  tclark
 * TASK_PRNAME
 * 
 * ci -u2.26 cl_task.h history.c

 * Revision 2.25  90/07/26  13:37:28  rcbc
 * Added $Revision header line.
 * 
 * 
 * ci -u2.25 cl_bypass.c history.c

 * Revision 2.24  90/07/25  14:03:27  ken
 * Oops, had it in verbose mode
 * 
 * ci -u2.24 cl_bypass.c history.c

 * Revision 2.23  90/07/25  13:51:38  ken
 * V2.1 chamges
 * 
 * ci -u2.23 cl_isis.c history.c

 * Revision 2.22  90/07/25  13:50:20  ken
 * isis_accept_events change to support ISIS_TIMEOUT
 * .,
 * 
 * ci -u2.22 flib1.c flib2.c history.c

 * Revision 2.21  90/07/25  13:48:57  ken
 *  V2.1 bypass-related changes
 * 
 * ci -u2.21 cl_bypass.c cl_dump.c cl_groups.c cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_task.c cl_watch.c flib1.c flib2.c isis.h tk_rmgr.c history.c

 * Revision 2.20  90/07/03  11:12:36  tclark
 * y
 * y
 * 
 * ci -u2.20 isis.h history.c

 * Revision 2.19  90/07/03  10:44:36  tclark
 * Improve handling of XBYREF bit
 * 
 * ci -u2.19 cl_task.c history.c

 * Revision 2.18  90/07/03  09:58:26  tclark
 * Some fortran mode bits were left on in imbedded ISIS system calls
 * 
 * ci -u2.18 cl_watch.c history.c

 * Revision 2.17  90/06/24  15:11:14  rcbc
 * Fixed C++ patches for AT&T C++ 1.2
 * 
 * ci -u2.17 cl_groupview.h isis.h history.c

 * Revision 2.16  90/06/24  14:54:09  rcbc
 * Fixed C++ patches for AT&T C++ 1.2
 * 
 * ci -u2.16 cl_sview.h cl_task.h cl_typedefs.h history.c

 * Revision 2.15  90/06/24  14:51:05  rcbc
 * Fixed C++ patches for AT&T C++ 1.2
 * 
 * ci -u2.15 cl_bcast.h spooler.h history.c

 * Revision 2.14  90/06/24  14:50:09  rcbc
 * Fixed C++ patches for AT&T C++ 1.2
 * 
 * ci -u2.14 cl_bcast.h cl_bypass.h cl_groupview.h cl_inter.h unix_lib.h tk_xaction.h tk_lmgr.h spooler.h history.c

 * Revision 2.13  90/06/22  13:56:09  ken
 * pg_watch calls pg_getview instead of pg_getlocalview
 * 
 * ci -u2.13 cl_watch.c history.c

 * Revision 2.12  90/06/20  14:38:51  ken
 * Changes to system-wide NOALARM mode, bypass flush.
 * .`
 * 
 * ci -u2.12 cl_bypass.c cl_inter.c cl_isis.c history.c

 * Revision 2.11  90/06/20  14:20:48  ken
 * Increase to avoid possible name overflows
 * .,
 * 
 * ci -u2.11 tk_rmgr.c history.c

 * Revision 2.10  90/06/14  15:25:18  ken
 * fix EBADF infinite loop
 * 
 * ci -u2.10 cl_isis.c history.c

 * Revision 2.9  90/06/14  13:24:26  ken
 * Fix select bug
 * 
 * ci -u2.9 cl_isis.c isis.h tk_lmgr.h history.c

 * Revision 2.8  90/06/13  15:00:39  patrick
 * bug fixes/updates
 * 
 * ci -u2.8 tk_long_haul.c tk_spool2.c history.c

 * Revision 2.7  90/06/11  10:34:38  rcbc
 * Fixed function prototypes for C++.
 * 
 * ci -u2.7 unix_lib.h history.c

 * Revision 2.6  90/06/11  10:34:01  ken
 * isis_dofork was misdeclared
 * 
 * ci -u2.6 isis.h history.c

 * Revision 2.5  90/06/11  10:33:34  rcbc
 * Fixed function prototypes for C++.
 * 
 * ci -u2.5 isis.h cl_groupview.h cl_bcast.h spooler.h cl_isis.c cl_queues.h history.c

 * Revision 2.4  90/06/11  10:14:31  ken
 * Minor change for XBYREF callbacks
 * 
 * ci -u2.4 tk_authen.c history.c

 * Revision 2.3  90/06/11  10:14:03  ken
 * Fix fortran problems (management of XBYREF)
 * 
 * ci -u2.3 cl_coord.c cl_dump.c cl_isis.c cl_join.c cl_task.c cl_task.h isis.h tk_autehn.c tk_xaction.c history.c

 * Revision 2.2  90/06/11  10:08:21  patrick
 * long haul upgrades...
 * 
 * ci -u2.2 spooler.h history.c

 * Revision 2.1  90/05/21  13:17:07  rcbc
 * Preprocessor syntax error. When #define SUN4 instead of #define SUN4 1
 * 
 * ci -u2.1 isis.h history.c

 * Revision 2.0  90/05/04  15:21:56  rcbc
 * 2.0
 * 
 * Revision 1.561  90/05/04  09:46:12  rcbc
 * Racked up lmgr log file version number to 2.1
 * 
 * ci -u1.561 tk_lmgr.h history.c

 * Revision 1.560  90/05/03  16:25:55  ken
 * Better messages
 * 
 * ci -u1.560 cl_bypass.c history.c

 * Revision 1.559  90/05/03  16:21:47  ken
 * Better messages.
 * 
 * ci -u1.559 cl_inter.c history.c

 * Revision 1.558  90/05/03  14:17:59  rcbc
 * more damn type casts.
 * 
 * ci -u1.558 spooler.h history.c

 * Revision 1.557  90/05/03  14:16:59  rcbc
 * more damn type casting problems.
 * 
 * ci -u1.557 cl_cmd.h history.c

 * Revision 1.556  90/05/02  16:20:43  rcbc
 * Type casts and other syntax errors fixed for gcc on MACH.
 * 
 * ci -u1.556 cl_sunlwp_fix.c isis.h cl_setjmp.s history.c

 * Revision 1.555  90/05/02  14:15:08  ken
 * Oops
 * 
 * ci -u1.555 cl_pgroup.c history.c

 * Revision 1.554  90/05/02  12:56:49  rcbc
 * Type casts for MACH/NEXT
 * 
 * ci -u1.554 cl_bypass.c isis.h cl_plist.c history.c

 * Revision 1.553  90/05/02  09:12:50  ken
 * Various changes, mostly to fix BYPASS bugs
 * 
 * ci -u1.553 cl_bypass.c cl_inter.c cl_isis.c cl_pgroup.c cl_plist.c history.c

 * Revision 1.552  90/05/01  17:53:17  ken
 * bypass bug fix
 * 
 * ci -u1.552 cl_isis.c cl_plist.c history.c

 * Revision 1.551  90/05/01  17:42:04  ken
 * fix plists, eliminate "group unknown" bug
 * 
 * ci -u1.551 cl_isis.c cl_plist.h isis.h makefile history.c

 * Revision 1.550  90/05/01  14:01:13  rcbc
 * Fixed a few type casts for gcc.
 * 
 * ci -u1.550 cl_join.c cl_inter.c cl_bypass.c history.c

 * Revision 1.549  90/05/01  12:34:34  ken
 * V2.0 copyright release
 * 
 * ci -u1.549 isis-c-load.cl isis-msg.cl isis-task.cl isis-tools.cl isis.cl make-allegro.cl isis-c-load.lisp isis-msg.lisp isis-task.lisp isis-tools.lisp isis.lisp make-lucid.lisp history.c

 * Revision 1.548  90/05/01  10:54:27  ken
 * V2.0 copyright disclaimer
 * 
 * ci -u1.548 cl_bcast.h cl_bypass.h cl_cmd.h cl_coord.h cl_dump.h cl_groupview.h cl_inter.h cl_plist.h cl_queues.h cl_sview.h cl_task.h cl_token.h cl_typedefs.h fisis.h isis.h jmp_pragmas.h spooler.h tk_connect.h tk_lmgr.h tk_news.h tk_rmgr.h tk_xaction.h unix_lib.h ap_dummy.c cl_bcast.c cl_bypass.c cl_coord.c cl_dump.c cl_failed.c cl_groups.c cl_inter.c cl_isis.c cl_join.c cl_pgroup.c cl_plist.c cl_print.c cl_queues.c cl_setjmp.c cl_spool1.c cl_sundummy.c cl_sunlwp_fix.c cl_sview.c cl_task.c cl_token.c cl_watch.c flib.c flib1.c flib2.c isis_c_refs.c lisp_lib.c lmgr_dummy.c rmgr_dummy.c spool_dummy.c sview_init.c tk_authen.c tk_cdummy.c tk_connect.c tk_lmgr.c tk_long_haul.c tk_news.c tk_remote.c tk_rexec.c tk_rmgr.c tk_spool2.c tk_xaction.c x_dummy.c cl_setjmp.s history.c

 * Revision 1.547  90/05/01  10:04:53  rcbc
 * Moved a couple of routines which didn't have underscores in their names
 * to flib.c
 * 
 * ci -u1.547 flib.c flib1.c flib2.c history.c

 * Revision 1.546  90/05/01  09:54:32  ken
 * V2.0 changes to fix bypass bugs (whew!  It works!)
 * 
 * ci -u1.546 cl_bypass.c cl_bcast.c cl_inter.c cl_isis.c cl_join.c history.c

 * Revision 1.545  90/04/30  11:34:55  rcbc
 * Checkin of Mesaac's last set of changes. Compiles OK, but needs to
 * be tested with latest release.
 * 
 * ci -u1.545 tk_long_haul.c history.c

 * Revision 1.544  90/04/30  10:46:40  siegel
 * Fixed minor problems with tk_connect
 * 
 * ci -u1.544 tk_connect.c history.c

 * Revision 1.543  90/04/30  10:45:45  siegel
 * Fixed minor problems with tk_connect
 * 
 * ci -u1.543  history.c

 * Revision 1.542  90/04/27  15:21:22  ken
 * Oops
 * 
 * ci -u1.542 cl_bypass.c history.c

 * Revision 1.541  90/04/27  14:59:19  ken
 * y
 * y
 * 
 * ci -u1.541 ap_dummy.c history.c

 * Revision 1.540  90/04/27  14:58:50  ken
 * Copyright notices
 * 
 * ci -u1.540 ap_dummy.c cl_bcast.c cl_bcast.h cl_bypass.c cl_bypass.h cl_cmd.h cl_coord.c cl_coord.h cl_dump.c cl_dump.h cl_failed.c cl_groups.c cl_groupview.h cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_pgroup.c cl_plist.c cl_plist.h cl_print.c cl_queues.c cl_queues.h cl_setjmp.c cl_setjmp.s cl_spool1.c cl_sundummy.c cl_sunlwp_fix.c cl_sview.c cl_sview.h cl_task.c cl_task.h cl_token.c cl_token.h cl_typedefs.h cl_watch.c fisis.h flib.c flib1.c flib2.c isis-c-load.cl isis-c-load.lisp isis-msg.cl isis-msg.lisp isis-task.cl isis-task.lisp isis-tools.cl isis-tools.lisp isis.cl isis.h isis.lisp isis_c_refs.c jmp_pragmas.h lisp_lib.c lmgr_dummy.c make-allegro.cl make-lucid.lisp makefile rmgr_dummy.c spool_dummy.c spooler.h sview_init.c tk_authen.c tk_cdummy.c tk_connect.c tk_connect.h tk_lmgr.c tk_lmgr.h tk_long_haul.c tk_news.c tk_news.h tk_remote.c tk_rexec.c tk_rmgr.c tk_rmgr.h tk_spool2.c tk_xaction.c tk_xaction.h unix_lib.h x x_dummy.c history.c

 * Revision 1.539  90/04/27  14:53:30  ken
 * Copyright notices
 * 
 * ci -u1.539 vi make-allegro.cl history.c

 * Revision 1.538  90/04/27  14:52:53  ken
 * Copyright notice
 * 
 * ci -u1.538 fg history.c

 * Revision 1.537  90/04/26  16:53:18  ken
 * ABCAST changes
 * 
 * ci -u1.537 cl_isis.c cl_bypass.c history.c

 * Revision 1.536  90/04/26  16:40:27  ken
 * V2.0 BYPASS related changes
 * 
 * ci -u1.536 cl_bypass.c cl_groups.c cl_isis.c cl_join.c cl_plist.c isis.h history.c

 * Revision 1.535  90/04/26  09:21:47  rcbc
 * Changed names of library files.
 * 
 * ci -u1.535 makefile history.c

 * Revision 1.534  90/04/25  14:54:25  ken
 * Oops
 * 
 * ci -u1.534 cl_bypass.c history.c

 * Revision 1.533  90/04/25  14:51:15  ken
 * Various fixes relating to gbcast flush in BYPASS mode
 * 
 * ci -u1.533 cl_bypass.c cl_dump.c cl_inter.c cl_isis.c cl_sview.c isis.h tk_rmgr.c tk_xaction.c history.c

 * Revision 1.532  90/04/25  13:51:59  ken
 * *** empty log message ***
 * 
 * ci -u1.532 cl_bcast.c history.c

 * Revision 1.531  90/04/25  13:45:36  ken
 * Fix Pat's intercl-congestion "improvement"
 * 
 * ci -u1.531 cl_bcast.c history.c

 * Revision 1.530  90/04/25  09:28:55  ken
 * Changes to when terminate messages are sent
 * 
 * ci -u1.530 cl_isis.c cl_bypass.c history.c

 * Revision 1.529  90/04/24  14:53:40  ken
 * Two down, but more to go
 * 
 * ci -u1.529 isis.h cl_isis.c cl_bypass.c history.c

 * Revision 1.528  90/04/23  19:02:33  ken
 * Oops
 * 
 * ci -u1.528 cl_inter.c history.c

 * Revision 1.527  90/04/23  12:51:35  rcbc
 * For building allegro_clib and lucid_clib we now copy the mlib.a
 * library, and remove the msg_sa.o file from it to avoid lots
 * of multiply defined symbols when loading from lisp.
 * 
 * ci -u1.527 makefile history.c

 * Revision 1.526  90/04/23  12:46:46  rcbc
 * Updated allegro lisp interface for isis v2.0
 * 
 * ci -u1.526 isis-task.cl isis.cl isis-c-load.cl isis-msg.cl history.c

 * Revision 1.525  90/04/23  12:44:44  rcbc
 * Fixed NOALARMS case in isis_alarm routine.
 * In cl_dump.c made isis_nblock reference conditional on BYPASS being defined.
 * 
 * ci -u1.525 cl_dump.c cl_isis.c history.c

 * Revision 1.524  90/04/20  15:23:08  ken
 * Minor stuff
 * 
 * ci -u1.524 cl_dump.c isis.h history.c

 * Revision 1.523  90/04/20  14:25:26  ken
 * Various changes to support tk_remote
 * 
 * ci -u1.523 cl_isis.c history.c

 * Revision 1.522  90/04/20  14:24:48  ken
 * Varios tk_remote changes
 * 
 * ci -u1.522 cl_isis.c history.c

 * Revision 1.521  90/04/20  14:22:07  ken
 * Various fixes, mostly to tk_remote
 * 
 * ci -u1.521 cl_bypass.c cl_inter.c cl_isis.c cl_join.c tk_rexec.c history.c

 * Revision 1.520  90/04/20  14:20:41  ken
 * Sundry changes
 * 
 * ci -u1.520 cl_isis.c history.c

 * Revision 1.519  90/04/18  16:29:22  ken
 * Various changes
 * 
 * ci -u1.519 cl_inter.c history.c

 * Revision 1.518  90/04/18  16:28:53  ken
 * Various changes
 * 
 * ci -u1.518 cl_inter.c cl_isis.c cl_join.c cl_task.c history.c

 * Revision 1.517  90/04/17  15:15:33  patrick
 * change congestion limits
 * 
 * ci -u1.517 cl_inter.c history.c

 * Revision 1.516  90/04/17  13:27:44  ken
 * Various changes including NOALARM
 * 
 * ci -u1.516 cl_bypass.c cl_coord.c cl_inter.c cl_inter.h cl_isis.c tk_remote.c history.c

 * Revision 1.515  90/04/17  11:30:12  patrick
 * added a needed declaration on the c++ side
 * 
 * ci -u1.515 cl_typedefs.h history.c

 * Revision 1.514  90/04/17  09:30:26  rcbc
 * Removed _ADDRESS and _PRO.
 * 
 * ci -u1.514 isis_c_refs.c history.c

 * Revision 1.513  90/04/16  17:26:57  rcbc
 * Changed calls to isis_wait to isis_select. Save's a function call.
 * 
 * ci -u1.513 tk_connect.c history.c

 * Revision 1.512  90/04/16  17:21:05  rcbc
 * Wrong number of args to coord-cohort.
 * 
 * ci -u1.512 isis-tools.lisp history.c

 * Revision 1.511  90/04/16  16:29:25  rcbc
 * Updated lisp ports for ISIS V2.0
 * 
 * ci -u1.511 isis_c_refs.c lisp_lib.c history.c

 * Revision 1.510  90/04/16  16:25:23  rcbc
 * Updated lucid port for isis v2.0
 * 
 * ci -u1.510 isis-c-load.lisp isis-msg.lisp isis-task.lisp isis-tools.lisp isis.lisp make-lucid.lisp history.c

 * Revision 1.509  90/04/16  14:52:32  ken
 * Still hacking away...
 * 
 * ci -u1.509 cl_bypass.c cl_inter.c cl_isis.c cl_task.h history.c

 * Revision 1.508  90/04/16  12:19:26  ken
 * *** empty log message ***
 * 
 * ci -u1.508 cl_task.c cl_bypass.c history.c

 * Revision 1.507  90/04/16  11:24:13  ken
 * task congestion shouldn't interrupt sending broadcasts
 * 
 * ci -u1.507 cl_task.c history.c

 * Revision 1.506  90/04/16  11:20:58  ken
 * Another try...
 * 
 * ci -u1.506 cl_task.c history.c

 * Revision 1.505  90/04/16  11:14:56  ken
 * Oops!
 * 
 * ci -u1.505 cl_inter.c history.c

 * Revision 1.504  90/04/16  11:10:12  ken
 * Implement in-client task congestion
 * 
 * ci -u1.504 cl_task.c history.c

 * Revision 1.503  90/04/16  10:56:27  ken
 * (basically, nslots--)
 * 
 * ci -u1.503 cl_bypass.c cl_inter.c cl_isis.c history.c

 * Revision 1.502  90/04/16  09:35:20  ken
 * Eliminate panic printout
 * 
 * ci -u1.502 cl_isis.c history.c

 * Revision 1.501  90/04/16  09:10:19  ken
 * y
 * y
 * y
 * y
 * y
 * y
 * y
 * y
 * y
 * y
 * y
 * y
 * y
 * 
 * ci -u1.501 cl_bcast.c cl_bypass.c cl_bypass.h cl_inter.c cl_inter.h cl_isis.c cl_queues.h isis.h tk_remote.c history.c

 * Revision 1.500  90/04/13  22:09:44  ken
 * fix BUGID #1
 * 
 * ci -u1.500 cl_bypass.c history.c

 * Revision 1.499  90/04/13  14:54:44  ken
 * V2.0 bug fixes
 * 
 * ci -u1.499 cl_bcast.c cl_bypass.c cl_bypass.h cl_inter.c cl_isis.c cl_queues.h isis.h history.c

 * Revision 1.498  90/04/12  12:53:37  ken
 * init my_host
 * 
 * ci -u1.498 cl_isis.c history.c

 * Revision 1.497  90/04/12  11:24:02  patrick
 * Output congestion fixes
 * 
 * ci -u1.497 isis.h cl_isis.c cl_inter.c cl_bcast.c cl_bypass.c history.c

 * Revision 1.496  90/04/10  21:33:52  ken
 * Buf fix
 * 
 * ci -u1.496 isis.h history.c

 * Revision 1.495  90/04/06  14:35:37  ken
 * V2.0 incremental checkin
 * 
 * ci -u1.495 cl_bcast.c cl_bypass.c cl_dump.c cl_inter.c cl_isis.c tk_remote.c history.c

 * Revision 1.494  90/04/05  15:03:51  ken
 * Chug chug chug
 * 
 * ci -u1.494 cl_dump.c cl_inter.c cl_isis.c tk_remote.c history.c

 * Revision 1.493  90/04/05  13:54:46  patrick
 * fixed typos
 * 
 * ci -u1.493 isis.h history.c

 * Revision 1.492  90/04/05  11:10:07  ken
 * various isis_remote related changes
 * 
 * ci -u1.492 cl_bcast.c cl_dump.c cl_inter.c cl_isis.c isis.h tk_remote.c history.c

 * Revision 1.491  90/04/05  11:06:54  ken
 * Didn't count number of local dests
 * 
 * ci -u1.491 cl_bypass.c history.c

 * Revision 1.490  90/04/04  13:24:18  rcbc
 * Make invoke routine external so that lisp can call it.
 * 
 * ci -u1.490 cl_task.c history.c

 * Revision 1.489  90/04/03  16:22:44  ken
 * More minor stuff
 * 
 * ci -u1.489 cl_bypass.c cl_inter.c cl_isis.c history.c

 * Revision 1.488  90/04/03  10:07:39  ken
 * HPUX compile fixes
 * 
 * ci -u1.488 cl_bcast.c cl_inter.c cl_isis.c tk_xaction.c history.c

 * Revision 1.487  90/04/03  09:20:09  ken
 * 
 * 
 * cd ../SUN4
 * make
 * 
 * ci -u1.487 cl_bypass.c history.c

 * Revision 1.486  90/04/02  15:32:54  ken
 * V2.0 misc. changes
 * 
 * ci -u1.486 cl_inter.c flib.c history.c

 * Revision 1.485  90/04/02  15:30:32  ken
 * various minor (and major) changes
 * 
 * ci -u1.485 RCS ap_dummy.c cl_bcast.c cl_bcast.h cl_bypass.c cl_bypass.h cl_cmd.h cl_coord.c cl_coord.h cl_dump.c cl_dump.h cl_failed.c cl_groups.c cl_groupview.h cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_pgroup.c cl_plist.c cl_plist.h cl_print.c cl_queues.c cl_queues.h cl_setjmp.c cl_setjmp.s cl_spool1.c cl_sundummy.c cl_sunlwp_fix.c cl_sview.c cl_sview.h cl_task.c cl_task.h cl_token.c cl_token.h cl_typedefs.h cl_watch.c fisis.h flib.c flib1.c flib2.c history.c isis-c-load.cl isis-c-load.lisp isis-msg.cl isis-msg.lisp isis-task.cl isis-task.lisp isis-tools.cl isis-tools.lisp isis.cl isis.h isis.lisp isis_c_refs.c jmp_pragmas.h lisp_lib.c lmgr_dummy.c make-allegro.cl make-lucid.lisp makefile rmgr_dummy.c spool_dummy.c spooler.h sview_init.c tk_authen.c tk_cdummy.c tk_connect.c tk_connect.h tk_lmgr.c tk_lmgr.h tk_lmgr.h.old tk_long_haul.c tk_news.c tk_news.h tk_remote.c tk_rexec.c tk_rmgr.c tk_rmgr.h tk_spool2.c tk_xaction.c tk_xaction.h x_dummy.c history.c

 * Revision 1.484  90/03/29  10:04:12  rcbc
 * Fixed void * --> VOID * errors.
 * 
 * ci -u1.484 cl_typedefs.h history.c

 * Revision 1.483  90/03/29  10:02:26  rcbc
 * Fixed void * --> VOID * errors.
 * 
 * ci -u1.483 cl_typdefs.h cl_dump.c history.c

 * Revision 1.482  90/03/26  14:11:00  ken
 * New log manager
 * 
 * ci -u1.482 cl_join.c isis.h lmgr_dummy.c tk_lmgr.c tk_lmgr.h history.c

 * Revision 1.481  90/03/21  09:14:07  ken
 * dec. of nullreply
 * 
 * ci -u1.481 cl_bcast.h history.c

 * Revision 1.480  90/03/19  19:08:30  ken
 * ...
 * 
 * ci -u1.480 cl_bcast.c history.c

 * Revision 1.479  90/03/19  16:00:53  ken
 * y
 * y
 * y
 * 
 * ci -u1.479 cl_bcast.c cl_bcast.h cl_bypass.c cl_dump.c cl_inter.c cl_isis.c cl_task.c isis.h ls -l history.c

 * Revision 1.478  90/03/17  15:48:12  rcbc
 * Fixed null pointer bug in loop.
 * 
 * ci -u1.478 cl_bypass.c history.c

 * Revision 1.477  90/03/17  15:14:26  ken
 * cl_dump used to core dump if called during task startup
 * 
 * ci -u1.477 cl_inter.c cl_dump.c history.c

 * Revision 1.476  90/03/16  17:27:29  patrick
 * changed __plusplus to __cplusplus
 * 
 * ci -u1.476 isis.h history.c

 * Revision 1.475  90/03/16  14:06:37  kane
 * *** empty log message ***
 * 
 * ci -u1.475 tk_lmgr.c history.c

 * Revision 1.474  90/03/16  13:44:01  ken
 * Minor problem
 * 
 * ci -u1.474 cl_bypass.c history.c

 * Revision 1.473  90/03/15  17:51:35  rcbc
 * Added RCSId.
 * 
 * ci -u1.473 cl_cbcast.h history.c

 * Revision 1.472  90/03/15  17:49:13  rcbc
 * Now knows about large group addresses.
 * 
 * ci -u1.472 cl_dump.c history.c

 * Revision 1.471  90/03/15  15:38:11  rcbc
 * Added RCS Id
 * 
 * ci -u1.471 cl_bcast.h cl_coord.h cl_dump.h cl_sview.h cl_token.h lmgr_dummy.c rmgr_dummy.c spool_dummy.c tk_long_haul.c x_dummy.c history.c

 * Revision 1.470  90/03/15  13:10:37  rcbc
 * Added rcsid.
 * 
 * ci -u1.470 cl_bypass.h history.c

 * Revision 1.469  90/03/15  11:41:20  patrick
 * fixed a couple of minor declaration problems.
 * 
 * ci -u1.469 cl_bypass.h cl_isis.c history.c

 * Revision 1.468  90/03/15  10:18:55  rcbc
 * Fixed two type errors.
 * 
 * ci -u1.468 cl_bypass.c history.c

 * Revision 1.467  90/03/14  16:58:40  rcbc
 * Fixed typechecking problems, and removed #include unix_lib.h from
 * isis.h.
 * 
 * ci -u1.467 isis.h unix_lib.h cl_isis.c history.c

 * Revision 1.466  90/03/14  14:35:33  rcbc
 * Re added lost changes to flib. Added other typechecking fixes.
 * Added unix function prototypes.
 * 
 * ci -u1.466 flib1.c flib2.c tk_xaction.c tk_xaction.h isis.h unix_lib.h history.c

 * Revision 1.465  90/03/14  13:07:42  ken
 * it works!
 * 
 * ci -u1.465 cl_bypass.c cl_groups.c cl_task.c cl_task.h isis.h history.c

 * Revision 1.464  90/03/14  10:20:28  rcbc
 * fixed up gross piece of address arithmetic in wrapper to x_outcomes.
 * Not tested, but then who uses Fortran?
 * 
 * ci -u1.464 flib.c history.c

 * Revision 1.463  90/03/14  10:16:37  rcbc
 * Now knows the names of more util processes.
 * 
 * ci -u1.463 cl_dump.c history.c

 * Revision 1.462  90/03/14  10:01:53  ken
 * Incremental changes checked in.
 * 
 * ci -u1.462 cl_bcast.c cl_bypass.c cl_dump.c cl_groups.c cl_groupview.h cl_isis.c cl_plist.c cl_plist.h isis.h history.c

 * Revision 1.461  90/03/09  11:43:16  ken
 * Move to dual flib that supports _ in fortran-called routines
 * 
 * ci -u1.461 makefile flib1.c flib2.c history.c

 * Revision 1.460  90/03/09  11:32:44  ken
 * Dup. decl.
 * 
 * ci -u1.460 cl_task.h history.c

 * Revision 1.459  90/03/09  11:19:14  ken
 * Whew!
 * 
 * ci -u1.459 cl_isis.c cl_setjmp.s cl_task.c cl_task.h makefile history.c

 * Revision 1.458  90/03/08  16:25:00  ken
 * ...
 * 
 * ci -u1.458 cl_task.c history.c

 * Revision 1.457  90/03/08  15:14:34  ken
 * Pat should have done this
 * 
 * ci -u1.457 cl_isis.c history.c

 * Revision 1.456  90/03/08  14:16:02  ken
 * ...
 * 
 * ci -u1.456 cl_task.c history.c

 * Revision 1.455  90/03/08  14:10:55  ken
 * y
 * y
 * y
 * cd ../SUN4/run_demos
 * 
 * ci -u1.455 cl_task.c history.c

 * Revision 1.454  90/03/08  14:08:53  ken
 * Arghh!
 * 
 * ci -u1.454 cl_task.c history.c

 * Revision 1.453  90/03/08  14:04:11  ken
 * ...
 * 
 * ci -u1.453 cl_isis.c history.c

 * Revision 1.452  90/03/08  13:45:55  ken
 * Changes to start adding isis_remote, SUN4 changes to use
 * setjmp/longjmp all through
 * 
 * ci -u1.452 isis.h makefile cl_task.c cl_setjmp.s cl_remote.c cl_isis.c cl_inter.c cl_inter.h history.c

 * Revision 1.451  90/03/07  13:32:26  ken
 * void dec.
 * 
 * ci -u1.451 cl_dump.c history.c

 * Revision 1.450  90/03/07  12:15:59  patrick
 * added missing #endif!!!!
 * 
 * ci -u1.450 tk_lmgr.h history.c

 * Revision 1.449  90/03/07  12:14:12  patrick
 * c++ mods
 * 
 * ci -u1.449 cl_isis.c isis.h history.c

 * Revision 1.448  90/03/07  11:40:23  ken
 * ...
 * 
 * ci -u1.448 isis.h cl_isis.c history.c

 * Revision 1.447  90/03/07  11:30:16  patrick
 * c+=
 * 
 * ci -u1.447 cl_bcast.h history.c

 * Revision 1.446  90/03/07  11:28:40  patrick
 * moved some vars from the .h file
 * 
 * ci -u1.446 cl_sview.c history.c

 * Revision 1.445  90/03/07  11:26:53  patrick
 * moved a var from the .h file
 * 
 * ci -u1.445 cl_pgroup.c history.c

 * Revision 1.444  90/03/07  11:24:27  patrick
 * y
 * 
 * ci -u1.444 tk_xaction.h history.c

 * Revision 1.443  90/03/07  11:22:23  patrick
 * c++
 * 
 * ci -u1.443 tk_rmgr.h history.c

 * Revision 1.442  90/03/07  11:21:17  patrick
 * 
 * c++ changes
 * 
 * ci -u1.442 tk_news.h history.c

 * Revision 1.441  90/03/07  11:20:17  patrick
 * c++ changes
 * 
 * ci -u1.441 tk_lmgr.h history.c

 * Revision 1.440  90/03/07  10:51:49  patrick
 * c++ changes
 * 
 * ci -u1.440 spooler.h history.c

 * Revision 1.439  90/03/07  10:47:53  patrick
 * *** empty log message ***
 * 
 * ci -u1.439 cl_typedefs.h history.c

 * Revision 1.438  90/03/07  10:46:12  patrick
 * c++ changes
 * 
 * ci -u1.438 cl_token.h history.c

 * Revision 1.437  90/03/07  10:45:20  patrick
 * c++ changes
 * 
 * ci -u1.437 cl_task.c cl_task.h history.c

 * Revision 1.436  90/03/07  10:37:38  patrick
 * c++ declarations
 * 
 * ci -u1.436 cl_sview.h history.c

 * Revision 1.435  90/03/07  10:36:39  patrick
 * c++ declarations
 * 
 * ci -u1.435 cl_queues.h history.c

 * Revision 1.434  90/03/07  10:32:21  patrick
 * c++ declarations
 * 
 * ci -u1.434 cl_inter.h history.c

 * Revision 1.433  90/03/07  10:29:16  patrick
 * c++ changes
 * 
 * ci -u1.433 cl_groupview.h history.c

 * Revision 1.432  90/03/07  10:26:45  patrick
 * c++ changes
 * 
 * ci -u1.432 cl_dump.h history.c

 * Revision 1.431  90/03/07  10:25:39  patrick
 * c++ changes
 * 
 * ci -u1.431 cl_coord.h history.c

 * Revision 1.430  90/03/07  10:24:30  patrick
 * c++ mods
 * 
 * ci -u1.430 cl_bypass.h history.c

 * Revision 1.429  90/03/07  10:18:05  patrick
 * c++ changes!
 * 
 * ci -u1.429 cl_cbcast.h history.c

 * Revision 1.428  90/03/06  15:58:25  ken
 * add cl_sundummy.c
 * 
 * ci -u1.428 makefile history.c

 * Revision 1.427  90/03/06  15:57:52  ken
 * Various minor changes
 * 
 * ci -u1.427 cl_bypass.c cl_inter.c cl_isis.c cl_sundummy.c cl_task.c isis.h isis_c_refs.c history.c

 * Revision 1.426  90/03/02  14:54:47  rcbc
 * Added definition of dump_task() routine.
 * 
 * ci -u1.426 cl_dump.h history.c

 * Revision 1.425  90/03/02  14:51:47  rcbc
 * dump_task() now prints the pc, sp, and fp for sun3s and sun4s.
 * This may help a hacker to look at tasks other than the currently
 * running task.
 * 
 * ci -u1.425 cl_dump.c history.c

 * Revision 1.424  90/02/28  12:58:31  rcbc
 * Fixed some function declarations. 
 * 
 * ci -u1.424 spooler.h history.c

 * Revision 1.423  90/02/25  11:46:23  ken
 * deallocate various group views
 * 
 * ci -u1.423 cl_groups.c history.c

 * Revision 1.422  90/02/24  10:19:22  rcbc
 * Added sunlwp_fix.c file for calling sun lwp routines from gcc compiled
 * sun4 code.
 * 
 * ci -u1.422 makefile history.c

 * Revision 1.421  90/02/24  10:16:45  rcbc
 * Added wrappers for Sun lightweight processes calls from Sun4 gcc
 * compiled code. Sun4 gcc and Sun cc disagree on by-value struct
 * passing so these wrappers are needed. Sun lwp still doesn't work
 * with Isis, so there are some remaining V2.0 bugs relating
 * to threads possiby.
 * 
 * ci -u1.421 cl_task.h makefile history.c

 * Revision 1.420  90/02/23  10:38:54  ken
 * Changed for rcbc
 * 
 * ci -u1.420 isis.h history.c

 * Revision 1.419  90/02/22  08:47:33  rcbc
 * Changed most spooler routines to return void.
 * 
 * ci -u1.419 spooler.h tk_spool2.c flib.c history.c

 * Revision 1.418  90/02/21  13:43:00  ken
 * ...
 * 
 * ci -u1.418 isis.h cl_isis.c history.c

 * Revision 1.417  90/02/21  12:51:04  ken
 * ...
 * 
 * ci -u1.417 cl_isis.c history.c

 * Revision 1.416  90/02/21  12:46:36  ken
 * ...
 * 
 * ci -u1.416 cl_watch.c history.c

 * Revision 1.415  90/02/21  12:41:49  ken
 * Fix a bug in the pg_lookup caching code
 * 
 * ci -u1.415 cl_watch.c history.c

 * Revision 1.414  90/02/20  16:19:45  ken
 * ...
 * 
 * ci -u1.414 cl_inter.c cl_isis.c cl_watch.c history.c

 * Revision 1.413  90/02/20  16:16:22  rcbc
 * Minor typing typos.
 * 
 * ci -u1.413 cl_task.h cl_inter.c history.c

 * Revision 1.412  90/02/20  15:33:35  ken
 * ....
 * 
 * ci -u1.412 cl_bcast.h cl_bypass.h cl_groupview.h cl_inter.h cl_isis.c cl_pgroup.c cl_task.h cl_watch.c isis.h history.c

 * Revision 1.411  90/02/20  14:54:56  ken
 * ...
 * 
 * ci -u1.411 tk_lmgr.h history.c

 * Revision 1.410  90/02/20  14:40:54  rcbc
 * Merge of Ken's changes with Robert's voidifying.
 * 
 * ci -u1.410 cl_watch.c cl_inter.c cl_bypass.c cl_pgroup.c history.c

 * Revision 1.409  90/02/20  14:11:41  rcbc
 * Voidified various functions, added implicit int arguments, and did
 * various other check using gcc -Wall, including removing unused
 * variables, fixing routines that return with and without values.
 * 
 * ci -u1.409 makefile tk_xaction.c cl_task.c cl_task.h cl_bcast.c cl_inter.c cl_inter.h flib.c tk_lmgr.c tk_spool2.c tk_connect.h spooler.h isis.h cl_isis.c cl_bypass.c cl_pgroup.c cl_bypass.h cl_bcast.h cl_groupview.h cl_watch.c cl_spool1.c tk_lmgr.h spool_dummy.c cl_queues.c cl_dump.c cl_sview.c cl_join.c cl_coord.c cl_print.c cl_token.c cl_failed.c tk_cdummy.c rmgr_dummy.c lmgr_dummy.c x_dummy.c tk_rexec.c tk_authen.c tk_rmgr.c tk_news.c tk_long_haul.c tk_connect.c tk_xaction.h history.c

 * Revision 1.408  90/02/16  17:30:44  rcbc
 * Minor syntax errors found when compiling with gcc.
 * 
 * ci -u1.408 tk_long_haul.c cl_bypass.c flib.c history.c

 * Revision 1.407  90/02/16  15:29:13  ken
 * ....
 * 
 * ci -u1.407 isis.h cl_isis.c cl_bypass.c history.c

 * Revision 1.406  90/02/16  10:59:17  ken
 * ...
 * 
 * ci -u1.406 cl_bcast.c cl_bypass.c cl_groups.c cl_inter.c cl_isis.c cl_pgroup.c isis.h tk_xaction.c history.c

 * Revision 1.405  90/02/14  09:48:44  ken
 * ...
 * 
 * ci -u1.405 cl_pgroup.c history.c

 * Revision 1.404  90/02/14  09:43:22  ken
 * garbage in pw_gv?
 * 
 * ci -u1.404 cl_pgroup.c history.c

 * Revision 1.403  90/02/12  15:24:27  ken
 * y
 * 
 * ci -u1.403 cl_isis.c history.c

 * Revision 1.402  90/02/12  15:23:43  ken
 * ...
 * 
 * ci -u1.402 cl_isis.c history.c

 * Revision 1.401  90/02/12  15:06:13  ken
 * ...
 * 
 * ci -u1.401 cl_isis.c history.c

 * Revision 1.400  90/02/12  15:05:29  ken
 * Oops
 * 
 * ci -u1.400 cl_isis.c history.c

 * Revision 1.399  90/02/12  14:19:22  ken
 * ...
 * 
 * ci -u1.399 cl_isis.c history.c

 * Revision 1.398  90/02/12  14:08:34  ken
 * y
 * y
 * cd ../SUN4
 * make clib
 * 
 * ci -u1.398 cl_isis.c history.c

 * Revision 1.397  90/02/12  10:57:04  ken
 * ....
 * 
 * ci -u1.397 cl_isis.c history.c

 * Revision 1.396  90/02/09  09:30:24  ken
 * ....
 * 
 * ci -u1.396 cl_task.c cl_task.h history.c

 * Revision 1.395  90/02/08  17:13:24  ken
 * ...
 * 
 * ci -u1.395 cl_isis.c history.c

 * Revision 1.394  90/02/08  17:09:08  ken
 * ...
 * 
 * ci -u1.394 cl_isis.c history.c

 * Revision 1.393  90/02/08  11:08:02  ken
 * ...
 * 
 * ci -u1.393 cl_isis.c history.c

 * Revision 1.392  90/02/08  10:49:29  ken
 * ...
 * 
 * ci -u1.392 cl_isis.c cl_inter.c cl_inter.h history.c

 * Revision 1.391  90/02/07  16:24:25  ken
 * MSG_DELETE/msg_delete
 * 
 * ci -u1.391 cl_bypass.c cl_coord.c cl_inter.c cl_isis.c history.c

 * Revision 1.390  90/02/06  14:49:41  ken
 * ...
 * y
 * 
 * ci -u1.390 cl_bcast.c cl_bypass.c cl_inter.c cl_isis.c cl_task.c makefile tk_xaction.c tk_xaction.h history.c

 * Revision 1.389  90/02/06  14:44:40  ken
 * ...
 * 
 * ci -u1.389 ap_dummy.c history.c

 * Revision 1.388  90/02/02  10:44:18  ken
 * args were swapped
 * 
 * ci -u1.388 cl_isis.c history.c

 * Revision 1.387  90/02/02  10:05:28  ken
 * oops
 * 
 * ci -u1.387 cl_isis.c history.c

 * Revision 1.386  90/02/02  09:53:40  ken
 * Fix isis_wait_cancel for Mark
 * 
 * ci -u1.386 isis.h cl_isis.c history.c

 * Revision 1.385  90/02/02  09:27:26  ken
 * ...
 * 
 * ci -u1.385 cl_isis.c history.c

 * Revision 1.384  90/02/01  13:12:03  ken
 * Oops!
 * 
 * ci -u1.384 cl_isis.c history.c

 * Revision 1.383  90/02/01  10:31:38  ken
 * don't t_waiting() on iw_cond if null
 * 
 * ci -u1.383 cl_isis.c history.c

 * Revision 1.382  90/02/01  10:30:00  ken
 * Various minor bug fixes
 * 
 * ci -u1.382 cl_bcast.c cl_bypass.c cl_inter.c isis.h history.c

 * Revision 1.381  90/01/30  21:46:31  ken
 * Minor typo
 * 
 * ci -u1.381 isis.h history.c

 * Revision 1.380  90/01/30  21:43:48  ken
 * SUNLWP no longer default
 * 
 * ci -u1.380 isis.h history.c

 * Revision 1.379  90/01/30  20:49:33  ken
 * *** empty log message ***
 * 
 * ci -u1.379 cl_task.h history.c

 * Revision 1.378  90/01/30  20:47:55  ken
 * address alignment changes
 * 
 * ci -u1.378 cl_typedefs.h cl_bcast.c cl_bypass.c cl_coord.c cl_dump.c cl_failed.c cl_groups.c cl_inter.c cl_isis.c cl_join.c cl_pgroup.c cl_print.c cl_queues.c cl_setjmp.c cl_spool1.c cl_sundummy.c cl_sview.c cl_task.c cl_token.c cl_watch.c flib.c history.c isis_c_refs.c lisp_lib.c lmgr_dummy.c rmgr_dummy.c spool_dummy.c sview_init.c tk_authen.c tk_cdummy.c tk_connect.c tk_lmgr.c tk_news.c tk_rexec.c tk_rmgr.c tk_spool2.c tk_xaction.c x_dummy.c history.c

 * Revision 1.377  90/01/28  14:59:53  ken
 * arg type mismatch
 * 
 * ci -u1.377 flib.c history.c

 * Revision 1.376  90/01/28  14:55:12  ken
 * Two minor bug fixes
 * 
 * ci -u1.376 cl_isis.c cl_bcast.c history.c

 * Revision 1.375  90/01/26  11:01:31  ken
 * fix suntools_loaded bug
 * 
 * ci -u1.375 cl_isis.c cl_sundummy.c history.c

 * Revision 1.374  90/01/26  10:46:09  ken
 * V2.0 changes
 * 
 * ci -u1.374 cl_isis.c history.c

 * Revision 1.373  90/01/26  10:43:34  ken
 * V2.0 debugging changes
 * 
 * ci -u1.373 cl_isis.c cl_bypass.c cl_inter.c flib.c isis.h cl_join.c cl_inter.h cl_queues.h cl_sview.c isis_c_refs.c cl_task.c cl_task.h cl_bypass.h cl_bcast.c cl_groups.c history.c

 * Revision 1.372  90/01/12  14:04:24  ken
 * BYPASS transport protocol interface changes
 * 
 * ci -u1.372 cl_bypass.c cl_bypass.h cl_inter.c cl_inter.h cl_sview.c isis.h isis_c_refs.c history.c

 * Revision 1.371  90/01/11  14:19:37  ken
 * Minor bug
 * 
 * ci -u1.371 cl_pgroup.c history.c

 * Revision 1.370  90/01/09  14:19:02  ken
 * Check in some minor GCC fixes
 * 
 * ci -u1.370 cl_inter.h cl_isis.c cl_join.c cl_pgroup.c cl_task.c makefile rmgr_dummy.c history.c

 * Revision 1.369  90/01/04  13:09:00  ken
 * Various minor V2.0 stuff
 * 
 * ci -u1.369 cl_bypass.c cl_inter.c cl_inter.h cl_join.c cl_task.c flib.c makefile history.c

 * Revision 1.368  89/12/15  13:39:56  ken
 * ISIS V2.0 major checkin
 * 
 * ci -u1.368 cl_bcast.c cl_bcast.h cl_bypass.c cl_bypass.h cl_cmd.h cl_coord.c cl_coord.h cl_dump.c cl_dump.h cl_failed.c cl_groups.c cl_groupview.h cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_pgroup.c cl_print.c cl_queues.c cl_queues.h cl_setjmp.c cl_spool1.c cl_sundummy.c cl_sview.c cl_sview.h cl_task.c cl_task.h cl_token.c cl_token.h cl_typedefs.h cl_watch.c fisis.h flib.c isis.h isis_c_refs.c jmp_pragmas.h lisp_lib.c lmgr_dummy.c rmgr_dummy.c spool_dummy.c spooler.h sview_init.c tk_authen.c tk_cdummy.c tk_connect.c tk_lmgr.c tk_lmgr.h tk_news.c tk_news.h tk_rexec.c tk_rmgr.c tk_rmgr.h tk_spool2.c tk_xaction.c tk_xaction.h x_dummy.c history.c

 * Revision 1.367  89/12/12  10:58:48  mak
 * *** empty log message ***
 * 
 * ci -u1.367 spooler.h history.c

 * Revision 1.366  89/12/12  10:57:14  mak
 * Adding new entry points for the long haul communication group.
 * 
 * ci -u1.366 spooler.h history.c

 * Revision 1.365  89/12/12  10:54:37  mak
 * Adding new entry points for the long haul communication group.
 * 
 * ci -u1.365 spooler.h history.c

 * Revision 1.364  89/12/12  10:51:40  mak
 * Adding the long haul direct interface.
 * 
 * ci -u1.364 cl_spool.c history.c

 * Revision 1.363  89/12/12  10:44:34  mak
 * Adding direct long haul interface
 * 
 * ci -u1.363  history.c

 * Revision 1.362  89/11/06  17:03:59  ken
 * ...
 * 
 * ci -u1.362 cl_inter.c history.c

 * Revision 1.361  89/11/06  17:01:26  ken
 * Slightly patched up
 * 
 * ci -u1.361 cl_inter.c history.c

 * Revision 1.360  89/11/06  16:45:07  ken
 * ISIS V2.0 checkin
 * 
 * ci -u1.360 cl_alloc.c cl_inter.c cl_task.c cl_task.h history.c

 * Revision 1.359  89/11/06  16:43:49  ken
 * y
 * y
 * y
 * y
 * y
 * y
 * y
 * co -l cl_inter.c
 * 
 * ci -u1.359 cl_alloc.c cl_inter.c cl_task.c cl_task.h history.c

 * Revision 1.358  89/11/06  16:41:15  ken
 * ISIS V2.0 mlib changes
 * 
 * ci -u1.358 cl_alloc.c cl_bcast.c cl_bypass.c cl_bypass.h cl_coord.c cl_dump.c cl_inter.c cl_inter.h cl_isis.c cl_pgroup.c cl_token.c cl_typedefs.h flib.c g_eval.c isis.h history.c

 * Revision 1.357  89/10/26  11:56:16  ken
 * ISIS V2.0 mlib changes
 * 
 * ci -u1.357 cl_bcast.c cl_bypass.c cl_dump.c cl_inter.c cl_isis.c cl_news.c cl_pgroup.c cl_token.c cl_typedefs.h cl_watch.c flib.c isis.h isis_c_refs.c spooler.h tk_rexec.c history.c

 * Revision 1.356  89/10/13  17:34:02  rcbc
 * C arrays were getting reversed when converting them to lisp lists!
 * 
 * ci -u1.356 isis-msg.cl isis-msg.lisp history.c

 * Revision 1.355  89/10/13  17:32:38  rcbc
 * Fixed typecast error in ISISCALL macro used for lisp.
 * 
 * ci -u1.355 cl_task.h history.c

 * Revision 1.354  89/10/11  12:52:22  ken
 * msg_gotmsg change
 * 
 * ci -u1.354 cl_bcast.c cl_bypass.c cl_inter.c cl_inter.h cl_isis.c history.c

 * Revision 1.353  89/10/10  10:34:59  ken
 * Changes for "continuous" mode
 * 
 * ci -u1.353 cl_inter.c cl_isis.c history.c

 * Revision 1.352  89/10/05  20:42:29  ken
 * BYPASS fixes...
 * 
 * ci -u1.352 cl_bypass.c cl_inter.c cl_isis.c isis.h history.c

 * Revision 1.351  89/09/29  15:53:57  ken
 * Move comment to a better place
 * 
 * ci -u1.351 cl_isis.c history.c

 * Revision 1.350  89/09/29  13:45:07  ken
 * cd ../util
 * !ls
 * cd ../protos
 * !ls
 * cd ../mlib
 * !ls
 * 
 * ci -u1.350 g_parse.y history.c

 * Revision 1.349  89/09/29  13:42:11  ken
 * Close, but keep those cigars wrapped...
 * 
 * ci -u1.349 cl_bypass.c history.c

 * Revision 1.348  89/09/28  17:07:17  ken
 * !ls
 * 
 * ci -u1.348 cl_queues.h cl_inter.c cl_bypass.c history.c

 * Revision 1.347  89/09/27  08:57:13  ken
 * Change CALL into ISISCALL
 * 
 * ci -u1.347 cl_coord.c cl_isis.c cl_join.c cl_lmgr.c cl_pgroup.c cl_queues.h cl_setjmp.s cl_sview.c cl_task.c cl_task.h cl_watch.c cl_xaction.c tk_authen.c history.c

 * Revision 1.346  89/09/26  11:34:17  ken
 * Problems found when checking out on terra
 * 
 * ci -u1.346 cl_isis.c isis.h history.c

 * Revision 1.345  89/09/26  09:13:56  ken
 * Fix apollo 'exit' bug with sledgehammer
 * 
 * ci -u1.345 isis.h history.c

 * Revision 1.344  89/09/25  21:12:34  ken
 * ....
 * 
 * ci -u1.344 isis.h history.c

 * Revision 1.343  89/09/25  20:20:50  ken
 * ...
 * 
 * ci -u1.343 cl_typedefs.h history.c

 * Revision 1.342  89/09/25  20:20:10  ken
 * ...
 * 
 * ci -u1.342 isis.h history.c

 * Revision 1.341  89/09/25  20:16:50  ken
 * ...
 * 
 * ci -u1.341 isis.h history.c

 * Revision 1.340  89/09/25  16:49:34  ken
 * add FORK_BROKEN option
 * 
 * ci -u1.340 cl_groups.c cl_isis.c cl_join.c cl_sundummy.c isis.h history.c

 * Revision 1.339  89/09/25  09:08:12  ken
 * OPT_LEVEL_1.h
 * 
 * ci -u1.339 cl_task.c cl_dump.c history.c

 * Revision 1.338  89/09/22  14:09:52  ken
 * Various stuff
 * 
 * ci -u1.338 cl_dump.c cl_isis.c cl_task.c cl_typedefs.h isis.h history.c

 * Revision 1.337  89/09/19  12:00:32  patrick
 * put in some c++ varargs declarations
 * 
 * ci -u1.337 isis.h history.c

 * Revision 1.336  89/09/18  17:04:07  patrick
 * fiexed compile bug
 * 
 * ci -u1.336 cl_bypass.c history.c

 * Revision 1.335  89/09/18  15:38:19  patrick
 * moved addr/host data structure to make it accessible
 * 
 * ci -u1.335 cl_inter.c cl_inter.h history.c

 * Revision 1.334  89/09/18  12:36:57  ken
 * ...
 * 
 * ci -u1.334 isis.h history.c

 * Revision 1.333  89/09/18  09:04:44  ken
 * gcc incompatibility message
 * .,
 * 
 * ci -u1.333 isis.h history.c

 * Revision 1.332  89/09/17  20:59:21  patrick
 * Fixed the "GCC cannot be used" error message so it would not cause the
 * preprocessor to fail
 * 
 * ci -u1.332 isis.h history.c

 * Revision 1.331  89/09/17  16:59:49  patrick
 * made inclusion of body of isis.h conditional, to avoid seeing it twice.
 * 
 * ci -u1.331 isis.h history.c

 * Revision 1.330  89/09/17  13:04:37  ken
 * gcc cannot be used to compile this version of ISIS
 * 
 * ci -u1.330 isis.h history.c

 * Revision 1.329  89/09/15  14:18:55  ken
 * Better diagnostic, eliminate a confusion
 * 
 * ci -u1.329 cl_bypass.h cl_inter.c cl_inter.h history.c

 * Revision 1.328  89/09/15  08:52:02  ken
 * ...
 * 
 * ci -u1.328 cl_bypass.h history.c

 * Revision 1.327  89/09/15  08:43:13  ken
 * Two minor changes
 * 
 * ci -u1.327 cl_inter.c cl_bypass.c history.c

 * Revision 1.326  89/09/14  15:14:02  patrick
 * Added a definition of vfunc inside "#ifdef c_plusplus"
 * 
 * ci -u1.326 cl_typedefs.h history.c

 * Revision 1.325  89/09/14  11:30:35  rcbc
 * More ANSI (and not so ANSI) C changes. Now define FUN_TYPES define
 * when we have a __STDC__ compiler that we like. For ones we don't
 * like we don't try to declare new style function headers.
 * 
 * ci -u1.325 cl_bcast.h cl_bypass.h cl_cmd.h cl_coord.h cl_dump.h cl_groupview.h cl_inter.h cl_lmgr.h cl_news.h cl_queues.h cl_rmgr.h cl_sview.h cl_task.h cl_token.h cl_typedefs.h cl_xaction.h fisis.h guards.h isis.h jmp_pragmas.h spooler.h cl_bypass.c cl_watch.c cl_xaction.c tk_rexec.c history.c

 * Revision 1.324  89/09/13  22:37:37  patrick
 * Fixed a declaration to keep gcc happy.
 * 
 * ci -u1.324 cl_coord.c history.c

 * Revision 1.323  89/09/13  19:49:45  ken
 * ...
 * 
 * ci -u1.323 cl_task.c history.c

 * Revision 1.322  89/09/13  14:51:17  ken
 * ...
 * 
 * ci -u1.322 cl_coord.c history.c

 * Revision 1.321  89/09/13  14:50:00  ken
 * ...
 * 
 * ci -u1.321 cl_coord.c cl_inter.c history.c

 * Revision 1.320  89/09/13  13:53:42  ken
 * signature problems
 * 
 * ci -u1.320 cl_bcast.h cl_bypass.h cl_cmd.h cl_coord.h cl_dump.h cl_groupview.h cl_inter.h cl_join.c cl_lmgr.h cl_news.h cl_pgroup.c cl_queues.h cl_rmgr.h cl_sview.h cl_task.h cl_token.h cl_typedefs.h cl_xaction.h fisis.h guards.h isis.h jmp_pragmas.h history.c

 * Revision 1.319  89/09/13  13:21:57  ken
 * Minor stuff
 * 
 * ci -u1.319 cl_groupview.h tk_connect.c history.c

 * Revision 1.318  89/09/13  13:07:24  ken
 * more signature complaints
 * 
 * ci -u1.318 cl_groupview.h cl_inter.h cl_sview.h cl_task.h history.c

 * Revision 1.317  89/09/13  12:31:27  ken
 * More minor type problems
 * 
 * ci -u1.317 cl_isis.c cl_join.c cl_pgroup.c cl_queues.h cl_sview.c cl_task.c cl_task.h history.c

 * Revision 1.316  89/09/13  10:16:05  ken
 * VOID
 * 
 * ci -u1.316 cl_isis.c history.c

 * Revision 1.315  89/09/13  09:56:53  ken
 * VOID
 * 
 * ci -u1.315 cl_alloc.c cl_isis.c cl_queues.c cl_sview.c cl_task.c cl_task.h cl_typedefs.h cl_watch.c cl_xaction.c cl_xaction.h isis.h tk_connect.c history.c

 * Revision 1.314  89/09/12  12:57:18  ken
 * Syntax errors
 * 
 * ci -u1.314 cl_isis.c history.c

 * Revision 1.313  89/09/12  12:44:12  ken
 * ....
 * 
 * ci -u1.313 cl_task.h cl_token.c cl_watch.c cl_xaction.c history.c

 * Revision 1.312  89/09/12  12:43:35  ken
 * 
 * ...
 * 
 * ci -u1.312 cl_alloc.c cl_coord.c cl_inter.c cl_isis.c cl_join.c cl_pgroup.c cl_queues.c cl_sview.c cl_task.c history.c

 * Revision 1.311  89/09/12  10:11:00  ken
 * Sigh
 * 
 * ci -u1.311 cl_isis.c history.c

 * Revision 1.310  89/09/12  09:20:55  ken
 * ...
 * 
 * y
 * y
 * y
 * y
 * 
 * ci -u1.310 cl_isis.c cl_xaction.c tk_rexec.c history.c

 * Revision 1.309  89/09/12  08:54:07  ken
 * More stupid typos
 * 
 * ci -u1.309 cl_isis.c history.c

 * Revision 1.308  89/09/12  08:42:46  ken
 * Minor typo
 * 
 * ci -u1.308 tk_connect.c history.c

 * Revision 1.307  89/09/12  08:12:59  ken
 * Mionor
 * 
 * ci -u1.307 cl_typedefs.h history.c

 * Revision 1.306  89/09/11  15:00:32  patrick
 * made declaration of bc_node externally visible
 * 
 * ci -u1.306 cl_bypass.c history.c

 * Revision 1.305  89/09/11  15:00:01  patrick
 * made declaration of bc_node externally visible
 * 
 * ci -u1.305 cl_bypass.c cl_bypass.h history.c

 * Revision 1.304  89/09/11  14:53:19  patrick
 * removed a double declaration.
 * 
 * ci -u1.304 cl_typedefs.h history.c

 * Revision 1.303  89/09/11  14:51:37  patrick
 * *** empty log message ***
 * 
 * ci -u1.303 cl_typedefs.h history.c

 * Revision 1.302  89/09/11  12:57:41  ken
 * oops
 * 
 * ci -u1.302 cl_queues.h history.c

 * Revision 1.301  89/09/11  12:53:14  ken
 * y
 * y
 * y
 * y
 * 
 * ci -u1.301 cl_queues.h cl_isis.c cl_bypass.c history.c

 * Revision 1.300  89/09/11  11:51:56  patrick
 * Yet more C++ declarations
 * 
 * ci -u1.300 cl_typedefs.h history.c

 * Revision 1.299  89/09/11  11:41:41  rcbc
 * Added VERSION and FULL_VERSION string defines. This way both protos
 * and clib get the same version number to use in startup messages.
 * 
 * ci -u1.299 cl_typedefs.h history.c

 * Revision 1.298  89/09/11  10:25:58  ken
 * ...
 * 
 * ci -u1.298 cl_isis.c history.c

 * Revision 1.297  89/09/11  09:50:47  ken
 * inv_level was an undeclared and useless variable
 * 
 * ci -u1.297 g_parse.y history.c

 * Revision 1.296  89/09/08  13:44:30  patrick
 * Fixing declarations etc for ATT C++
 * 
 * ci -u1.296 cl_bypass.c cl_typedefs.h history.c

 * Revision 1.295  89/09/08  13:30:22  patrick
 * Fixed declarations so the Isis header files can pass through ATT C++ 1.2
 * 
 * 
 * ci -u1.295 cl_bcast.h cl_bypass.c cl_bypass.h cl_coord.h cl_dump.h cl_groupview.h cl_inter.h cl_isis.c cl_lmgr.h cl_news.h cl_queues.c cl_queues.h cl_rmgr.h cl_sview.h cl_task.c cl_task.h cl_token.h cl_typedefs.h cl_xaction.c cl_xaction.h g_eval.c guards.h isis.h spooler.h history.c

 * Revision 1.294  89/09/07  12:21:06  rcbc
 * Fixed type checking of xterm.
 * 
 * ci -u1.294 flib.c history.c

 * Revision 1.293  89/09/07  10:01:23  rcbc
 * Random bug fixes: cl_task.c had a syntax error in the debugging prints.
 * cl_isis.c had a bug in the error message printed by isis_entry,
 * and I fixed isis_has_crashed to avoid reinitializing the task system
 * upon program restart. This was needed for util/isis.c and auto restart.
 * 
 * ci -u1.293 cl_task.c cl_isis.c history.c

 * Revision 1.292  89/09/05  13:04:51  patrick
 * Fixed for GCC.  Added third arg to isis_gotmsg, misc declarations.
 * 
 * ci -u1.292 cl_bypass.c history.c

 * Revision 1.291  89/08/31  15:14:37  rcbc
 * Type error in function declarations.
 * 
 * ci -u1.291 isis.h history.c

 * Revision 1.290  89/08/31  09:43:00  rcbc
 * Fixed typing of callback routines.
 * 
 * ci -u1.290 cl_xaction.c cl_xaction.h history.c

 * Revision 1.289  89/08/30  17:57:19  rcbc
 * Moved make_address and save_address from lisp_lib to cl_isis.c
 * and put their prototypes in isis.h, to make them generally available
 * to more than just lisp program.s
 * 
 * ci -u1.289 lisp_lib.c isis.h cl_isis.c history.c

 * Revision 1.288  89/08/30  17:30:25  rcbc
 * Added bool = unsigned int define to cl_typedefs.h and removed 
 * definition of bool from cl_xaction and tk_rmgr.
 * 
 * ci -u1.288 cl_typedefs.h cl_xaction.h cl_xaction.c tk_rmgr.c history.c

 * Revision 1.287  89/08/30  14:15:48  rcbc
 * Fixed some uninitialized variable errors flagged by gcc.
 * 
 * ci -u1.287 cl_coord.c cl_spool.c history.c

 * Revision 1.286  89/08/30  13:49:31  patrick
 * Fixed typechecking when BYPASS is defined.
 * 
 * ci -u1.286 cl_bypass.c cl_inter.h history.c

 * Revision 1.285  89/08/29  10:20:44  rcbc
 * Yet more typechecking fixes.
 * 
 * ci -u1.285 cl_bypass.h cl_isis.c flib.c isis.h history.c

 * Revision 1.284  89/08/28  15:21:51  rcbc
 * Fixes to make sure ISIS still compiles under non-ansi compilers.
 * 
 * ci -u1.284 cl_xaction.c cl_coord.c cl_inter.c cl_isis.c cl_inter.h cl_bypass.c cl_task.h cl_dump.h cl_spool.c cl_groupview.h cl_coord.h cl_join.c cl_alloc.c cl_bcast.c isis.h g_eval.c guards.h flib.c history.c

 * Revision 1.283  89/08/28  09:28:35  rcbc
 * Fixed bug in transacitions: a union was meant to be a struct!
 * 
 * ci -u1.283 cl_xaction.c cl_xaction.h cl_typedefs.h history.c

 * Revision 1.282  89/08/28  09:27:14  rcbc
 * Cleaned up some gcc warnings.
 * 
 * ci -u1.282 cl_isis.c cl_task.c cl_task.h history.c

 * Revision 1.281  89/08/25  14:05:50  rcbc
 * Fixed typechecking for use by gcc.
 * 
 * ci -u1.281 cl_queues.c history.c

 * Revision 1.280  89/08/25  14:03:07  rcbc
 * Now typechecks though gcc -W
 * Header files now contain function prototypes, and
 * many arguments/results now use void and void *.
 * 
 * ci -u1.280 RCS cl_alloc.c cl_bcast.c cl_bcast.h cl_bypass.c cl_bypass.h cl_cmd.h cl_coord.c cl_coord.h cl_dump.c cl_dump.h cl_failed.c cl_fio.c cl_groups.c cl_groupview.h cl_hashtab.c cl_hashtab.h cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_lgroup.c cl_lgroup.h cl_lmgr.c cl_lmgr.h cl_news.c cl_news.h cl_pgroup.c cl_print.c cl_queues.c cl_queues.h cl_rmgr.h cl_setjmp.c cl_setjmp.s cl_spool.c cl_sundummy.c cl_sview.c cl_sview.h cl_task.c cl_task.h cl_token.c cl_token.h cl_typedefs.h cl_watch.c cl_xaction.c cl_xaction.h fisis.h flib.c g_eval.c g_parse.y guards.h history.c isis-c-load.cl isis-c-load.lisp isis-msg.cl isis-msg.lisp isis-task.cl isis-task.lisp isis-tools.cl isis-tools.lisp isis.cl isis.h isis.lisp isis_c_refs.c jmp_pragmas.h lisp_lib.c make-allegro.cl make-lucid.lisp makefile spooler.h sview_init.c tk_authen.c tk_cdummy.c tk_connect.c tk_init.c tk_rexec.c tk_rmgr.c history.c

 * Revision 1.279  89/08/23  00:01:28  ken
 * make nullreply use fbcast; make it a no-op if no reply is wanted
 * 
 * ci -u1.279 cl_bcast.c history.c

 * Revision 1.278  89/08/21  20:11:34  ken
 * Change, for Alex S.
 * 
 * ci -u1.278 cl_isis.c history.c

 * Revision 1.277  89/08/21  18:50:34  ken
 * Alex's bug fix
 * 
 * ci -u1.277 cl_join.c history.c

 * Revision 1.276  89/08/20  21:00:49  ken
 * Might help a little
 * 
 * ci -u1.276 cl_isis.c history.c

 * Revision 1.275  89/08/19  15:22:56  siegel
 * Bug fix in isis_connect
 * 
 * ci -u1.275 tk_connect.c history.c

 * Revision 1.274  89/08/19  14:18:16  siegel
 * Fixed bug in isis_conn_rcv
 * 
 * ci -u1.274 tk_connect.c history.c

 * Revision 1.273  89/08/18  14:41:23  ken
 * minor boo-boo
 * 
 * ci -u1.273 tk_rexec.c history.c

 * Revision 1.272  89/08/18  13:43:02  ken
 * t_sig wants_incarn if needed
 * 
 * ci -u1.272 cl_sview.c history.c

 * Revision 1.271  89/08/18  13:41:55  ken
 * APOLLO fixes
 * 
 * ci -u1.271 cl_isis.c cl_task.c cl_task.h history.c

 * Revision 1.270  89/08/17  16:51:42  ken
 * Minor bus
 * 
 * ci -u1.270 cl_queues.h cl_groups.c cl_bypass.c history.c

 * Revision 1.269  89/08/17  10:06:33  ken
 * Sent ISIS into an infinite loop, scope proved to be a problem
 * 
 * ci -u1.269 cl_isis.c cl_coord.c history.c

 * Revision 1.268  89/08/17  08:41:57  siegel
 * Fixed prototype declaration
 * 
 * ci -u1.268 cl_xaction.h history.c

 * Revision 1.267  89/08/16  21:15:47  siegel
 * RCS hassles
 * 
 * ci -u1.267 tk_connect.c history.c

 * Revision 1.266  89/08/16  21:13:43  siegel
 * null
 * 
 * ci -u1.266 tk_connect.c history.c

 * Revision 1.265  89/08/16  21:12:29  siegel
 * Major overhaul to fix numerous problems
 * 
 * ci -u1.265 tk_connect.c history.c

 * Revision 1.264  89/08/16  17:54:33  rcbc
 * Fixed isis-input, fixed some foreign types, add defun-callback
 * 
 * ci -u1.264 isis.lisp isis-msg.lisp isis-c-load.lisp isis-task.lisp isis-task.cl history.c

 * Revision 1.263  89/08/15  15:53:17  ken
 * ...
 * 
 * ci -u1.263 cl_isis.c history.c

 * Revision 1.262  89/08/15  11:28:06  rcbc
 * Fixed bugs in Allegro and Lucid lisp ports.
 * 
 * ci -u1.262 isis-msg.cl isis-c-load.lisp isis-msg.lisp isis-task.lisp isis-tools.lisp isis.lisp make-lucid.lisp history.c

 * Revision 1.261  89/08/11  10:55:25  rcbc
 * (sigh) more lost changes from revision 1.220 which had to be merged back in.
 * 
 * ci -u1.261 cl_xaction.c cl_xaction.h history.c

 * Revision 1.260  89/08/10  16:50:37  rcbc
 * Reinstalled lost changes from revs 1.220 and 1.223 relating to 
 * lucid / allegro lisp port.
 * 
 * ci -u1.260 cl_task.h cl_task.c cl_coord.c history.c

 * Revision 1.259  89/08/10  15:24:10  rcbc
 * Added $Revision RCS keyword to all files.
 * 
 * ci -u1.259 RCS cl_alloc.c cl_bcast.c cl_bypass.c cl_cmd.h cl_coord.c cl_dump.c cl_failed.c cl_groups.c cl_groupview.h cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_lmgr.c cl_lmgr.h cl_news.c cl_news.h cl_pgroup.c cl_print.c cl_queues.c cl_queues.h cl_rmgr.h cl_setjmp.c cl_setjmp.s cl_setjmp.s.~1~ cl_spool.c cl_sundummy.c cl_sview.c cl_task.c cl_task.h cl_token.c cl_typedefs.h cl_watch.c cl_xaction.c cl_xaction.h fisis.h flib.c g_eval.c g_parse.y guards.h history.c isis-c-load.cl isis-c-load.lisp isis-msg.cl isis-msg.lisp isis-task.cl isis-task.lisp isis-tools.cl isis-tools.lisp isis.cl isis.h isis.lisp isis_c_refs.c jmp_pragmas.h lisp_lib.c make-allegro.cl make-lucid.lisp makefile spooler.h sview_init.c tk_authen.c tk_cdummy.c tk_connect.c tk_rexec.c tk_rmgr.c history.c

 * Revision 1.258  89/08/10  14:50:48  ken
 * Check sooner
 * 
 * ci -u1.258 cl_isis.c history.c

 * Revision 1.257  89/08/10  13:16:04  ken
 * Minor bug fix
 * 
 * ci -u1.257 flib.c history.c

 * Revision 1.256  89/08/10  13:15:28  ken
 * Minor stuff
 * 
 * ci -u1.256 flib.c history.c

 * Revision 1.255  89/08/09  15:00:54  ken
 * More MACH stuff
 * 
 * ci -u1.255 cl_join.c history.c

 * Revision 1.254  89/08/09  14:53:27  ken
 * ...
 * 
 * ci -u1.254 cl_inter.c cl_isis.c flib.c lisp_lib.c tk_rexec.c history.c

 * Revision 1.253  89/08/09  14:01:51  ken
 * ...
 * 
 * ci -u1.253 cl_task.h cl_task.c isis.h history.c

 * Revision 1.252  89/08/09  11:37:54  ken
 * ...
 * 
 * ci -u1.252 isis.h history.c

 * Revision 1.251  89/08/09  10:38:05  siegel
 * null
 * 
 * ci -u1.251 tk_connect.c history.c

 * Revision 1.250  89/08/09  10:36:53  siegel
 * Fixed bug caused by multiple connections
 * 
 * ci -u1.250 tk_connect.c history.c

 * Revision 1.249  89/08/09  10:16:37  ken
 * ...
 * 
 * ci -u1.249 cl_join.c history.c

 * Revision 1.248  89/08/09  09:16:48  ken
 * add AIX entry
 * 
 * ci -u1.248 makefile history.c

 * Revision 1.247  89/08/09  09:14:34  ken
 * ...
 * 
 * ci -u1.247 cl_inter.h history.c

 * Revision 1.246  89/08/08  16:46:53  rcbc
 * Simplified makefile, and added lucid targets.
 * 
 * ci -u1.246 makefile history.c

 * Revision 1.245  89/08/08  16:33:02  rcbc
 * Added lucid_clib target
 * 
 * ci -u1.245 makefile history.c

 * Revision 1.244  89/08/08  16:15:48  rcbc
 * Recheckin lost changes from revision 1.230.
 * 
 * ci -u1.244 isis_c_refs.c history.c

 * Revision 1.243  89/08/08  16:10:25  ken
 * ...
 * 
 * ci -u1.243 cl_inter.h history.c

 * Revision 1.242  89/08/08  14:38:04  ken
 * ...
 * 
 * ci -u1.242 cl_alloc.c history.c

 * Revision 1.241  89/08/08  14:37:15  ken
 * ...
 * 
 * ci -u1.241 cl_alloc.c cl_bcast.c cl_bypass.c cl_cmd.h cl_coord.c cl_dump.c cl_failed.c cl_groups.c cl_groupview.h cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_lmgr.c cl_lmgr.h cl_news.c cl_news.h cl_pgroup.c cl_print.c cl_queues.c cl_queues.h cl_rmgr.h cl_setjmp.c cl_setjmp.s cl_spool.c cl_sundummy.c cl_sview.c cl_task.c cl_task.h cl_token.c cl_typedefs.h cl_watch.c cl_xaction.c cl_xaction.h fisis.h flib.c g_eval.c g_parse.y guards.h isis-c-load.cl isis-msg.cl isis-task.cl isis-tools.cl isis.cl isis.h isis_c_refs.c jmp_pragmas.h lisp_lib.c make-allegro.cl makefile spooler.h sview_init.c tk_authen.c tk_cdummy.c tk_connect.c tk_rexec.c tk_rmgr.c history.c

 * Revision 1.240  89/08/08  14:15:33  rcbc
 * checkpoint of "stable" version of ISIS of 8/8/89.
 * 
 * ci -u1.240 tk_cdummy.c tk_connect.c history.c

 * Revision 1.239  89/08/08  14:13:42  rcbc
 * checkpoint of "stable" version of ISIS of 8/8/89.
 * 
 * ci -u1.239 -l cl_coord.c cl_isis.c cl_spool.c makefile spooler.h history.c

 * Revision 1.238  89/08/08  13:05:19  rcbc
 * Checkin of final version 1.2 system.
 * 
 * ci -u1.238 -l cl_coord.c cl_isis.c cl_task.c cl_task.h cl_xaction.c cl_xaction.h isis_c_refs.c makefile history.c

 * Revision 1.237  89/07/26  13:04:56  ken
 * ...
 * 
 * ci -u1.237 cl_spool.c history.c

 * Revision 1.236  89/07/26  12:44:41  ken
 * ...
 * 
 * ci -u1.236 cl_join.c cl_spool.c history.c

 * Revision 1.235  89/07/25  12:21:14  ken
 * ...
 * 
 * ci -u1.235 cl_join.c history.c

 * Revision 1.234  89/07/24  16:25:46  ken
 * ..
 * 
 * ci -u1.234 cl_spool.c flib.c history.c

 * Revision 1.233  89/07/24  14:04:08  ken
 * Various V1.2 mods
 * 
 * ci -u1.233 cl_isis.c cl_watch.c isis.h spooler.h history.c

 * Revision 1.232  89/07/19  17:01:30  rcbc
 * checkpoint in midhack.
 * 
 * ci -u1.232 isis-task.lisp history.c

 * Revision 1.231  89/07/19  17:00:43  rcbc
 * Fixed a few things in the Allegro port.
 * 
 * ci -u1.231 isis-tools.cl isis-task.cl history.c

 * Revision 1.230  89/07/19  17:00:22  rcbc
 * Fixed a few things in the Allegro port. 
 * 
 * ci -u1.230 isis_c_refs.c isis_tools.cl isis_task.cl isis-msg.cl isis-c-load.cl history.c

 * Revision 1.229  89/07/19  15:58:09  siegel
 * Fixed declaration of last_gettimeofday
 * 
 * ci -u1.229 cl_isis.c history.c

 * Revision 1.228  89/07/19  15:43:53  rcbc
 * Changed print to isis_print and made print a define.
 * . 
 * 
 * ci -u1.228 cl_print.c history.c

 * Revision 1.227  89/07/19  15:32:56  siegel
 * Added declaration for last_gettimeofday
 * 
 * ci -u1.227 cl_spool.c history.c

 * Revision 1.226  89/07/19  15:31:52  rcbc
 * Deleted last_gettimeofday declaration and added print define.
 * 
 * ci -u1.226 isis.h history.c

 * Revision 1.225  89/07/19  15:29:24  siegel
 * *** empty log message ***
 * 
 * ci -u1.225 cl_isis.c history.c

 * Revision 1.224  89/07/19  15:28:25  siegel
 * added declaration for last_gettimeofday
 * 
 * ci -u1.224 cl_isis.c history.c

 * Revision 1.223  89/07/14  14:30:53  rcbc
 * Fix typechecking problem in CALL macro.
 * 
 * ci -u1.223 cl_task.h cl_coord.c history.c

 * Revision 1.222  89/07/14  11:26:41  rcbc
 * Changes for lucid port (merged from 1.213.1.1).
 * 
 * ci -u1.222 cl_isis.c history.c

 * Revision 1.221  89/07/14  11:14:33  rcbc
 * First checkin
 * 
 * ci -u1.221 isis-c-load.lisp isis-msg.lisp isis-task.lisp isis-tools.lisp isis.lisp make-lucid.lisp history.c

 * Revision 1.220  89/07/14  11:08:38  rcbc
 * Changes for lucid common lisp port. Includes fixes to the
 * allegro port.
 * 
 * ci -u1.220 makefile isis_c_refs.c isis.h isis.cl isis-tools.cl isis-task.cl isis-msg.cl isis-c-load.cl cl_xaction.h cl_xaction.c cl_task.h cl_task.c cl_coord.c history.c

 * Revision 1.219  89/07/13  15:37:52  ken
 * More fixes for Alex Siegel
 * 
 * ci -u1.219 cl_isis.c cl_queues.c history.c

 * Revision 1.218  89/07/13  15:00:01  ken
 * Alex's syndorm
 * 
 * ci -u1.218 cl_isis.c cl_alloc.c history.c

 * Revision 1.217  89/07/13  10:59:13  ken
 * ...
 * .y
 * 
 * ci -u1.217 flib.c history.c

 * Revision 1.216  89/07/13  10:14:47  ken
 * change from SIG_HUP to SIG_DUMP for dump signal
 * 
 * ci -u1.216 cl_isis.c history.c

 * Revision 1.215  89/07/12  22:05:09  ken
 * ...
 * 
 * ci -u1.215 cl_isis.c cl_bypass.c cl_bcast.c cl_join.c cl_pgroup.c cl_spool.c cl_watch.c g_parse.y spooler.h history.c

 * Revision 1.214  89/07/06  22:52:05  ken
 * Minor one
 * 
 * ci -u1.214 cl_spool.c history.c

 * Revision 1.213  89/07/06  22:36:25  ken
 * Various mods, spooler hacks
 * 
 * ci -u1.213 cl_bcast.c cl_groups.c cl_isis.c cl_join.c cl_spool.c cl_task.c spooler.h history.c

 * Revision 1.212  89/07/06  10:49:59  rcbc
 * Fixed setting of return codes in some routines.
 * 
 * ci -u1.212 cl_pgroup.c history.c

 * Revision 1.211  89/07/05  15:16:30  rcbc
 * Fixed bug in coord-cohort function.
 * 
 * ci -u1.211 isis-tools.cl history.c

 * Revision 1.210  89/07/05  15:13:39  rcbc
 * Added CALL macro for invoking user call back routines.
 * Provides a host language independent mechanism for user
 * call backs. Needed for Lucid lisp.
 * Also fixed bug in Fortran callbacks in cl_xaction.c
 * 
 * ci -u1.210 tk_authen.c cl_watch.c cl_task.h cl_task.c cl_sview.c cl_queues.h cl_pgroup.c cl_lmgr.c cl_join.c cl_isis.c cl_coord.c cl_xaction.c history.c

 * Revision 1.209  89/07/05  13:10:11  ken
 * ...
 * 
 * ci -u1.209 makefile history.c

 * Revision 1.208  89/07/03  20:15:33  ken
 * July 4th checkpoint of spooler code
 * 
 * ci -u1.208 cl_dump.c cl_isis.c cl_spool.c spooler.h history.c

 * Revision 1.207  89/07/03  14:59:30  rcbc
 * Removed duplicate get_sv_list and get_sv_incarn
 * 
 * ci -u1.207 isis-c-load.cl history.c

 * Revision 1.206  89/06/30  14:50:16  rcbc
 * Typo in exports list.
 * 
 * ci -u1.206 isis-c-load.cl history.c

 * Revision 1.205  89/06/29  11:18:01  ken
 * oops...
 * 
 * ci -u1.205 cl_sview.c history.c

 * Revision 1.204  89/06/27  11:43:19  ken
 * oops
 * 
 * ci -u1.204 cl_isis.c history.c

 * Revision 1.203  89/06/27  10:37:33  ken
 * ...
 * 
 * ci -u1.203 cl_spool.c spooler.h history.c

 * Revision 1.202  89/06/26  15:36:22  ken
 * ...
 * 
 * ci -u1.202 cl_spool.c history.c

 * Revision 1.201  89/06/26  15:13:15  ken
 * ...
 * 
 * ci -u1.201 cl_spool.c history.c

 * Revision 1.200  89/06/26  13:26:58  ken
 * ...
 * 
 * ci -u1.200 cl_sview.c history.c

 * Revision 1.199  89/06/26  13:02:46  ken
 * ...
 * 
 * ci -u1.199 isis.h history.c

 * Revision 1.198  89/06/26  10:27:39  ken
 * ...
 * 
 * ci -u1.198 cl_sview.c history.c

 * Revision 1.197  89/06/23  15:19:47  ken
 * Spooler related changes
 * 
 * ci -u1.197 spooler.h isis.h cl_spool.c cl_join.c history.c

 * Revision 1.196  89/06/22  16:28:04  ken
 * ...
 * 
 * ci -u1.196 cl_isis.c history.c

 * Revision 1.195  89/06/22  16:23:40  ken
 * Correct problem with detection of protocols termination
 * 
 * ci -u1.195 cl_isis.c history.c

 * Revision 1.194  89/06/22  11:09:15  ken
 * ....
 * 
 * ci -u1.194 cl_isis.c history.c

 * Revision 1.193  89/06/22  11:08:38  ken
 * ...
 * 
 * ci -u1.193 cl_spool.c spooler.h history.c

 * Revision 1.192  89/06/20  15:31:40  ken
 * ...
 * 
 * ci -u1.192 cl_task.c history.c

 * Revision 1.191  89/06/20  12:15:49  ken
 * ...
 * 
 * ci -u1.191 cl_coord.c history.c

 * Revision 1.190  89/06/16  16:49:13  ken
 * y
 * ...
 * 
 * ci -u1.190 cl_isis.c history.c

 * Revision 1.189  89/06/16  15:52:31  ken
 * ...
 * 
 * ci -u1.189 cl_spool.c makefile spooler.h history.c

 * Revision 1.188  89/06/15  13:53:29  ken
 * ...
 * y
 * 
 * ci -u1.188 tk_rmgr.c history.c

 * Revision 1.187  89/06/15  13:38:11  ken
 * ...
 * 
 * ci -u1.187 cl_join.c g_parse.y history.c

 * Revision 1.186  89/06/15  12:25:03  ken
 * ...
 * 
 * ci -u1.186 cl_spool.c spooler.h history.c

 * Revision 1.185  89/06/13  15:21:24  ken
 * y
 * cd ../util
 * checkin spooler.c
 * ....
 * 
 * ci -u1.185 spooler.h history.c

 * Revision 1.184  89/06/13  15:21:05  ken
 * ...
 * 
 * ci -u1.184 cl_spool.c history.c

 * Revision 1.183  89/06/12  16:07:16  ken
 * abort() has no args...
 * 
 * ci -u1.183 cl_task.c history.c

 * Revision 1.182  89/06/09  11:26:06  ken
 * V1.2 header added, attribute code
 * 
 * ci -u1.182 cl_alloc.c cl_bcast.c cl_bypass.c cl_cmd.h cl_coord.c cl_dump.c cl_failed.c cl_groups.c cl_groupview.h cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_lmgr.c cl_lmgr.h cl_news.c cl_news.h cl_pgroup.c cl_print.c cl_queues.c cl_queues.h cl_rmgr.h cl_setjmp.c cl_setjmp.s cl_sundummy.c cl_sview.c cl_task.c cl_task.h cl_token.c cl_typedefs.h cl_watch.c cl_xaction.c cl_xaction.h fisis.h fix flib.c g_eval.c g_parse.y guards.h isis-c-load.cl isis-msg.cl isis-task.cl isis-tools.cl isis.cl isis.h isis_c_refs.c jmp_pragmas.h ken lisp_lib.c make-allegro.cl makefile sview_init.c tk_authen.c tk_rexec.c tk_rmgr.c tom history.c

 * Revision 1.181  89/06/09  09:41:56  ken
 * ...
 * 
 * ci -u1.181 cl_spool.c history.c

 * Revision 1.180  89/06/07  15:37:58  ken
 * ...
 * 
 * ci -u1.180 makefile history.c

 * Revision 1.179  89/06/07  15:13:33  ken
 * MIPS change
 * 
 * ci -u1.179 makefile history.c

 * Revision 1.178  89/06/07  15:10:51  ken
 * the usual static/extern business
 * 
 * ci -u1.178 cl_watch.c history.c

 * Revision 1.177  89/06/07  15:08:56  ken
 * Add MIPS stuff
 * 
 * ci -u1.177 isis.h cl_task.c history.c

 * Revision 1.176  89/06/07  14:26:27  ken
 * ...
 * 
 * ci -u1.176 cl_isis.c history.c

 * Revision 1.175  89/06/07  14:25:53  rcbc
 * Changed machine dependent defines again. Now if you don't
 * specify -DSUN3 or whatever we make a few guesses based on 
 * builtin cpp defines.
 * 
 * ci -u1.175 isis.h history.c

 * Revision 1.174  89/06/07  14:21:01  ken
 * .intermediate checkin
 * 
 * ci -u1.174 cl_bcast.c cl_bypass.c cl_groups.c cl_inter.c cl_inter.h cl_isis.c cl_lmgr.c cl_queues.h cl_spool.c cl_xaction.c isis.h tk_rmgr.c history.c

 * Revision 1.173  89/06/02  15:40:53  rcbc
 * Bug fixes to isis-c-load.cl and isis-msg.cl, and added
 * lisp machine style mode line to all .cl files.
 * 
 * ci -u1.173 isis-c-load.cl isis-msg.cl isis-task.cl isis-tools.cl isis.cl make-allegro.cl history.c

 * Revision 1.172  89/06/01  16:29:57  rcbc
 * Fixed syntax error.
 * 
 * ci -u1.172 cl_lmgr.c history.c

 * Revision 1.171  89/06/01  10:11:02  ken
 * Change lmgr to create logs in mode 666
 * 
 * ci -u1.171 cl_lmgr.c history.c

 * Revision 1.170  89/05/31  14:12:12  ken
 * arrange to call cthread_yield
 * 
 * ci -u1.170 cl_task.h history.c

 * Revision 1.169  89/05/30  20:17:59  ken
 * minor changes
 * 
 * ci -u1.169 cl_isis.c cl_spool.c history.c

 * Revision 1.168  89/05/30  15:19:08  ken
 * wait in clib for incarn if incarn unknown
 * 
 * ci -u1.168 cl_isis.c history.c

 * Revision 1.167  89/05/30  14:51:19  ken
 * fixes related to the mlib problem
 * 
 * ci -u1.167 cl_task.c cl_isis.c cl_bypass.c history.c

 * Revision 1.166  89/05/26  09:37:03  ken
 * ...
 * 
 * ci -u1.166 isis_c_refs.c history.c

 * Revision 1.165  89/05/26  09:36:01  ken
 * ...
 * 
 * ci -u1.165 isis_c_refs.c history.c

 * Revision 1.164  89/05/26  09:32:50  ken
 * various minor fixes and changes
 * 
 * ci -u1.164 cl_isis.c cl_inter.c cl_bypass.c history.c

 * Revision 1.163  89/05/25  17:52:25  rcbc
 * Ensured that compilation fails if you don't define one of SUN3, APOLLO etc.
 * 
 * ci -u1.163 isis.h history.c

 * Revision 1.162  89/05/25  17:49:38  rcbc
 * Ensured that compilation fails if you don't define one of SUN3, APOLLO etc.
 * 
 * ci -u1.162 isis.h history.c

 * Revision 1.161  89/05/25  17:15:48  rcbc
 * New define NOSUNLWP which avoids use of Sun lwp. If NOSUNLWP isn't
 * defined, then compilation on a Sun automatically uses lwp.
 * 
 * ci -u1.161 isis.h history.c

 * Revision 1.160  89/05/24  11:13:41  rcbc
 * SUN3 or SUN4 implies SUNLWP *unless* you define NOSUNLWP before
 * including isis.h
 * 
 * ci -u1.160 isis.h history.c

 * Revision 1.159  89/05/22  10:48:51  ken
 * More bypass bug fixes
 * 
 * ci -u1.159 cl_bypass.c history.c

 * Revision 1.158  89/05/19  16:36:23  ken
 * y
 * y
 * y
 * little typos
 * 
 * ci -u1.158 isis.h cl_inter.c cl_bypass.c history.c

 * Revision 1.157  89/05/19  16:14:24  ken
 * y
 * y
 * y
 * y
 * fix some bypass bugs
 * 
 * ci -u1.157 cl_inter.c cl_inter.h cl_bcast.c cl_bypass.c history.c

 * Revision 1.156  89/05/19  11:08:58  rcbc
 * Changed addr_isnull to _addr_isnull.
 * 
 * ci -u1.156 isis_c_refs.c isis-c-load.cl history.c

 * Revision 1.155  89/05/19  09:46:48  ken
 * detect malloc failures
 * 
 * ci -u1.155 cl_alloc.c history.c

 * Revision 1.154  89/05/19  09:18:10  ken
 * Garbage collect in /tmp...
 * 
 * ci -u1.154 cl_isis.c history.c

 * Revision 1.153  89/05/18  11:42:30  ken
 * oops
 * 
 * ci -u1.153 cl_isis.c history.c

 * Revision 1.152  89/05/18  11:09:34  ken
 * addr_isnull -> aptr_isnull
 * 
 * ci -u1.152 cl_bypass.c cl_coord.c cl_dump.c cl_inter.c cl_isis.c cl_join.c cl_pgroup.c cl_watch.c cl_xaction.c history.c

 * Revision 1.151  89/05/18  10:42:09  ken
 * various BYPASS fixes
 * 
 * ci -u1.151 cl_inter.c cl_bypass.c cl_bcast.c history.c

 * Revision 1.150  89/05/18  10:41:08  ken
 * fixes to support sun_grid properly
 * 
 * ci -u1.150 cl_isis.c isis.h history.c

 * Revision 1.149  89/05/17  14:32:49  ken
 * V1.2 alpha minor bug fixes
 * 
 * ci -u1.149 cl_isis.c history.c

 * Revision 1.148  89/05/17  09:54:11  ken
 * BYPASS code fixes
 * 
 * ci -u1.148 cl_bcast.c cl_bypass.c cl_inter.c cl_isis.c g_eval.c g_parse.y history.c

 * Revision 1.147  89/05/12  16:10:08  ken
 * fix isis_accept_events(0)
 * 
 * ci -u1.147 cl_isis.c cl_task.c history.c

 * Revision 1.146  89/05/12  16:08:45  ken
 * fix isis_accept_events(0)
 * 
 * ci -u1.146 isis.h cl_isis.c cl_dump.c cl_task.c history.c

 * Revision 1.145  89/05/12  14:58:18  rcbc
 * Initial version.
 * of allegro build file.
 * 
 * ci -u1.145 make-allegro.cl history.c

 * Revision 1.144  89/05/12  14:46:08  rcbc
 * Fixed references to clib and mlib binaries.
 * 
 * ci -u1.144 isis-c-load.cl history.c

 * Revision 1.143  89/05/12  14:29:56  ken
 * oops
 * 
 * ci -u1.143 cl_task.c history.c

 * Revision 1.142  89/05/12  14:22:43  ken
 * typo
 * 
 * ci -u1.142 cl_task.c history.c

 * Revision 1.141  89/05/12  14:10:28  ken
 * oops
 * 
 * ci -u1.141 cl_task.c history.c

 * Revision 1.140  89/05/12  13:52:53  ken
 * Oops
 * 
 * ci -u1.140 isis.h history.c

 * Revision 1.139  89/05/12  13:52:21  ken
 * Better solution to fork/vfork business
 * 
 * ci -u1.139 isis.h history.c

 * Revision 1.138  89/05/12  13:51:40  ken
 * fix lightweight problem on non-external systems
 * 
 * ci -u1.138 cl_isis.c history.c

 * Revision 1.137  89/05/12  13:50:11  ken
 * fix non-threads problem
 * 
 * ci -u1.137 cl_task.c history.c

 * Revision 1.136  89/05/12  12:46:25  ken
 * catch a fortran problem
 * 
 * ci -u1.136 cl_bcast.c history.c

 * Revision 1.135  89/05/12  10:29:00  rcbc
 * Fixes to lisp port and thread switching.
 * 
 * ci -u1.135 makefile lisp_lib.c cl_task.h cl_isis.c history.c

 * Revision 1.134  89/05/12  09:20:51  rcbc
 * Allegro Common Lisp interface, first checkin
 * 
 * ci -u1.134 isis-c-load.cl isis-msg.cl isis-task.cl isis-tools.cl isis.cl isis_c_refs.c history.c

 * Revision 1.133  89/05/12  09:16:05  ken
 * get rid of vfork/fork define (ugly and confusing)
 * 
 * ci -u1.133 isis.h history.c

 * Revision 1.132  89/05/11  21:16:48  ken
 * fix what seems to be a bug in non-lwp task imp.
 * 
 * ci -u1.132 cl_task.c history.c

 * Revision 1.131  89/05/11  21:05:24  ken
 * correct xbyref problem
 * 
 * ci -u1.131 cl_task.c history.c

 * Revision 1.130  89/05/11  14:38:05  ken
 * catch a silly problem...
 * 
 * ci -u1.130 cl_bcast.c history.c

 * Revision 1.129  89/05/11  14:32:05  ken
 * various lightweight changes
 * 
 * ci -u1.129 isis.h cl_task.c cl_task.h flib.c cl_isis.c history.c

 * Revision 1.128  89/05/10  16:29:20  ken
 * fixing bugs in lightweight stuff
 * 
 * ci -u1.128 cl_isis.c cl_task.c cl_task.h history.c

 * Revision 1.127  89/05/10  10:02:34  rcbc
 * ISIS mainloop now waits forever on a condition variable
 * instead of looping calling run_isis.
 * 
 * ci -u1.127 cl_isis.c history.c

 * Revision 1.126  89/05/10  09:28:04  ken
 * improve error message
 * 
 * ci -u1.126 cl_bcast.c history.c

 * Revision 1.125  89/05/09  16:11:21  ken
 * Changes to support new tasking scheme for Allegro
 * 
 * ci -u1.125 cl_dump.c cl_isis.c cl_task.c cl_task.h isis.h history.c

 * Revision 1.124  89/05/08  16:28:12  ken
 * fix the isis_accept_events thing
 * 
 * ci -u1.124 cl_isis.c cl_task.c history.c

 * Revision 1.123  89/05/06  18:42:37  rcbc
 * Fixed problems in forked off scheduler task.
 * 
 * ci -u1.123 cl_task.c history.c

 * Revision 1.122  89/05/06  18:14:52  rcbc
 * Fixes for forked-off scheduler.
 * 
 * ci -u1.122 cl_task.h cl_task.c history.c

 * Revision 1.121  89/05/06  17:41:22  rcbc
 * Fixup of forked-off scheduler.
 * 
 * ci -u1.121 cl_isis.c history.c

 * Revision 1.120  89/05/05  10:23:37  ken
 * correct a call to bc_cancel
 * 
 * ci -u1.120 cl_bcast.c history.c

 * Revision 1.119  89/05/05  09:19:23  ken
 * fix some bypass bugs
 * 
 * ci -u1.119 cl_bypass.c history.c

 * Revision 1.118  89/05/04  14:51:47  ken
 * checkpoint of bypass code
 * 
 * ci -u1.118 cl_inter.c cl_bypass.c history.c

 * Revision 1.117  89/05/03  22:00:09  ken
 * fix some problems with lightweight tasks
 * 
 * ci -u1.117 cl_dump.c cl_lmgr.h cl_task.c cl_task.h history.c

 * Revision 1.116  89/05/03  14:57:27  ken
 * "fix" the isis_enter bug
 * 
 * ci -u1.116 cl_task.c cl_task.h cl_isis.c cl_lmgr.h cl_dump.c history.c

 * Revision 1.115  89/05/03  11:24:24  ken
 * initialize entris count to 1
 * 
 * ci -u1.115 cl_task.c history.c

 * Revision 1.114  89/05/03  11:14:51  rcbc
 * removed comment.
 * 
 * ci -u1.114 cl_task.h history.c

 * Revision 1.113  89/05/03  08:54:01  ken
 * remove spurious call to bzero
 * 
 * ci -u1.113 flib.c history.c

 * Revision 1.112  89/05/02  15:58:35  rcbc
 * Added make_condition
 * 
 * ci -u1.112 lisp_lib.c history.c

 * Revision 1.111  89/05/02  09:31:11  ken
 * Eliminate a minor bug in cl_setscope
 * 
 * ci -u1.111 cl_isis.c history.c

 * Revision 1.110  89/05/01  17:59:04  rcbc
 * isis_start_done forks off a scheduler if isis_mainloop hasn't been called.
 * 
 * ci -u1.110 cl_isis.c history.c

 * Revision 1.109  89/05/01  17:04:13  ken
 * eliminate unused task entry
 * 
 * ci -u1.109 cl_task.h cl_task.c history.c

 * Revision 1.108  89/05/01  16:40:12  ken
 * Apollo exit has problems
 * 
 * ci -u1.108 isis.h history.c

 * Revision 1.107  89/05/01  16:07:11  ken
 * extra &
 * 
 * ci -u1.107 cl_inter.c history.c

 * Revision 1.106  89/05/01  14:55:27  rcbc
 * Fixed defines for APOLLO threads.
 * 
 * ci -u1.106 cl_task.h history.c

 * Revision 1.105  89/05/01  14:54:25  rcbc
 * More functions for lisp to call.
 * 
 * ci -u1.105 lisp_lib.c history.c

 * Revision 1.104  89/05/01  13:56:58  ken
 * HP yacc has a variable yymaxdepth...
 * 
 * ci -u1.104 g_parse.y history.c

 * Revision 1.103  89/05/01  13:54:15  ken
 * add chwait stuff, fix isis_accept_events bug
 * 
 * ci -u1.103 cl_isis.c history.c

 * Revision 1.102  89/05/01  09:11:54  ken
 * Oops
 * 
 * ci -u1.102 cl_isis.c history.c

 * Revision 1.101  89/05/01  09:06:11  ken
 * add isis_signal_sig, placeholders for chwait
 * 
 * ci -u1.101 cl_isis.c history.c

 * Revision 1.100  89/04/30  06:02:01  ken
 * &gaddr on bcast calls...
 * 
 * ci -u1.100 cl_pgroup.c history.c

 * Revision 1.99  89/04/27  11:55:11  ken
 * Apollo in UNIX_DOM
 * 
 * ci -u1.99 isis.h history.c

 * Revision 1.98  89/04/27  10:34:34  ken
 * oops...
 * 
 * ci -u1.98 makefile history.c

 * Revision 1.97  89/04/27  10:33:50  ken
 * make the apollo task stuff look worse to make it compile...
 * 
 * ci -u1.97 cl_task.h history.c

 * Revision 1.96  89/04/27  10:10:52  rcbc
 * mid-hack
 * 
 * ci -u1.96 cl_task.h history.c

 * Revision 1.95  89/04/27  09:52:59  ken
 * Add ISIS_BLOCK/non-block option to isis_accept_events
 * 
 * ci -u1.95 fisis.h flib.c isis.h cl_isis.c history.c

 * Revision 1.94  89/04/26  22:24:53  rcbc
 * Added comment.
 * 
 * ci -u1.94 isis.h history.c

 * Revision 1.93  89/04/26  22:24:03  rcbc
 * Removed spurious test for calling from lisp.
 * 
 * ci -u1.93 cl_bcast.c history.c

 * Revision 1.92  89/04/26  20:33:34  ken
 * implment $(CC) convention
 * 
 * ci -u1.92 makefile isis.h history.c

 * Revision 1.91  89/04/26  18:48:04  ken
 * make compatible with pr_queues.h
 * 
 * ci -u1.91 cl_queues.h history.c

 * Revision 1.90  89/04/26  15:54:56  ken
 * V1.2 cont.
 * 
 * ci -u1.90 cl_isis.c history.c

 * Revision 1.89  89/04/26  13:29:18  rcbc
 * Added cc_terminate_msg.
 * 
 * ci -u1.89 cl_coord.c history.c

 * Revision 1.88  89/04/26  10:17:09  ken
 * fix includefile reference
 * 
 * ci -u1.88 tk_rmgr.c history.c

 * Revision 1.87  89/04/26  09:57:53  ken
 * V1.2 master checkin
 * 
 * ci -u1.87 cl_bypass.c cl_pgroup.c cl_inter.c g_eval.c history.c

 * Revision 1.86  89/04/26  09:55:42  ken
 * V1.2 master checkin
 * 
 * ci -u1.86 sview_init.c tk_authen.c tk_rexec.c tk_rmgr.c history.c

 * Revision 1.85  89/04/26  09:54:24  ken
 * V1.2 master checkin
 * 
 * ci -u1.85 RCS cl_alloc.c cl_bcast.c cl_bypass.c cl_cmd.h cl_coord.c cl_dump.c cl_failed.c cl_groups.c cl_groupview.h cl_inter.c cl_inter.h cl_isis.c cl_join.c cl_lmgr.c cl_lmgr.h cl_news.c cl_news.h cl_pgroup.c cl_print.c cl_queues.c cl_queues.h cl_rmgr.h cl_setjmp.c cl_setjmp.s cl_sundummy.c cl_sview.c cl_task.c cl_task.h cl_token.c cl_typedefs.h cl_watch.c cl_xaction.c cl_xaction.h fisis.h flib.c g_eval.c g_parse.y guards.h history.c isis.h lib1.a lisp_lib.c makefile sview_init.c tk_authen.c tk_rexec.c tk_rmgr.c history.c

 * Revision 1.84  89/04/26  09:47:11  rcbc
 * First checkin of lisp_lib.c
 * 
 * ci -u1.84 lisp_lib.c history.c

 * Revision 1.83  89/04/26  09:07:12  rcbc
 * Changes for Allegro common lisp port.
 * Also address now passed/returned by reference instead of by-value.
 * 
 * ci -u1.83 cl_isis.c cl_task.c cl_task.h isis.h history.c

 * Revision 1.82  89/04/18  14:10:13  rcbc
 * Checkpoint in mid-hack of allegro common lisp changes.
 * 
 * ci -u1.82 -l cl_task.h cl_task.c history.c

 * Revision 1.81  89/04/18  09:55:58  rcbc
 * Added allegro CL option. Moved token tool into standard client
 * library.
 * 
 * ci -u1.81 makefile history.c

 * Revision 1.80  89/04/16  16:22:14  rcbc
 * In mid-hack.
 * 
 * ci -u1.80 -l cl_task.c history.c

 * Revision 1.79  89/04/11  11:17:58  rcbc
 * Checkpoint before adding common lisp support
 * 
 * ci -u1.79 cl_task.h cl_task.c history.c

 * Revision 1.78  89/04/10  12:59:44  rcbc
 * Malloced a condition to avoid pointers onto the stack.
 * 
 * ci -u1.78 cl_xaction.c history.c

 * Revision 1.77  89/04/07  11:06:06  rcbc
 * Changed transaction routines to return x_id* rather than x_ids
 * (related to address* changes).
 * 
 * ci -u1.77 cl_xaction.c cl_xaction.h cl_typedefs.h history.c

 * Revision 1.76  89/04/07  10:14:24  ken
 * *** empty log message ***
 * 
 * ci -u1.76 cl_bypass.c cl_inter.c history.c

 * Revision 1.75  89/04/07  10:12:35  ken
 * This is version V1.2
 * 
 * ci -u1.75 cl_failed.c history.c

 * Revision 1.74  89/04/07  10:11:44  ken
 * Revisions for ISIS V1.2
 * 
 * ci -u1.74 cl_cmd.h cl_groupview.h cl_inter.h cl_lmgr.h cl_news.h cl_queues.h cl_rmgr.h cl_task.h cl_typedefs.h cl_xaction.h fisis.h guards.h isis.h cl_alloc.c cl_bcast.c cl_bypass.c cl_coord.c cl_dump.c cl_failed.c cl_groups.c cl_inter.c cl_isis.c cl_join.c cl_lmgr.c cl_news.c cl_pgroup.c cl_print.c cl_queues.c cl_setjmp.c cl_sundummy.c cl_sview.c cl_task.c cl_token.c cl_watch.c cl_xaction.c flib.c g_eval.c history.c sview_init.c tk_authen.c tk_rexec.c tk_rmgr.c history.c

 * Revision 1.73  89/02/10  10:42:35  ken
 * eliminate some unused assignments
 * 
 * ci -u1.73 cl_inter.c cl_bypass.c cl_isis.c history.c

 * Revision 1.72  89/02/02  11:55:04  ken
 * MACH related changes
 * 
 * ci -u1.72 cl_isis.c history.c

 * Revision 1.71  89/01/31  11:06:21  rcbc
 * Now embeds rcs version number in library file.
 * 
 * ci -u1.71 makefile history.c

 * Revision 1.70  89/01/31  11:05:29  rcbc
 * Fixed rcsid declaration.
 * 
 * Revision 1.69  89/01/30  17:38:34  rcbc
 * No changes. Just testing new checkin program.
 * 
 * ci -u1.69 cl_xaction.c history.c

 * Revision 1.68  89/01/30  17:35:15  rcbc
 * Updated for new checkin program.
 * 
 * Revision 1.67  89/01/30  14:27:23  ken
 * COrrect more things that MACH noticed and cared about
 * 
 * ci -u1.67 cl_bcast.c cl_bypass.c cl_coord.c cl_inter.c cl_join.c cl_lmgr.c cl_setjmp.s cl_task.c cl_watch.c cl_xaction.c g_eval.c isis.h tk_token.c history

 * Revision 1.66  89/01/24  15:15:23  ken
 * MACH complained
 * 
 * ci -u1.66 tk_token.c history

 * Revision 1.65  89/01/24  15:02:31  ken
 * make MACH happy
 * 
 * ci -u1.65 cl_task.c isis.h history

 * Revision 1.64  89/01/24  15:01:59  ken
 * make MACH happy
 * 
 * ci -u1.64 cl_setjmp.s history

 * Revision 1.63  89/01/24  14:52:07  ken
 * MACH changes
 * 
 * ci -u1.63 g_eval.c history

 * Revision 1.62  89/01/24  14:45:53  ken
 * wouldn't compile under MACH
 * 
 * ci -u1.62 cl_bcast.c history

 * Revision 1.61  89/01/24  14:42:32  ken
 * delete duplicated declaration
 * 
 * ci -u1.61 cl_inter.h history

 * Revision 1.60  89/01/24  14:09:59  ken
 * MACH complained a bit too many times....
 * 
 * ci -u1.60 cl_isis.c history

 * Revision 1.59  89/01/13  09:04:50  ken
 * zero isis_enum after copying it
 * 
 * ci -u1.59 cl_task.c history

 * Revision 1.58  89/01/11  17:29:45  ken
 * fix another entry_stacksize problem
 * 
 * ci -u1.58 cl_task.c history

 * Revision 1.57  89/01/11  14:11:43  ken
 * catch case of enum < 0 in new entry_stacksize code
 * 
 * ci -u1.57 cl_task.c history

 * Revision 1.56  89/01/10  12:44:35  ken
 * increase isis_dir to 128 chars long
 * 
 * ci -u1.56 isis.h history

 * Revision 1.55  89/01/10  10:58:33  ken
 * automate check for varargs problem
 * 
 * ci -u1.55 cl_bcast.c cl_dump.c cl_inter.c cl_isis.c cl_task.c g_eval.c tk_rexec.c history

 * Revision 1.54  89/01/06  09:26:05  ken
 * Pragmas about control flow in setjmp, needed on SUN4
 * 
 * ci -u1.54 jmp_pragmas.h history

 * Revision 1.53  89/01/06  09:25:00  ken
 * Add OPTIM flag, fix misc. minor SUN4/HPUX problems
 * 
 * ci -u1.53 makefile cl_coord.c cl_task.c cl_setjmp.s cl_xaction.c jmp_pragma.h history

 * Revision 1.52  89/01/02  13:17:16  ken
 * force bourne shell
 * 
 * ci -u1.52 makefile history

 * Revision 1.51  89/01/02  11:09:12  ken
 * Oops
 * y
 * 
 * ci -u1.51 makefile history

 * Revision 1.50  89/01/02  11:07:19  ken
 * ensure that if exits with status 0
 * 
 * ci -u1.50 makefile history

 * Revision 1.49  89/01/02  11:05:50  ken
 * eliminate attempt to run ranlib on HPUX systems
 * 
 * ci -u1.49 makefile history

 * Revision 1.48  89/01/02  10:35:27  ken
 * fix a minor syntax thing that upsets HPUX
 * 
 * ci -u1.48 cl_inter.c cl_isis.c cl_join.c cl_news.c cl_pgroup.c cl_queues.h cl_sview.c cl_task.c cl_watch.c isis.h history

 * Revision 1.47  89/01/02  10:16:02  ken
 * eliminate a minor HPUX complaint
 * 
 * ci -u1.47 tk_authen.c history

 * Revision 1.46  88/12/22  10:26:45  rcbc
 * Fixed null pointer bug.
 * 
 * ci -u1.46 cl_xaction.c history

 * Revision 1.45  88/12/22  09:14:24  ken
 * Final tuning on VAX setjmp/longjmp
 * 
 * ci -u1.45 cl_setjmp.s history

 * Revision 1.44  88/12/21  16:21:48  ken
 * increase amount of padding just in case....
 * 
 * ci -u1.44 cl_task.h history

 * Revision 1.43  88/12/21  15:42:57  ken
 * must save registers now
 * 
 * ci -u1.43 cl_setjmp.s history

 * Revision 1.42  88/12/21  15:32:54  ken
 * Compiler bug more common then I imagined
 * 
 * ci -u1.42 cl_queues.h history

 * Revision 1.41  88/12/19  14:05:10  rcbc
 * Turned off debug mode in compiles. 
 * 
 * ci -u1.41 makefile history

 * Revision 1.40  88/12/19  13:59:44  rcbc
 * Coordinator now calls cleanup at end of transaction, to cover case
 * where there is no participant at the coordinator's process.
 * Also turned off debugging defines.
 * 
 * ci -u1.40 cl_xaction.c cl_xaction.h history

 * Revision 1.39  88/12/19  13:19:47  ken
 * ctp-> should be isis_ctp->
 * 
 * ci -u1.39 cl_task.c history

 * Revision 1.38  88/12/19  12:00:49  ken
 * fix switch on SUN4
 * 
 * ci -u1.38 cl_task.c history

 * Revision 1.37  88/12/19  11:15:37  ken
 * add control flow pragma to isis switch routine
 * 
 * ci -u1.37 cl_task.c history

 * Revision 1.36  88/12/19  09:37:36  ken
 * Edit in some HPUX spectrum changes and add APOLLO ifdef
 * 
 * ci -u1.36 isis.h cl_task.c cl_setjmp.s cl_alloc.c cl_dump.c history

 * Revision 1.35  88/12/16  15:40:47  ken
 * Correct a problem with the reuse of activity slots
 * 
 * ci -u1.35 cl_join.c history

 * Revision 1.34  88/12/16  13:34:44  ken
 * implement pg_join mutex
 * ./
 * 
 * ci -u1.34 cl_join.c history

 * Revision 1.33  88/12/15  09:15:09  ken
 * Include V1.1 disclaimer
 * 
 * ci -u1.33 g_parse.y history

 * Revision 1.32  88/12/15  09:12:27  ken
 * edit to include disclaimer and expand tabs
 * 
 * ci -u1.32 cl_setjmp.s history

 * Revision 1.31  88/12/15  09:10:04  ken
 * edit to expand tabs and insert V1.1 disclaimer
 * 
 * ci -u1.31 cl_fio.c cl_alloc.c cl_failed.c cl_news.c cl_print.c cl_queues.c cl_sview.c sview_init.c tk_authen.c tk_init.c tk_token.c cl_bypass.h cl_cmd.h cl_lmgr.h cl_news.h cl_qnode.h cl_rmgr.h guards.h history

 * Revision 1.30  88/12/15  09:04:55  ken
 * edit to expand tabs, include disclaimer
 * 
 * ci -u1.30 cl_alloc.c cl_bcast.c cl_bypass.c cl_coord.c cl_dump.c cl_failed.c cl_fio.c cl_groups.c cl_inter.c cl_isis.c cl_join.c cl_lmgr.c cl_news.c cl_pgroup.c cl_print.c cl_queues.c cl_sundummy.c cl_sview.c cl_task.c cl_watch.c cl_xaction.c g_eval.c sview_init.c tk_authen.c tk_init.c tk_rexec.c tk_rmgr.c tk_token.c cl_bypass.h cl_cmd.h cl_groupview.h cl_inter.h cl_lmgr.h cl_news.h cl_qnode.h cl_queues.h cl_rmgr.h cl_task.h cl_typedefs.h cl_xaction.h guards.h isis.h history

 * Revision 1.29  88/12/13  09:06:17  ken
 * fix syntax error
 * 
 * ci -u1.29 cl_isis.c history

 * Revision 1.28  88/12/13  09:05:44  ken
 * add error message
 * 
 * ci -u1.28 cl_isis.c history

 * Revision 1.27  88/12/13  08:43:39  ken
 * add isis_input_sig feature for ajei
 * 
 * ci -u1.27 cl_isis.c history

 * Revision 1.26  88/12/09  16:49:36  rcbc
 * Changed to presumed-abort scheme.
 * 
 * ci -u1.26 cl_xaction.c history

 * Revision 1.25  88/12/09  14:17:03  ken
 * improve message
 * 
 * ci -u1.25 cl_join.c history

 * Revision 1.24  88/12/09  13:50:43  rcbc
 * Fixed bug in dump_trans.
 * 
 * ci -u1.24 cl_xaction.c history

 * Revision 1.23  88/12/09  08:39:58  ken
 * testing check_cl_watch_queue
 * 
 * ci -u1.23 cl_isis.c history

 * Revision 1.22  88/12/08  16:57:16  rcbc
 * Fix null pointer in check_cl_watch_queue
 * 
 * ci -u1.22 cl_isis.c history

 * Revision 1.21  88/12/08  13:58:09  ken
 * minor typo
 * 
 * ci -u1.21 cl_isis.c history

 * Revision 1.20  88/12/08  13:47:30  ken
 * minor mods while working on check_cl_watch_queue stuff
 * 
 * ci -u1.20 cl_task.c cl_queues.h history

 * Revision 1.19  88/12/08  13:46:28  ken
 * more changes to cl_check_watch_queue
 * 
 * ci -u1.19 cl_isis.c history

 * Revision 1.18  88/12/08  13:35:47  rcbc
 * Changed error message.
 * 
 * ci -u1.18 cl_lmgr.c history

 * Revision 1.17  88/12/08  13:10:36  ken
 * make check_cl_watch_queue a bit more paranoid
 * 
 * ci -u1.17 cl_isis.c history

 * Revision 1.16  88/12/08  12:02:19  ken
 * fix problems with check_cl_watch_queue
 * 
 * ci -u1.16 cl_join.c cl_groups.c history

 * Revision 1.15  88/12/08  12:01:38  ken
 * fix problems with check_cl_watch_queue
 * 
 * ci -u1.15 cl_watch.c cl_isis.c isis.h cl_groupview.h history

 * Revision 1.14  88/12/08  11:13:34  ken
 * call isis_task on do_pmonitor
 * 
 * ci -u1.14 cl_watch.c history

 * Revision 1.13  88/12/07  13:42:37  rcbc
 * Changed assembly of setjmp.s
 * 
 * ci -u1.13 makefile history

 * Revision 1.12  88/12/07  11:31:10  ken
 * get rid of a debug message
 * 
 * ci -u1.12 cl_isis.c history

 * Revision 1.11  88/12/07  10:31:59  rcbc
 * Checkin of makefile
 * 
 * ci -u1.11 -l makefile history

 * Revision 1.10  88/12/06  16:57:11  rcbc
 * checkpoint.
 * 
 * ci -u1.10 cl_xaction.h history

 * Revision 1.9  88/12/06  16:45:57  rcbc
 * Added entries for recovery manager.
 * 
 * ci -u1.9 cl_xaction.h history

 * Revision 1.8  88/11/30  20:24:03  rcbc
 * Added transaction mechanism.
 * 
 * ci -u1.8 cl_typedefs.h cl_xaction.c cl_xaction.h history

 * Revision 1.7  88/11/30  15:57:26  ken
 * fix pg_monitor problem after a join
 * 
 * ci -u1.7 cl_pgroup.c history

 * Revision 1.6  88/11/30  14:56:29  ken
 * improve handling of case where pg_leave called during a coord_cohort comp.
 * 
 * ci -u1.6 cl_coord.c history

 * Revision 1.5  88/11/30  14:56:04  ken
 * improve handling of case where pg_leave called during a coord_cohort comp.
 * 
 * ci -u1.5 cl_isis.c cl_join.c cl_pgroup.c tk_token.c history

 * 
 * Revision 1.4  88/11/29  08:44:17  ken
 * improve clib "wait" messages (prepend "isis system: .....")
 * 
 * ci -u1.4 cl_bcast.c cl_bypass.c cl_coord.c cl_isis.c cl_join.c cl_task.c cl_watch.c g_eval.c tk_rexec.c tk_rmgr.c history

 * Revision 1.3  88/11/28  13:55:02  ken
 * improve "wait" dump message
 * 
 * ci -u1.3 cl_dump.c history

 * Revision 1.2  88/11/28  12:54:06  ken
 * change t_wait to include descriptive message
 * 
 * ci -u1.2 cl_alloc.c cl_bcast.c cl_coord.c cl_dump.c cl_failed.c cl_fio.c cl_isis.c cl_join.c cl_news.c cl_pgroup.c cl_print.c cl_queues.c cl_sview.c cl_task.c cl_watch.c sview_init.c tk_authen.c tk_init.c tk_rexec.c tk_rmgr.c tk_token.c cl_cmd.h cl_groupview.h cl_news.h cl_qnode.h cl_queues.h cl_rmgr.h cl_task.h cl_typedefs.h cl_groups.c g_eval.c isis.h guards.h cl_inter.h cl_lmgr.c cl_bypass.c cl_lmgr.h cl_bypass.h cl_inter.c cl_sundummy.c cl_xaction.c cl_xaction.h history

 * Revision 1.1  88/11/21  10:59:25  rcbc
 * checkpoint in midst of building release-1-1
 * 
 * Revision 1.0  88/11/21  10:44:43  rcbc
 * Check-in of ~isis/distrib
 * 
 */
