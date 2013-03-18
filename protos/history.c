/* History file for protos */
char protos_rcsid[] = "$Revision: 2.26 $$Date: 90/09/18 14:33:44 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/protos/RCS/history.c,v $";

/* $Log:	history.c,v $
 * Revision 2.26  90/09/18  14:33:44  ken
 * Change mode to 0x666
 * 
 * ci -u2.26 pr_client.c history.c

 * Revision 2.25  90/09/18  08:38:26  ken
 * Minor changes for HP, SGI
 * 
 * ci -u2.25 makefile history.c

 * Revision 2.24  90/09/12  13:28:00  ken
 * SGI port
 * 
 * ci -u2.24 makefile pr.h pr_task.c history.c

 * Revision 2.23  90/09/07  16:46:01  ken
 * HPUX optimizer has problems with longjmp
 * 
 * ci -u2.23 makefile history.c

 * Revision 2.22  90/09/07  16:39:09  ken
 * Message leak when task wasn't waiting for this reply
 * 
 * ci -u2.22 pr_mcast.c history.c

 * Revision 2.21  90/08/15  09:50:49  ken
 * Correct memory leak (qnode called cb_ids)
 * 
 * ci -u2.21 pr_astore.c pr_cbcast.c pr_init.c history.c

 * Revision 2.20  90/08/14  10:45:37  ken
 * Set socket opts in consistent way
 * 
 * ci -u2.20 pr_inter.c history.c

 * Revision 2.19  90/08/13  16:07:19  ken
 * 100% sure this will work...
 * 
 * ci -u2.19 pr_inter.c pr_inter.h history.c

 * Revision 2.18  90/08/12  20:45:16  ken
 * Oops
 * 
 * ci -u2.18 pr_cbcast.c history.c

 * Revision 2.17  90/08/12  20:37:46  ken
 * Less ominous message
 * 
 * ci -u2.17 pr_main.c history.c

 * Revision 2.16  90/08/12  20:32:22  ken
 * Possible cause of the slow cbcast qnode leak
 * 
 * ci -u2.16 pr_astore.c pr_cbcast.c history.c

 * Revision 2.15  90/08/10  14:12:49  ken
 * is_seqn is a short, not a char!
 * 
 * ci -u2.15 pr_inter.c history.c

 * Revision 2.14  90/08/08  11:16:10  tclark
 * Gould, MACH would have had problems on this
 * 
 * ci -u2.14 pr_setjmp.s makefile history.c

 * Revision 2.13  90/08/06  13:48:04  ken
 * AUX changes
 * 
 * ci -u2.13 pr.h pr_dump.c pr_init.c pr_setjmp.s pr_task.c history.c

 * Revision 2.12  90/08/03  10:20:24  ken
 * tyop
 * 
 * ci -u2.12 pr_address.h history.c

 * Revision 2.11  90/08/03  09:28:13  ken
 * Improve clib printout of protos entry point names
 * 
 * ci -u2.11 pr_address.h history.c

 * Revision 2.10  90/08/02  18:05:12  rcbc
 * Fixed handling of short vs. long hostnames to be the same as in isis.c
 * 
 * ci -u2.10 pr_main.c history.c

 * Revision 2.9  90/08/01  16:55:08  ken
 * RT43 changes
 * 
 * ci -u2.9 makefile history.c

 * Revision 2.8  90/08/01  16:54:11  ken
 * RT43 changes
 * 
 * ci -u2.8 pr.h pr_task.c history.c

 * Revision 2.7  90/08/01  15:26:31  ken
 * isis_nsent incorrectly limited to nwanted
 * 
 * ci -u2.7 pr_mcast.c history.c

 * Revision 2.6  90/07/31  14:00:10  ken
 * Changes for AIX/RS 6000
 * 
 * ci -u2.6 pr_setjmp.s pr_global.h pr.h pr_task.c pr_gbcast.c history.c

 * Revision 2.5  90/07/25  13:47:47  ken
 * V2.1 bypass-related changes
 * 
 * ci -u2.5 pr_global.h pr_inter.c pr_inter.h pr_main.c history.c

 * Revision 2.4  90/07/06  14:37:57  ken
 * prepend isis_dir to logfile name
 * 
 * ci -u2.4 pr_main.c history.c

 * Revision 2.3  90/06/24  15:15:48  rcbc
 * Fixed C++ patches for AT&T C++ 1.2
 * 
 * ci -u2.3 pr_client.h history.c

 * Revision 2.2  90/06/20  14:39:29  ken
 * Change error message (only print once)
 * 
 * ci -u2.2 pr_fdect.c pr_inter.c pr_main.c history.c

 * Revision 2.1  90/06/11  10:16:03  ken
 * Fix management of free mem info and also panic on startup with xxx/1 stuff
 * 
 * ci -u2.1 pr_main.c pr_queues.h history.c

 * Revision 2.0  90/05/04  15:21:37  rcbc
 * 2.0
 * 
 * Revision 1.266  90/05/03  23:34:22  ken
 * y
 * y
 * y
 * cd ../mlib
 * vi msg_types.c
 * 
 * ci -u1.266 pr_pgroups.h history.c

 * Revision 1.265  90/05/02  18:28:33  rcbc
 * More typechecking changes for MACH/gcc.
 * 
 * ci -u1.265 pr_gbcast.c history.c

 * Revision 1.264  90/05/02  16:22:02  rcbc
 * MACH/gcc type checking problems.
 * 
 * ci -u1.264 pr_setjmp.s pr.h pr_gbcast.c pr_inter.c history.c

 * Revision 1.263  90/05/01  10:33:43  ken
 * V2.0 copyright notice
 * 
 * ci -u1.263 OPT_LEVEL_1.h bits.h generic.h isis_errno.h pr.h pr_abcast.h pr_address.h pr_astore.h pr_cbcast.h pr_client.h pr_errors.h pr_fdect.h pr_gbcast.h pr_global.h pr_glocks.h pr_inter.h pr_intersite.h pr_msgfields.h pr_msgflags.h pr_msgtank.h pr_pgroups.h pr_qnode.h pr_queues.h pr_stats.h pr_task.h pr_typedefs.h pr_wqueues.h pr_abcast.c pr_addr.c pr_astore.c pr_cbcast.c pr_clentry.c pr_client.c pr_dlist.c pr_dump.c pr_fdect.c pr_gbcast.c pr_glocks.c pr_gmgr.c pr_init.c pr_inter.c pr_local.c pr_main.c pr_mcast.c pr_msgtank.c pr_queues.c pr_setjmp.c pr_task.c pr_viewlocks.c pr_watch.c pr_setjmp.s history.c

 * Revision 1.262  90/04/27  14:37:25  ken
 * Copyright notices
 * 
 * ci -u1.262 history.c history.c

 * Revision 1.261  90/04/27  14:34:34  ken
 * Copyright notice
 * 
 * ci -u1.261 OPT_LEVEL_1.h RCS bits.h generic.h history.c isis_errno.h makefile pr.h pr_abcast.c pr_abcast.h pr_addr.c pr_address.h pr_astore.c pr_astore.h pr_cbcast.c pr_cbcast.h pr_clentry.c pr_client.c pr_client.h pr_dlist.c pr_dump.c pr_errors.h pr_fdect.c pr_fdect.h pr_gbcast.c pr_gbcast.h pr_global.h pr_glocks.c pr_glocks.h pr_gmgr.c pr_init.c pr_inter.c pr_inter.h pr_intersite.h pr_local.c pr_main.c pr_mcast.c pr_msgfields.h pr_msgflags.h pr_msgtank.c pr_msgtank.h pr_pgroups.h pr_qnode.h pr_queues.c pr_queues.h pr_setjmp.c pr_setjmp.s pr_stats.h pr_task.c pr_task.h pr_typedefs.h pr_viewlocks.c pr_watch.c pr_wqueues.h history.c

 * Revision 1.260  90/04/26  16:38:22  ken
 * Eliminate a silly source of core images
 * 
 * ci -u1.260 pr_main.c history.c

 * Revision 1.259  90/04/26  09:24:18  rcbc
 * Changed names of library files.
 * 
 * ci -u1.259 makefile history.c

 * Revision 1.258  90/04/25  11:11:42  ken
 * use _LO for these bits
 * 
 * ci -u1.258 pr_init.c history.c

 * Revision 1.257  90/04/25  11:11:08  ken
 * Improve statistics
 * 
 * ci -u1.257 pr_queues.h history.c

 * Revision 1.256  90/04/20  16:21:47  ken
 * Fix Alex's termination bug.
 * 
 * ci -u1.256 rcsdiff pr_client.c pr_gmgr.c pr_local.c pr_mcast.c history.c

 * Revision 1.255  90/04/20  14:45:40  ken
 * Oops
 * 
 * ci -u1.255 pr_inter.c history.c

 * Revision 1.254  90/04/20  14:23:25  ken
 * Various changes`
 * 
 * ci -u1.254 pr_clentry.c pr_client.c pr_dump.c pr_init.c pr_inter.c pr_main.c pr_stats.h history.c

 * Revision 1.253  90/04/18  16:30:11  ken
 * ...
 * 
 * ci -u1.253 pr_client.c history.c

 * Revision 1.252  90/04/17  15:14:18  ken
 * Scope seems to be an endless source of hassle...
 * 
 * ci -u1.252 pr_clentry.c history.c

 * Revision 1.251  90/04/17  13:29:31  ken
 * Various changes
 * 
 * ci -u1.251 pr_address.h pr_client.c pr_init.c pr_ pr_local.c history.c

 * Revision 1.250  90/04/16  09:11:07  ken
 * Weekend bug fixes
 * 
 * ci -u1.250 pr_client.c pr_client.h pr_main.c history.c

 * Revision 1.249  90/04/13  14:56:11  ken
 * V2.0 fixes
 * 
 * ci -u1.249 pr_main.c history.c

 * Revision 1.248  90/04/11  09:58:22  rcbc
 * Changes for make install style makefile.
 * 
 * ci -u1.248 makefile history.c

 * Revision 1.247  90/04/06  14:23:15  ken
 * V2.0 bug fixes.
 * 
 * ci -u1.247 pr_client.c pr_dump.c pr_inter.c pr_main.c history.c

 * Revision 1.246  90/04/05  17:02:56  ken
 * make exceptions look like ready input
 * 
 * ci -u1.246 pr_main.c history.c

 * Revision 1.245  90/04/05  16:41:48  ken
 * ...
 * 
 * ci -u1.245 pr_client.c history.c

 * Revision 1.244  90/04/05  14:55:28  ken
 * print more niceluy
 * 
 * ci -u1.244 pr_dump.c history.c

 * Revision 1.243  90/04/05  14:55:02  ken
 * Oops
 * 
 * ci -u1.243 pr_cbcast.c history.c

 * Revision 1.242  90/04/05  11:10:39  ken
 * Various isis_remote-related changes
 * 
 * ci -u1.242 pr_client.c pr_dump.c pr_main.c history.c

 * Revision 1.241  90/04/02  14:48:13  ken
 * V2.0 bug fix and other minor changes
 * 
 * ci -u1.241 bits.h pr_address.h pr_astore.c pr_client.c pr_cbcast.c pr_client.h pr_dump.c pr_init.c history.c

 * Revision 1.240  90/03/22  11:48:15  rcbc
 * Ken Kane's extensions to the log mgr.
 * 
 * ci -u1.240 pr_errors.h history.c

 * Revision 1.239  90/03/18  12:48:48  kane
 * Added IE_OLDLOG error to signal out-of-date log format
 * 
 * ci -u1.239 pr_errors.h history.c

 * Revision 1.238  90/03/15  19:14:08  rcbc
 * Added RCSID.
 * 
 * ci -u1.238 pr_cbcast.c OPT_LEVEL_1.h history.c

 * Revision 1.237  90/03/14  10:00:22  ken
 * add IE_EMPTY
 * 
 * ci -u1.237 pr_errors.h history.c

 * Revision 1.236  90/03/13  10:00:42  ken
 * error in building cl_task.c on SUN3/SUN4
 * 
 * ci -u1.236 makefile history.c

 * Revision 1.235  90/03/09  11:54:54  ken
 * not an improvement!
 * 
 * ci -u1.235 pr_main.c pr_inter.c history.c

 * Revision 1.234  90/03/09  11:21:07  ken
 * Whew!
 * 
 * ci -u1.234 makefile pr_setjmp.s pr_task.c history.c

 * Revision 1.233  90/03/09  11:20:46  ken
 * co -l history.c
 * Disable debug flag
 * 
 * ci -u1.233 pr_gmgr.c c history.c

 * Revision 1.232  90/03/08  16:25:50  ken
 * ...`
 * 
 * ci -u1.232 pr_task.c history.c

 * Revision 1.231  90/03/08  14:10:23  ken
 * y
 * y
 * y
 * cd ../clib
 * co -l cl_task.c
 * vi cl_task.c
 * 
 * ci -u1.231 pr_task.c history.c

 * Revision 1.230  90/03/08  13:44:44  ken
 * Change SUN4 to use setjmp/longjmp throughout
 * 
 * ci -u1.230 makefile pr_setjmp.s pr_task.c history.c

 * Revision 1.229  90/03/08  09:59:43  ken
 * Muck up setjmp/longjmp.s
 * 
 * ci -u1.229 pr_setjmp.s history.c

 * Revision 1.228  90/03/07  22:39:28  ken
 * These changes improve the handling of rapid restart/failure sequences
 * 
 * ci -u1.228 pr_inter.c pr_fdect.c history.c

 * Revision 1.227  90/03/06  15:53:43  ken
 * Various minor changes
 * 
 * ci -u1.227 pr_client.c pr_dump.c pr_gbcast.c pr_gmgr.c pr_inter.c pr_main.c pr_mcast.c pr_task.c history.c

 * Revision 1.226  90/03/06  15:52:47  ken
 * ...
 * 
 * ci -u1.226 pr_client.c history.c

 * Revision 1.225  90/02/20  15:39:23  ken
 * ...
 * 
 * ci -u1.225 generic.h history.c

 * Revision 1.224  90/02/20  15:34:04  ken
 * y
 * y
 * 
 * ci -u1.224 generic.h pr_inter.c history.c

 * Revision 1.223  90/02/19  22:17:44  patrick
 * Moved some variables from pr_fdect.h to pr_fdect.c
 * 
 * ci -u1.223 pr_fdect.c pr_fdect.h history.c

 * Revision 1.222  90/02/19  14:14:25  ken
 * ..
 * 
 * ci -u1.222 pr_inter.c history.c

 * Revision 1.221  90/02/19  14:12:31  ken
 * y
 * y
 * y
 * 
 * ci -u1.221 pr_inter.c history.c

 * Revision 1.220  90/02/16  17:32:14  rcbc
 * Changed to include generic.h instead of pr_generic.h.
 * 
 * ci -u1.220 pr_address.h history.c

 * Revision 1.219  90/02/16  11:59:49  patrick
 * 
 * 
 * ci -u1.219 pr_address.h pr_generic.h history.c

 * Revision 1.218  90/02/12  13:36:37  ken
 * ...
 * 
 * ci -u1.218 pr_addr.c history.c

 * Revision 1.217  90/02/08  10:49:46  ken
 * ...
 * 
 * ci -u1.217 bits.h history.c

 * Revision 1.216  90/02/07  16:23:09  ken
 * do_msg_)delete -> MSG_DELETE
 * 
 * ci -u1.216 pr_client.c pr_abcast.c pr_gbcast.c history.c

 * Revision 1.215  90/02/06  14:50:59  ken
 * ...
 * 
 * ci -u1.215 generic.h pr_client.c pr_dump.c history.c

 * Revision 1.214  90/01/30  20:45:08  ken
 * address alignment fix
 * 
 * ci -u1.214 generic.h pr_abcast.c pr_addr.c pr_astore.c pr_cbcast.c pr_clentry.c pr_client.c pr_dlist.c pr_dump.c pr_fdect.c pr_gbcast.c pr_glocks.c pr_gmgr.c pr_init.c pr_inter.c pr_local.c pr_main.c pr_mcast.c pr_msgtank.c pr_queues.c pr_setjmp.c pr_task.c pr_viewlocks.c pr_watch.c pr_queues.h history.c

 * Revision 1.213  90/01/12  14:06:45  ken
 * V2.0 transport protocol change
 * 
 * ci -u1.213 generic.h history.c

 * Revision 1.212  90/01/11  13:45:57  ken
 * Typo
 * 
 * ci -u1.212 pr_cbcast.c history.c

 * Revision 1.211  90/01/09  14:20:28  ken
 * minor stuff
 * 
 * ci -u1.211 pr_cbcast.c pr_task.c pr_task.h makefile history.c

 * Revision 1.210  90/01/04  13:07:43  ken
 * Various minor V2.0 stuff
 * 
 * ci -u1.210 pr_client.c pr_init.c pr_inter.h history.c

 * Revision 1.209  89/12/15  13:35:16  ken
 * ISIS V2.0 major checkin
 * 
 * ci -u1.209 makefile pr_abcast.c pr_abcast.h pr_addr.c pr_address.h pr_astore.c pr_astore.h pr_cbcast.c pr_cbcast.h pr_clentry.c pr_client.c pr_client.h pr_gbcast.c pr_gbcast.h pr_global.h pr_glocks.c pr_glocks.h pr_gmgr.c pr_init.c pr_inter.c pr_inter.h pr_intersite.h pr_task.c pr_task.h pr_typedefs.h history.c

 * Revision 1.208  89/11/10  13:28:43  rcbc
 * Made opening of log file more robust. Now does a chmod or deletes
 * the log file if it can't open it for writing.
 * 
 * ci -u1.208 pr_main.c history.c

 * Revision 1.207  89/11/06  16:37:50  ken
 * V2.0 mlib changes
 * 
 * ci -u1.207 pr_addr.c pr_astore.c pr_clentry.c pr_client.c pr_dump.c pr_gmgr.c pr_inter.c pr_local.c pr_mcast.c history.c

 * Revision 1.206  89/10/26  12:13:57  ken
 * ISIS V2.0 mlib changes
 * 
 * ci -u1.206 pr_addr.c pr_clentry.c pr_client.c pr_client.h pr_dump.c pr_gmgr.c pr_inter.c pr_mcast.c history.c

 * Revision 1.205  89/10/05  20:44:49  ken
 * Add BY_WAKEUP
 * 
 * ci -u1.205 generic.h history.c

 * Revision 1.204  89/09/26  11:38:37  ken
 * Speed up hellos when -f10 is used
 * 
 * ci -u1.204 pr_inter.c history.c

 * Revision 1.203  89/09/26  11:29:50  ken
 * ....
 * 
 * ci -u1.203 pr_main.c history.c

 * Revision 1.202  89/09/26  09:15:33  ken
 * Sledgehammer Apollo exit() bug
 * 
 * ci -u1.202 pr.h history.c

 * Revision 1.201  89/09/25  09:06:26  ken
 * y
 * y
 * y
 * y
 * y
 * cd ../clib
 * 
 * ci -u1.201 OPT_LEVEL_1.h pr_dump.c pr_task.c history.c

 * Revision 1.200  89/09/22  14:15:36  ken
 * Whew!
 * 
 * ci -u1.200 pr_dump.c pr_task.c pr_gbcast.c pr_fdect.c pr_cbcast.c history.c

 * Revision 1.199  89/09/18  14:58:06  ken
 * Fix packet size limits
 * 
 * ci -u1.199 pr_inter.h history.c

 * Revision 1.198  89/09/18  12:33:12  ken
 * Better debug outpout
 * 
 * ci -u1.198 pr_cbcast.c history.c

 * Revision 1.197  89/09/14  12:57:18  rcbc
 * Now uses FUN_TYPES define to choose between old and new style function
 * declarations. FUN_TYPES is defined for ANSI C compilers we "like".
 * 
 * ci -u1.197 isis_alloc.h pr.h history.c

 * Revision 1.196  89/09/13  14:33:51  ken
 * ...
 * 
 * ci -u1.196 pr_client.c history.c

 * Revision 1.195  89/09/13  09:59:44  ken
 * VOID
 * 
 * ci -u1.195 pr_abcast.c pr_addr.c pr_alloc.c pr_clentry.c pr_fdect.c pr_gbcast.c pr_gmgr.c pr_mcast.c history.c

 * Revision 1.194  89/09/12  21:09:50  ken
 * stupid bug
 * 
 * ci -u1.194 pr_gbcast.c history.c

 * Revision 1.193  89/09/12  15:58:33  ken
 * ...
 * 
 * ci -u1.193 pr_init.c history.c

 * Revision 1.192  89/09/12  12:45:00  ken
 * ....
 * 
 * ci -u1.192 pr_alloc.c history.c

 * Revision 1.191  89/09/11  11:42:13  rcbc
 * Now uses FULL_VERSION string containing current ISIS version number
 * from cl_typedefs.h in startup message.
 * 
 * ci -u1.191 pr_main.c history.c

 * Revision 1.190  89/09/11  10:16:06  ken
 * oops
 * ./
 * 
 * ci -u1.190 pr_init.c history.c

 * Revision 1.189  89/09/11  10:12:23  ken
 * fix some extern defs
 * 
 * ci -u1.189 pr_init.c history.c

 * Revision 1.188  89/09/08  13:34:03  patrick
 * Fixed declarations so the isis header files go through att c++ 1.2
 * 
 * ci -u1.188 isis_alloc.h pr_alloc.c pr_client.h pr_fdect.c pr_fdect.h pr_queues.c pr_queues.h history.c

 * Revision 1.187  89/08/28  15:28:33  rcbc
 * Fixes to make sure ISIS still compiles under non-ansi compilers.
 * 
 * ci -u1.187 pr_alloc.c isis_alloc.h history.c

 * Revision 1.186  89/08/25  14:30:07  rcbc
 * Some fixes to get protos to compile under gcc -w (i.e. with warnings 
 * disabled).
 * 
 * ci -u1.186 bits.h isis_alloc.h pr.h pr_alloc.c pr_inter.h pr_pgroups.h pr_typedefs.h history.c

 * Revision 1.185  89/08/22  16:14:34  ken
 * BYPASS glitch
 * 
 * ci -u1.185 pr_cbcast.c history.c

 * Revision 1.184  89/08/22  14:37:18  ken
 * Last minute "roll backs to old vbersions"
 * 
 * ci -u1.184 pr_main.c pr_astore.c pr_cbcast.c history.c

 * Revision 1.183  89/08/22  12:37:54  ken
 * ...
 * 
 * ci -u1.183 pr_clentry.c pr_cbcast.c history.c

 * Revision 1.182  89/08/22  09:26:14  ken
 * Hopefully, eliminates the =[] nodes that seem to pile up
 * 
 * ci -u1.182 pr_astore.c history.c

 * Revision 1.181  89/08/21  16:09:43  ken
 * ...
 * 
 * ci -u1.181 pr_dump.c pr_cbcast.c history.c

 * Revision 1.180  89/08/20  20:58:44  ken
 * 'resca' meaning has changed
 * 
 * ci -u1.180 pr_cbcast.c history.c

 * Revision 1.179  89/08/20  20:58:08  ken
 * Changes to reflect the new thinking
 * 
 * ci -u1.179 pr_astore.c pr_addr.c history.c

 * Revision 1.178  89/08/20  20:57:13  ken
 * change to reflect new thinking
 * 
 * ci -u1.178 pr_addr.c history.c

 * Revision 1.177  89/08/17  11:27:03  ken
 * oops
 * 
 * ci -u1.177 pr_client.c history.c

 * Revision 1.176  89/08/17  11:01:08  ken
 * fix 'exclude' option in protocols
 * 
 * ci -u1.176 pr_gmgr.c history.c

 * Revision 1.175  89/08/17  10:10:02  ken
 * add ILLEGAL_INCARN
 * 
 * ci -u1.175 pr_addr.c pr_client.c pr_fdect.c pr_fdect.h history.c

 * Revision 1.174  89/08/15  15:57:04  ken
 * trade a panic for an infinite loop
 * 
 * ci -u1.174 pr_cbcast.c history.c

 * Revision 1.173  89/08/15  15:56:10  ken
 * ...
 * 
 * ci -u1.173 pr_dump.c history.c

 * Revision 1.172  89/08/15  15:55:44  ken
 * ...
 * 
 * ci -u1.172 pr_client.c pr_gbcast.c history.c

 * Revision 1.171  89/08/10  15:40:09  rcbc
 * Added $Revision RCS keyword to all files. 
 * 
 * ci -u1.171 RCS bits.h generic.h history.c isis_alloc.h isis_errno.h makefile pr.h pr_abcast.c pr_abcast.h pr_addr.c pr_address.h pr_alloc.c pr_astore.c pr_astore.h pr_cbcast.c pr_cbcast.h pr_clentry.c pr_client.c pr_client.h pr_dlist.c pr_dump.c pr_errors.h pr_fdect.c pr_fdect.h pr_gbcast.c pr_gbcast.h pr_global.h pr_glocks.c pr_glocks.h pr_gmgr.c pr_init.c pr_inter.c pr_inter.h pr_intersite.h pr_local.c pr_main.c pr_mcast.c pr_msgfields.h pr_msgflags.h pr_msgtank.c pr_msgtank.h pr_pgroups.h pr_qnode.h pr_queues.c pr_queues.h pr_setjmp.c pr_setjmp.s pr_setjmp.s.~1~ pr_stats.h pr_task.c pr_task.h pr_typedefs.h pr_viewlocks.c pr_watch.c pr_wqueues.h history.c

 * Revision 1.170  89/08/10  13:14:09  ken
 * Fix a pg_delete problem
 * 
 * ci -u1.170 pr_gmgr.c history.c

 * Revision 1.169  89/08/09  14:05:01  ken
 * ...
 * 
 * ci -u1.169 pr.h pr_task.c pr_task.h history.c

 * Revision 1.168  89/08/09  13:10:24  ken
 * ...
 * 
 * ci -u1.168 makefile history.c

 * Revision 1.167  89/08/09  13:07:25  ken
 * pr_fdect bug fixes
 * 
 * ci -u1.167 makefile pr_dump.c pr_fdect.c history.c

 * Revision 1.166  89/08/09  11:38:45  ken
 * ...
 * 
 * ci -u1.166 pr.h history.c

 * Revision 1.165  89/08/09  09:15:21  ken
 * ...
 * 
 * ci -u1.165 pr_inter.h history.c

 * Revision 1.164  89/08/08  16:09:08  ken
 * ...
 * 
 * ci -u1.164 pr_inter.h history.c

 * Revision 1.163  89/08/08  14:41:02  ken
 * ...
 * 
 * ci -u1.163 pr_cbcast.c pr_astore.c history.c

 * Revision 1.162  89/08/08  10:35:08  ken
 * Add meta entry points
 * 
 * ci -u1.162 generic.h history.c

 * Revision 1.161  89/08/04  12:21:11  ken
 * add some AIX stuff
 * 
 * ci -u1.161 pr.h pr_task.h pr_task.c pr_init.c history.c

 * Revision 1.160  89/08/04  11:09:38  ken
 * Eliminate useless chit-chat
 * 
 * ci -u1.160 pr_gmgr.c history.c

 * Revision 1.159  89/08/04  09:23:46  ken
 * add IE_REFUSED
 * .y
 * y
 * y
 * cd ../clib
 * 
 * ci -u1.159 pr_errors.h history.c

 * Revision 1.158  89/08/03  22:01:16  ken
 * fix the "before" gbcast problem, again
 * 
 * ci -u1.158 pr_client.c pr_dump.c pr_gbcast.c pr_inter.c history.c

 * Revision 1.157  89/08/03  11:13:17  ken
 * Useful debug message
 * 
 * ci -u1.157 pr_client.c history.c

 * Revision 1.156  89/07/28  17:53:46  ken
 * ...
 * 
 * ci -u1.156 pr_gbcast.c history.c

 * Revision 1.155  89/07/28  17:50:23  ken
 * oops
 * 
 * ci -u1.155 makefile pr_gbcast.c history.c

 * Revision 1.154  89/07/27  11:21:04  ken
 * eliminate spurious message
 * 
 * ci -u1.154 pr_main.c history.c

 * Revision 1.153  89/07/27  11:15:25  ken
 * ...
 * 
 * ci -u1.153 generic.h history.c

 * Revision 1.152  89/07/26  12:43:35  ken
 * ...
 * 
 * ci -u1.152 pr_abcast.c pr_addr.c pr_cbcast.c pr_clentry.c pr_dump.c pr_gmgr.c pr_main.c history.c

 * Revision 1.151  89/07/25  09:56:49  ken
 * ...
 * 
 * ci -u1.151 pr_task.c history.c

 * Revision 1.150  89/07/25  09:53:28  ken
 * wasn't doing the accounting of tasks anymore
 * 
 * ci -u1.150 pr_task.c history.c

 * Revision 1.149  89/07/24  16:33:57  ken
 * Fix a minor problem
 * 
 * ci -u1.149 pr_gbcast.c history.c

 * Revision 1.148  89/07/24  14:01:58  ken
 * ...
 * 
 * ci -u1.148 pr_client.h history.c

 * Revision 1.147  89/07/24  14:01:36  ken
 * ....
 * 
 * ci -u1.147 pr_astore.c pr_cbcast.c pr_client.c pr_gbcast.c history.c

 * Revision 1.146  89/07/21  12:56:35  ken
 * ...
 * 
 * ci -u1.146 pr.h history.c

 * Revision 1.145  89/07/21  12:56:21  ken
 * ...
 * 
 * ci -u1.145 pr_main.c pr_inter.c pr_inter.h pr_gbcast.,c pr_client.c history.c

 * Revision 1.144  89/07/17  17:30:13  ken
 * Two minor congestion bugs
 * 
 * ci -u1.144 pr_client.c pr_main.c history.c

 * Revision 1.143  89/07/13  10:15:17  ken
 * Change from SIG_HUP to SIG_DUMP
 * 
 * ci -u1.143 pr_client.h pr_main.c history.c

 * Revision 1.142  89/07/13  09:32:27  ken
 * ...
 * 
 * ci -u1.142 pr_cbcast.c history.c

 * Revision 1.141  89/07/13  00:11:54  ken
 * :-)
 * 
 * ci -u1.141 pr_cbcast.c history.c

 * Revision 1.140  89/07/12  23:41:21  ken
 * ....
 * 
 * ci -u1.140 pr_cbcast.c history.c

 * Revision 1.139  89/07/12  23:38:47  ken
 * finally!
 * 
 * ci -u1.139 pr_cbcast.c pr_gbcast.c history.c

 * Revision 1.138  89/07/12  23:00:48  ken
 * ...
 * 
 * ci -u1.138 pr_cbcast.c pr_astore.c history.c

 * Revision 1.137  89/07/12  22:02:27  ken
 * Various urgent and pressing changes
 * 
 * ci -u1.137 pr_addr.c pr_address.h pr_astore.c pr_astore.h pr_cbcast.c pr_clentry.c pr_client.c pr_gmgr.c pr_init.c pr_inter.c pr_main.c history.c

 * Revision 1.136  89/07/10  20:55:47  ken
 * ...
 * 
 * ci -u1.136 pr_task.c history.c

 * Revision 1.135  89/07/10  20:55:13  ken
 * ...
 * 
 * ci -u1.135 pr_task.c history.c

 * Revision 1.134  89/07/05  13:17:43  ken
 * ...
 * 
 * ci -u1.134 makefile history.c

 * Revision 1.133  89/07/03  20:16:34  ken
 * July 4th spooler checkin
 * 
 * ci -u1.133 generic.h history.c

 * Revision 1.132  89/07/03  13:24:39  ken
 * Fix a few buggsies
 * 
 * ci -u1.132 pr_addr.c pr_client.c pr_dump.c pr_gbcast.c pr_gmgr.c pr_inter.c history.c

 * Revision 1.131  89/06/27  12:06:16  ken
 * ...
 * 
 * ci -u1.131 pr_glocks.c history.c

 * Revision 1.130  89/06/26  13:00:59  ken
 * ...
 * 
 * ci -u1.130 pr_inter.c pr_client.c pr_cbcast.c pr_alloc.c pr.h generic.h history.c

 * Revision 1.129  89/06/26  12:59:00  ken
 * eliminate infinite loop in pg_update_pgviews
 * 
 * ci -u1.129 pr_gmgr.c history.c

 * Revision 1.128  89/06/26  10:21:30  ken
 * ...
 * 
 * ci -u1.128 pr_fdect.c history.c

 * Revision 1.127  89/06/20  14:05:30  ken
 * ...
 * 
 * ci -u1.127 pr_init.c pr_gbcast.c history.c

 * Revision 1.126  89/06/20  13:12:33  ken
 * ...
 * 
 * ci -u1.126 generic.h history.c

 * Revision 1.125  89/06/18  17:23:57  ken
 * ...
 * 
 * ci -u1.125 pr_init.c history.c

 * Revision 1.124  89/06/16  16:42:52  ken
 * ...
 * 
 * ci -u1.124 pr_glocks.c history.c

 * Revision 1.123  89/06/16  16:16:42  ken
 * Fix shr_gunlock bug
 * 
 * ci -u1.123 pr_gbcast.c history.c

 * Revision 1.122  89/06/16  15:48:56  ken
 * Fix ghastly error
 * 
 * ci -u1.122 pr_addr.c pr_client.c pr_fdect.c pr_gmgr.c history.c

 * Revision 1.121  89/06/15  15:21:31  ken
 * finally!
 * 
 * ci -u1.121 pr_main.c history.c

 * Revision 1.120  89/06/15  15:13:01  ken
 * ...
 * 
 * ci -u1.120 pr_main.c history.c

 * Revision 1.119  89/06/15  13:13:51  ken
 * ...
 * 
 * ci -u1.119 pr_clentry.c history.c

 * Revision 1.118  89/06/15  13:10:37  ken
 * ...
 * 
 * ci -u1.118 pr_main.c history.c

 * Revision 1.117  89/06/15  12:49:57  ken
 * ...
 * 
 * ci -u1.117 pr_init.c history.c

 * Revision 1.116  89/06/15  12:47:56  ken
 * ...
 * 
 * ci -u1.116 pr_init.c pr_mcast.c history.c

 * Revision 1.115  89/06/15  12:43:18  ken
 * ...
 * 
 * ci -u1.115 pr_init.c history.c

 * Revision 1.114  89/06/15  12:38:54  ken
 * ...
 * 
 * ci -u1.114 pr_gbcast.c history.c

 * Revision 1.113  89/06/13  15:37:25  ken
 * ...
 * 
 * ci -u1.113 pr.h history.c

 * Revision 1.112  89/06/13  09:50:39  ken
 * fiddle with retransmission rules again, now times out after about 1:30 minutes
 * 
 * ci -u1.112 pr_inter.c history.c

 * Revision 1.111  89/06/12  09:36:31  ken
 * Include version number into ISIS log
 * 
 * ci -u1.111 pr_main.c history.c

 * Revision 1.110  89/06/09  10:20:33  ken
 * attribute code, change version number
 * 
 * ci -u1.110 bits.h generic.h isis_alloc.h isis_errno.h pr.h pr_abcast.h pr_address.h pr_astore.h pr_cbcast.h pr_client.h pr_errors.h pr_fdect.h pr_gbcast.h pr_global.h pr_glocks.h pr_inter.h pr_intersite.h pr_msgfields.h pr_msgflags.h pr_msgtank.h pr_pgroups.h pr_qnode.h pr_queues.h pr_stats.h pr_task.h pr_typedefs.h pr_wqueues.h pr_abcast.c pr_addr.c pr_alloc.c pr_astore.c pr_cbcast.c pr_clentry.c pr_client.c pr_dlist.c pr_dump.c pr_fdect.c pr_gbcast.c pr_glocks.c pr_gmgr.c pr_init.c pr_inter.c pr_local.c pr_main.c pr_mcast.c pr_msgtank.c pr_queues.c pr_setjmp.c pr_task.c pr_viewlocks.c pr_watch.c pr_setjmp.s history.c

 * Revision 1.109  89/06/07  15:38:13  ken
 * y
 * ...
 * 
 * ci -u1.109 makefile history.c

 * Revision 1.108  89/06/07  15:08:35  ken
 * add MIPS stuff
 * 
 * ci -u1.108 pr.h pr_task.c history.c

 * Revision 1.107  89/06/06  15:36:28  rcbc
 * Added isis_errno.h.
 * 
 * ci -u1.107 isis_errno.h history.c

 * Revision 1.106  89/05/31  10:15:21  ken
 * ...
 * 
 * ci -u1.106 pr_dump.c history.c

 * Revision 1.105  89/05/31  10:10:30  ken
 * Format was ugly...
 * 
 * ci -u1.105 pr_dump.c history.c

 * Revision 1.104  89/05/31  09:37:55  ken
 * change calls to printf into calls to print; print date on front of line
 * 
 * ci -u1.104 pr_dump.c pr_fdect.c pr_inter.c pr_main.c history.c

 * Revision 1.103  89/05/31  09:14:01  ken
 * play with intersite failure detection parameters
 * 
 * ci -u1.103 pr_inter.c history.c

 * Revision 1.102  89/05/30  20:16:11  ken
 * "tune" retransmission code a little
 * 
 * ci -u1.102 pr_inter.c history.c

 * Revision 1.101  89/05/30  20:14:25  ken
 * fix "incarn prematurity" problem
 * 
 * ci -u1.101 pr_fdect.c history.c

 * Revision 1.100  89/05/30  16:30:20  ken
 * ...
 * 
 * ci -u1.100 pr_addr.c history.c

 * Revision 1.99  89/05/30  15:13:19  ken
 * V12 interim checkin
 * 
 * ci -u1.99 pr_cbcast.c pr_fdect.c pr_client.c pr_inter.c history.c

 * Revision 1.98  89/05/29  12:40:00  ken
 * fix a missing dependency
 * 
 * ci -u1.98 makefile history.c

 * Revision 1.97  89/05/18  14:21:16  ken
 * SUNLWP from makefile now
 * 
 * ci -u1.97 pr.h history.c

 * Revision 1.96  89/05/18  11:04:24  ken
 * addr_isnull -> aptr_isnull
 * 
 * ci -u1.96 pr_addr.c pr_cbcast.c pr_clentry.c pr_client.c pr_dlist.c pr_dump.c pr_gmgr.c pr_local.c pr_mcast.c history.c

 * Revision 1.95  89/05/17  14:23:15  ken
 * checkin msg.h
 * addr_isnull problem
 * 
 * ci -u1.95 pr_addr.c history.c

 * Revision 1.94  89/05/17  13:33:46  ken
 * eliminate a "double dequeue" problem
 * 
 * ci -u1.94 pr_task.c history.c

 * Revision 1.93  89/05/16  15:08:46  ken
 * incorrect use of addr_isnull
 * .,
 * 
 * ci -u1.93 pr_clentry.c history.c

 * Revision 1.92  89/05/15  16:37:00  ken
 * fix dump message
 * 
 * ci -u1.92 pr_dump.c history.c

 * Revision 1.91  89/05/01  14:47:46  ken
 * minor change related to apollo code
 * 
 * ci -u1.91 pr_client.c history.c

 * Revision 1.90  89/05/01  14:47:06  ken
 * Improve error handling
 * 
 * ci -u1.90 pr_client.h pr_cbcast.c history.c

 * Revision 1.89  89/04/30  13:44:36  ken
 * calls time_warp too often
 * 
 * ci -u1.89 pr_main.c history.c

 * Revision 1.88  89/04/30  13:22:24  ken
 * Minor changes for APOLLO
 * 
 * ci -u1.88 pr.h pr_client.h history.c

 * Revision 1.87  89/04/27  11:56:01  ken
 * y
 * y
 * APOLLO in UNIX DOMAIN
 * 
 * ci -u1.87 pr_client.c pr.h history.c

 * Revision 1.86  89/04/26  20:29:59  ken
 * make cl_queues.h match pr_queues.h
 * 
 * ci -u1.86 pr_queues.h makefile history.c

 * Revision 1.85  89/04/26  09:51:42  ken
 * V1.2 checkin of pr_addr.c
 * 
 * ci -u1.85 pr_addr.c history.c

 * Revision 1.84  89/04/26  09:50:52  ken
 * V1.2 checking
 * 
 * ci -u1.84 pr_addr.c history.c

 * Revision 1.83  89/04/26  09:50:27  ken
 * V1.2 checkin of all changes
 * 
 * ci -u1.83 bits.h generic.h isis_alloc.h makefile pr.h pr_abcast.c pr_abcast.h pr_addr.c pr_address.h pr_alloc.c pr_astore.c pr_astore.h pr_cbcast.c pr_cbcast.h pr_clentry.c pr_client.c pr_client.h pr_dlist.c pr_dump.c pr_errors.h pr_fdect.c pr_fdect.h pr_gbcast.c pr_gbcast.h pr_global.h pr_glocks.c pr_glocks.h pr_gmgr.c pr_init.c pr_inter.c pr_inter.h pr_intersite.h pr_local.c pr_main.c pr_mcast.c pr_msgfields.h pr_msgflags.h pr_msgtank.c pr_msgtank.h pr_pgroups.h pr_qnode.h pr_queues.c pr_queues.h pr_setjmp.c pr_setjmp.s pr_stats.h pr_task.c pr_task.h pr_typedefs.h pr_viewlocks.c pr_watch.c pr_wqueues.h history.c

 * Revision 1.82  89/04/04  11:59:03  rcbc
 * ...
 * 
 * ci -u1.82 -l pr_addr.c history.c

 * Revision 1.81  89/03/28  15:41:37  ken
 * Oops...
 * 
 * ci -u1.81 pr_addr.c history.c

 * Revision 1.80  89/03/24  15:16:41  ken
 * Insert msgid into async messages too
 * 
 * ci -u1.80 pr_addr.c history.c

 * Revision 1.79  89/02/14  09:56:14  ken
 * put some ack stuff in...
 * 
 * ci -u1.79 pr_fdect.c pr_inter.c pr_inter.h history.c

 * Revision 1.78  89/01/31  11:08:27  rcbc
 * embed rcs version number in run file.
 * 
 * ci -u1.78 makefile history.c

 * Revision 1.77  89/01/31  11:07:31  rcbc
 * Fixed rcsid decl.
 * 
 * Revision 1.76  89/01/31  08:32:56  ken
 * Substantially increase ack rate to overcome poor performance
 * 
 * ci -u1.76 pr_inter.h pr_inter.c history.c

 * Revision 1.75  89/01/30  17:51:08  rcbc
 * Updated for new checkin program.
 * 
 * Revision 1.74  89/01/30  14:55:18  ken
 * Fix a few bugs and also some stuff that LINT and MACH complained about
 * 
 * ci -u1.74 pr_client.c pr_dump.c pr_fdect.c pr_gbcast.c pr_glocks.c pr_gmgr.c pr_init.c pr_inter.c pr_local.c pr_main.c pr_msgtank.h pr_setjmp.s pr_task.c history

 * Revision 1.73  89/01/24  16:04:28  ken
 * MACH mistake
 * 
 * ci -u1.73 pr.h history

 * Revision 1.72  89/01/24  16:04:02  ken
 * NEXT in UNIX_DOM
 * 
 * ci -u1.72 pr.h history

 * Revision 1.71  89/01/24  15:31:51  ken
 * Use MACH assembler format
 * 
 * ci -u1.71 pr_setjmp.s history

 * Revision 1.70  89/01/24  15:27:08  ken
 * MACH complained
 * 
 * ci -u1.70 pr_fdect.c history

 * Revision 1.69  89/01/24  15:25:46  ken
 * Extra comma caused MACH to complain
 * 
 * ci -u1.69 pr_dlist.c history

 * Revision 1.68  89/01/24  15:24:18  ken
 * MACH still complained
 * 
 * ci -u1.68 pr_inter.c history

 * Revision 1.67  89/01/24  15:22:15  ken
 * no change
 * 
 * ci -u1.67 pr_inter.c history

 * Revision 1.66  89/01/24  15:21:36  ken
 * MACH complained
 * 
 * ci -u1.66 pr_inter.c history

 * Revision 1.65  89/01/24  15:19:57  ken
 * MACH complained
 * 
 * ci -u1.65 pr_main.c history

 * Revision 1.64  89/01/24  15:16:59  ken
 * MACH complained
 * 
 * ci -u1.64 pr_gmgr.c history

 * Revision 1.63  89/01/24  15:13:54  ken
 * MACH complained
 * 
 * ci -u1.63 pr_dump.c history

 * Revision 1.62  89/01/24  15:11:59  ken
 * MACH complained
 * 
 * ci -u1.62 pr_task.c history

 * Revision 1.61  89/01/24  15:09:33  ken
 * MACH still complained
 * 
 * ci -u1.61 pr_gbcast.c history

 * Revision 1.60  89/01/24  15:07:55  ken
 * MACH complained
 * 
 * ci -u1.60 pr_gbcast.c history

 * Revision 1.59  89/01/24  15:06:01  ken
 * MACH complained
 * 
 * ci -u1.59 pr_msgtank.h history

 * Revision 1.58  89/01/13  12:49:19  morrison
 * add "date" of restart in log file
 * 
 * Revision 1.57  89/01/11  14:33:52  ken
 * NULLADDRESS extern declaration in pr_address.h
 * 
 * ci -u1.57 pr_address.h pr_task.c history

 * ci -u1.57 pr_main.c history

 * Revision 1.56  89/01/10  12:43:44  ken
 * y
 * 
 * ci -u1.56 pr_client.h history

 * 
 * Revision 1.55  89/01/09  12:47:49  ken
 * Fix some stuff HPUX complains about
 * 
 * ci -u1.55 pr_addr.c pr_clentry.c pr_dump.c pr_local.c pr_mcast.c history

 * Revision 1.54  89/01/06  09:23:13  ken
 * Add OPTIM flag, modify SUN4 setjmp/longjmp
 * 
 * ci -u1.54 pr_setjmp.s makefile history

 * Revision 1.53  89/01/02  10:55:49  ken
 * fix a syntax problem in HPUX version
 * 
 * ci -u1.53 pr_addr.c pr_astore.c pr_inter.c pr_mcast.c pr_queues.h pr_task.c pr_watch.c history

 * Revision 1.52  88/12/22  09:14:48  ken
 * Final tuning on VAX setjmp/longjmp
 * 
 * ci -u1.52 pr_setjmp.s pr_task.c history

 * Revision 1.51  88/12/21  16:22:44  ken
 * leave room for over-runs...
 * 
 * ci -u1.51 pr_task.h history

 * Revision 1.50  88/12/21  15:44:33  ken
 * must save registers now
 * 
 * ci -u1.50 pr_setjmp.s history

 * Revision 1.49  88/12/21  15:33:41  ken
 * Everyone else seems to have a compiler bug!
 * 
 * ci -u1.49 pr_queues.h history

 * Revision 1.48  88/12/21  13:51:14  ken
 * eliminate an illegal register declaration
 * 
 * ci -u1.48 pr_gbcast.c history

 * Revision 1.47  88/12/19  12:01:05  ken
 * ix switch on SUN4
 * 
 * ci -u1.47 pr_task.c history

 * Revision 1.46  88/12/19  11:15:15  ken
 * add copntrol flow pragma for isis swtch routine
 * 
 * ci -u1.46 pr_task.c history

 * Revision 1.45  88/12/19  09:23:33  ken
 * proto_names should be declared extern
 * 
 * ci -u1.45 pr.h history

 * Revision 1.44  88/12/19  09:21:37  ken
 * Add HPUX spectrum and APOLLO mods
 * 
 * ci -u1.44 pr_task.c history

 * Revision 1.43  88/12/19  09:13:15  ken
 * Include some HP spectrum related mods
 * 
 * ci -u1.43 pr_dump.c pr_mcast.c history

 * Revision 1.42  88/12/15  14:55:02  ken
 * warp time when clocks lurch forward or back
 * 
 * ci -u1.42 pr_inter.c pr_main.c history

 * Revision 1.41  88/12/15  09:26:43  ken
 * Edit to include V1.1 disclaimer
 * 
 * ci -u1.41 pr_setjmp.s history

 * Revision 1.40  88/12/15  09:25:53  ken
 * Edit to expand tabs and include V1.1 disclaimer
 * 
 * ci -u1.40 bits.h generic.h isis_alloc.h pr.h pr_abcast.h pr_address.h pr_astore.h pr_cbcast.h pr_client.h pr_errors.h pr_fdect.h pr_gbcast.h pr_global.h pr_glocks.h pr_inter.h pr_intersite.h pr_msgfields.h pr_msgflags.h pr_msgtank.h pr_pgroups.h pr_qnode.h pr_queues.h pr_stats.h pr_task.h pr_typedefs.h pr_wqueues.h pr_abcast.c pr_addr.c pr_alloc.c pr_astore.c pr_cbcast.c pr_clentry.c pr_client.c pr_dlist.c pr_dump.c pr_fdect.c pr_gbcast.c pr_glocks.c pr_gmgr.c pr_init.c pr_inter.c pr_local.c pr_main.c pr_mcast.c pr_msgtank.c pr_queues.c pr_task.c pr_viewlocks.c pr_watch.c history

 * Revision 1.39  88/12/14  16:22:45  ken
 * more fixes to the fdect bug
 * 
 * ci -u1.39 pr_fdect.c pr_inter.c history

 * Revision 1.38  88/12/14  16:03:43  ken
 * type
 * 
 * ci -u1.38 pr_inter.c history

 * Revision 1.37  88/12/14  15:56:16  ken
 * case of the hang during 2 rapid recoveries in a row
 * 
 * ci -u1.37 pr_fdect.c history

 * Revision 1.36  88/12/14  15:47:52  ken
 * eliminate deadlocks in fdect on quick failure/restart seqs
 * 
 * ci -u1.36 pr_inter.c history

 * Revision 1.35  88/12/14  11:48:30  ken
 * keep channel alive during proposed recovery protocol
 * 
 * ci -u1.35 pr_inter.c history

 * Revision 1.34  88/12/14  11:25:53  ken
 * watch didn't trigger when 2 view changes in a row occurred
 * 
 * ci -u1.34 pr_watch.c pr_dump.c history

 * Revision 1.33  88/12/14  11:19:48  ken
 * avoid locking PROTOCOLS to simpliy logs
 * 
 * ci -u1.33 pr_glocks.c history

 * Revision 1.32  88/12/14  10:54:38  ken
 * oops
 * 
 * ci -u1.32 pr_gbcast.c history

 * Revision 1.31  88/12/14  09:15:32  ken
 * eliminate all risk of a race between gb_free and recv2/recvabort message
 * 
 * ci -u1.31 pr_gbcast.c history

 * Revision 1.30  88/12/09  14:41:10  ken
 * gbcast seems to have a cb_free case after failures that
 * legitimately doesn't find the id in the astore.  Removed message
 * 
 * ci -u1.30 pr_astore.c history

 * Revision 1.29  88/12/09  13:35:16  ken
 * delete incorrect debug check
 * 
 * ci -u1.29 pr_gbcast.c history

 * Revision 1.28  88/12/09  13:24:59  ken
 * infinite loop when pbufs queue changed while gbcast was delivering
 * 
 * ci -u1.28 pr_gbcast.c history

 * Revision 1.27  88/12/09  12:40:40  ken
 * fix a debug message that refed an uninitialized var
 * 
 * ci -u1.27 pr_cbcast.c history

 * Revision 1.26  88/12/09  12:37:56  ken
 * delete a debug message that sometimes ran on an uninitialized variable
 * 
 * ci -u1.26 pr_cbcast.c history

 * Revision 1.25  88/12/09  10:27:50  ken
 * proposed recoveries listed by site-number only, not as site-id's
 * 
 * ci -u1.25 pr_dump.c history

 * Revision 1.24  88/12/08  13:45:43  ken
 * add -C option test fdect bug fix
 * 
 * ci -u1.24 pr_fdect.c pr_main.c history

 * Revision 1.23  88/12/08  12:02:54  ken
 * *** empty log message ***
 * 
 * ci -u1.23 pr_task.c history

 * Revision 1.22  88/12/08  11:12:36  ken
 * slight fix(?) to eliminate a deadlock
 * improve formatting in fdect dump
 * 
 * ci -u1.22 pr_gbcast.c pr_fdect.c pr_dump.c history

 * Revision 1.21  88/12/08  09:04:09  ken
 * switch back to t_fork of cbcast to eliminate what seems to  be a race
 * condition between the send2 phase and the cb_free operation.
 * 
 * ci -u1.21 pr_gbcast.c history

 * Revision 1.20  88/12/07  16:08:43  ken
 * ab_free no longer called from gbcast
 * 
 * ci -u1.20 pr_gbcast.c history

 * Revision 1.19  88/12/07  16:07:13  ken
 * fix gbcast garbage collection problem
 * 
 * ci -u1.19 pr_gbcast.c history

 * Revision 1.18  88/12/07  13:42:06  rcbc
 * Changed assembly of setjmp.s and turned off some debugging flags.
 * 
 * ci -u1.18 makefile history

 * Revision 1.17  88/12/07  13:34:05  ken
 * delete debug message
 * 
 * ci -u1.17 pr_gmgr.c history

 * Revision 1.16  88/12/07  11:51:03  ken
 * disable debug flags
 * 
 * ci -u1.16 makefile history

 * Revision 1.15  88/12/07  11:49:54  ken
 * fix a race condition in gbcast locking
 * 
 * ci -u1.15 pr_gbcast.c pr_gmgr.c pr_glocks.c pr_abcast.c pr_local.c pr_task.c history

 * Revision 1.14  88/12/07  10:37:35  rcbc
 * First checkin of makefile.
 * 
 * ci -u1.14 makefile history

 * Revision 1.13  88/12/06  16:23:42  ken
 * eliminate race condition in locking
 * 
 * ci -u1.13 pr_gbcast.c history

 * Revision 1.12  88/12/06  16:22:56  ken
 * eliminate locking race condition
 * 
 * ci -u1.12 pr_task.c pr_local.c pr_gmgr.c pr_addr.c history

 * Revision 1.11  88/12/06  11:19:31  rcbc
 * Added references to new XMGR process.
 * 
 * ci -u1.11 pr_client.c history

 * Revision 1.10  88/12/01  10:09:41  ken
 * fix error message
 * 
 * ci -u1.10 pr_fdect.c history

 * Revision 1.9  88/11/30  20:42:45  rcbc
 * Added generic entries and error messages for transaction mechanism.
 * 
 * ci -u1.9 pr_errors.h generic.h history

 * Revision 1.8  88/11/30  14:25:43  ken
 * fix a gbcast locking bug in the local case
 * 
 * ci -u1.8 pr_gbcast.c pr_glocks.c history

 * Revision 1.7  88/11/29  13:32:18  ken
 * eliminate unecessary messages
 * 
 * ci -u1.7 pr_client.c history

 * Revision 1.6  88/11/29  12:41:27  ken
 * better error message
 * 
 * ci -u1.6 pr_main.c history

 * Revision 1.5  88/11/29  12:39:22  ken
 * fix restart check routine
 * 
 * ci -u1.5 pr_main.c history

 * Revision 1.4  88/11/29  12:26:15  ken
 * detect hung site restart sequence and panic
 * 
 * ci -u1.4 pr_main.c history

 * Revision 1.3  88/11/28  13:43:23  ken
 * fix error message on bad arg
 * 
 * ci -u1.3 pr_main.c history

 * Revision 1.2  88/11/28  13:02:24  ken
 * minor formatting change
 * 
 * ci -u1.2 bits.h generic.h isis_alloc.h pr.h pr_abcast.c pr_abcast.h pr_addr.c pr_address.h pr_alloc.c pr_astore.c pr_astore.h pr_cbcast.c pr_cbcast.h pr_client.c pr_client.h pr_dlist.c pr_dump.c pr_errors.h pr_fdect.c pr_fdect.h pr_gbcast.c pr_stats.h pr_gbcast.h pr_global.h pr_glocks.c pr_glocks.h pr_gmgr.c pr_init.c pr_inter.c pr_inter.h pr_local.c pr_main.c pr_mcast.c pr_msgfields.h pr_msgflags.h pr_pgroups.h pr_qnode.h pr_queues.c pr_queues.h pr_task.c pr_task.h pr_typedefs.h pr_viewlocks.c pr_watch.c pr_clentry.c pr_wqueues.h pr_intersite.h pr_msgtank.c pr_msgtank.h history

 * Revision 1.1  88/11/21  11:50:15  rcbc
 * checkpoint
 * 
 * ci -u1.1 -mcheckpoint in midst of building release-1-1 bits.h generic.h isis_alloc.h pr.h pr_abcast.c pr_abcast.h pr_addr.c pr_address.h pr_alloc.c pr_astore.c pr_astore.h pr_cbcast.c pr_cbcast.h pr_client.c pr_client.h pr_dlist.c pr_dump.c pr_errors.h pr_fdect.c pr_fdect.h pr_gbcast.c pr_stats.h pr_gbcast.h pr_global.h pr_glocks.c pr_glocks.h pr_gmgr.c pr_init.c pr_inter.c pr_inter.h pr_local.c pr_main.c pr_mcast.c pr_msgfields.h pr_msgflags.h pr_pgroups.h pr_qnode.h pr_queues.c pr_queues.h pr_task.c pr_task.h pr_typedefs.h pr_viewlocks.c pr_watch.c pr_clentry.c pr_wqueues.h pr_setjmp.s pr_intersite.h pr_msgtank.c pr_msgtank.h syswait.h history

 * Revision 1.0  88/11/21  10:47:17  rcbc
 * Check-in of ~isis/distrib
 * 
 */
