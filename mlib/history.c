/* History file for mlib */
char mlib_rcsid[] = "$Revision: 2.23 $$Date: 90/09/12 13:27:28 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/mlib/RCS/history.c,v $";

/* $Log:	history.c,v $
 * Revision 2.23  90/09/12  13:27:28  ken
 * SGI changes
 * 
 * ci -u2.23 msg_alloc.c history.c

 * Revision 2.22  90/08/23  21:37:06  patrick
 * added include of varargs.h ; gcc seemed to need it.
 * 
 * ci -u2.22 msg.h history.c

 * Revision 2.21  90/08/13  20:41:23  ken
 * Needed in linking with protos
 * 
 * ci -u2.21 msg.c history.c

 * Revision 2.20  90/08/13  20:25:24  ken
 * Missing an ISISCALL1 callback
 * 
 * ci -u2.20 msg.c history.c

 * Revision 2.19  90/08/09  13:43:48  ken
 * MIPS wants everything double-word aligned.
 * 
 * ci -u2.19 msg.h history.c

 * Revision 2.18  90/08/09  09:52:38  ken
 * Include cl_typedefs.h
 * 
 * ci -u2.18 msg.h history.c

 * Revision 2.17  90/08/08  15:49:38  rcbc
 * Now include "cl_typedefs.h" since that's needed by msg.h.
 * 
 * ci -u2.17 msg.c history.c

 * Revision 2.16  90/08/08  10:52:14  tclark
 * Move varargs.h include
 * 
 * ci -u2.16 msg.h history.c

 * Revision 2.15  90/08/06  14:13:45  ken
 * Eliminate redeclaration
 * 
 * ci -u2.15 msg_sa.c history.c

 * Revision 2.14  90/08/06  13:47:36  ken
 * AUX changes
 * 
 * ci -u2.14 makefile msg.h history.c

 * Revision 2.13  90/08/01  21:05:21  ken
 * Fortran callers have ref-ref pointers to %A objects, use VA_ARG to
 * get ISIS to deref once
 * 
 * ci -u2.13 msg_fmt.c history.c

 * Revision 2.12  90/07/31  14:02:13  ken
 * AIXRS changes
 * 
 * ci -u2.12 msg_alloc.c history.c

 * Revision 2.11  90/07/31  11:33:06  ken
 * Add QUORUM as alias for MAJORITY
 * 
 * ci -u2.11 msg.h history.c

 * Revision 2.10  90/07/03  16:05:46  tclark
 * Oops
 * 
 * ci -u2.10 msg.c history.c

 * Revision 2.9  90/07/03  11:11:04  tclark
 * Oops
 * 
 * ci -u2.9 msg.c history.c

 * Revision 2.8  90/07/03  11:08:08  tclark
 * Add FREF and CALL1 mechanism in msg.c, msg.h
 * 
 * ci -u2.8 msg.h msg.c history.c

 * Revision 2.7  90/06/24  14:09:40  rcbc
 * Fixed C++ for AT&T 1.2 and g++.
 * 
 * ci -u2.7 msg.h history.c

 * Revision 2.6  90/06/24  14:08:15  rcbc
 * Fixed C++ for AT&T 1.2 and g++.
 * 
 * ci -u2.6 isis_alloc.h msg.h history.c

 * Revision 2.5  90/06/13  15:02:49  patrick
 * *** empty log message ***
 * 
 * ci -u2.5 sysfields.h history.c

 * Revision 2.4  90/06/11  10:15:00  ken
 * Ellen caught a typo
 * 
 * ci -u2.4 msg.c history.c

 * Revision 2.3  90/06/11  10:08:55  rcbc
 * Fixed function prototypes for C++.
 * 
 * ci -u2.3 msg.h msg_sa.c history.c

 * Revision 2.2  90/05/15  16:11:23  patrick
 * Fixed a minor bug that meant some iovec structures were freed twice,
 * with the predictable results.
 * 
 * ci -u2.2 msg.c history.c

 * Revision 2.1  90/05/04  15:15:51  rcbc
 * Fixed declaration of msg_doreconstruct.
 * 
 * ci -u2.1 msg.h history.c

 * Revision 2.0  90/05/04  15:12:25  rcbc
 * 2.0
 * 
 * Revision 1.154  90/05/04  00:01:30  ken
 * Oops
 * 
 * ci -u1.154 msg.c history.c

 * Revision 1.153  90/05/03  23:55:53  ken
 * Fix an annoying conversion bug
 * 
 * ci -u1.153 msg.c msg.h msg_types.c history.c

 * Revision 1.152  90/05/03  13:50:20  ken
 * Oops
 * 
 * ci -u1.152 msg.c msg_types.c history.c

 * Revision 1.151  90/05/03  13:44:25  ken
 * Fix dbl swap problem
 * 
 * ci -u1.151 msg.c msg.h msg_types.c history.c

 * Revision 1.150  90/05/03  12:56:12  ken
 * Fix byte-swapping of addresses
 * 
 * ci -u1.150 msg.c msg_types.c history.c

 * Revision 1.149  90/05/03  11:11:41  ken
 * Byte swap length on FTYPE_CHAR fields
 * .,`
 * 
 * ci -u1.149 msg.h msg.c msg_types.c history.c

 * Revision 1.148  90/05/02  16:21:17  rcbc
 * MACH/gcc type checking problems.
 * 
 * ci -u1.148 msg_fmt.c history.c

 * Revision 1.147  90/05/01  14:04:08  rcbc
 * Changed cp to /bin/cp
 * 
 * ci -u1.147 fix_mlib_for_gcc history.c

 * Revision 1.146  90/05/01  10:13:30  ken
 * FIx V2.0 copyright notice
 * 
 * ci -u1.146 isis_alloc.h makefile mlib-l.c msg.c msg.h msg_alloc.c msg_fio.c msg_fmt.c msg_sa.c msg_types.c sysfields.h history.c

 * Revision 1.145  90/05/01  10:09:14  rcbc
 * Changed name of mlib.a to libisism.a
 * 
 * ci -u1.145 fix_mlib_for_gcc history.c

 * Revision 1.144  90/05/01  10:05:31  rcbc
 * Fixed a few type casting problem for MIPS and HPUX.
 * 
 * ci -u1.144 msg.c msg_alloc.c history.c

 * Revision 1.143  90/04/27  14:20:11  ken
 * Add copyright notices
 * 
 * ci -u1.143 isis_alloc.h makefile mlib-l.c msg.c msg.h msg_alloc.c msg_fio.c msg_fmt.c msg_sa.c msg_types.c sysfields.h history.c

 * Revision 1.142  90/04/26  09:23:14  rcbc
 * Changed names of library files.
 * 
 * ci -u1.142 makefile history.c

 * Revision 1.141  90/04/25  14:24:43  ken
 * New "fair accounting" scheme for memory on msg freelist
 * 
 * ci -u1.141 msg.c history.c

 * Revision 1.140  90/04/25  12:56:53  ken
 * Oops!
 * 
 * ci -u1.140 msg.c history.c

 * Revision 1.139  90/04/24  14:51:21  ken
 * typos
 * 
 * ci -u1.139 msg.c history.c

 * Revision 1.138  90/04/24  14:41:07  ken
 * Better memory usage statistics.
 * 
 * ci -u1.138 msg.c history.c

 * Revision 1.137  90/04/23  11:39:36  rcbc
 * Reinstalled lost change from rev 1.128.
 * 
 * ci -u1.137 msg.c history.c

 * Revision 1.136  90/04/20  15:22:13  ken
 * many fixed
 * 
 * ci -u1.136 msg.c msg.h msg_fmt.c history.c

 * Revision 1.135  90/04/20  15:21:44  ken
 * *** empty log message ***
 * 
 * ci -u1.135 msg.c msg.h msg_fmt.c history.c

 * Revision 1.134  90/04/20  15:21:24  ken
 * *** empty log message ***
 * 
 * ci -u1.134 msg.c msg.h msg_fmt.c history.c

 * Revision 1.133  90/04/20  15:01:18  ken
 * HPUX picky compiler
 * 
 * ci -u1.133 msg_types.c history.c

 * Revision 1.132  90/04/20  14:57:24  ken
 * Fix msglib bug
 * 
 * ci -u1.132 msg.c history.c

 * Revision 1.131  90/04/19  16:31:36  patrick
 * fix a problem with null message reference callback routines.
 * 
 * ci -u1.131 msg_fmt.c history.c

 * Revision 1.130  90/04/19  08:53:02  ken
 * ...
 * 
 * ci -u1.130 msg.c history.c

 * Revision 1.129  90/04/17  13:31:26  ken
 * Minor change
 * 
 * ci -u1.129 msg.c history.c

 * Revision 1.128  90/04/17  08:49:21  rcbc
 * Fixed msg_getscantype to initialize msg_fpointer correctly.
 * 
 * ci -u1.128 msg.c history.c

 * Revision 1.127  90/04/16  11:09:39  patrick
 * increase  MSG_MAXIOV
 * 
 * ci -u1.127 msg.h history.c

 * Revision 1.126  90/04/16  09:08:43  ken
 * Minor.
 * 
 * ci -u1.126 msg.h history.c

 * Revision 1.125  90/04/03  20:05:50  rcbc
 * Ensure mlib.a has default protection specified by umask.
 * 
 * ci -u1.125 fix_mlib_for_gcc history.c

 * Revision 1.124  90/04/03  13:11:46  rcbc
 * Fixed msg_gettruesender.
 * 
 * ci -u1.124 msg.h history.c

 * Revision 1.123  90/04/03  10:08:10  ken
 * HPUX compile fixes
 * 
 * ci -u1.123 msg.h msg_types.c history.c

 * Revision 1.122  90/04/02  15:55:57  ken
 * oops
 * 
 * ci -u1.122 sysfields.h history.c

 * Revision 1.121  90/04/02  15:38:55  ken
 * Oops
 * 
 * ci -u1.121 msg.h history.c

 * Revision 1.120  90/04/02  15:35:02  ken
 * oops
 * 
 * ci -u1.120 msg.h history.c

 * Revision 1.119  90/04/02  14:48:52  ken
 * Minor stuff
 * 
 * ci -u1.119 history.c mlib-l.c msg.c msg_alloc.c msg_fio.c msg_fmt.c msg_sa.c msg_types.c history.c

 * Revision 1.118  90/03/29  10:04:39  rcbc
 * Fixed void * --> VOID * errors.
 * 
 * ci -u1.118 msg.c history.c

 * Revision 1.117  90/03/15  18:48:27  rcbc
 * Added RCSId.
 * 
 * ci -u1.117 fix_mlib_for_gcc history.c

 * Revision 1.116  90/03/15  17:50:53  rcbc
 * Added large group address type. There's no large group support
 * yet though and noone should notice this change.
 * 
 * ci -u1.116 msg.h history.c

 * Revision 1.115  90/03/14  10:03:34  ken
 * add PLIST address type
 * 
 * ci -u1.115 msg.h history.c

 * Revision 1.114  90/03/13  11:06:20  rcbc
 * Added "lost" msg_getscantype routine.
 * 
 * ci -u1.114 msg.c history.c

 * Revision 1.113  90/03/07  10:08:19  patrick
 * c++ changes
 * 
 * ci -u1.113 isis_alloc.h history.c

 * Revision 1.112  90/03/06  15:54:49  ken
 * Checking in cplusplus changes
 * 
 * ci -u1.112 msg.h history.c

 * Revision 1.111  90/03/06  15:54:01  ken
 * ...
 * 
 * ci -u1.111 msg.h history.c

 * Revision 1.110  90/02/25  11:45:01  ken
 * fix uninitialized var
 * 
 * ci -u1.110 msg_alloc.c history.c

 * Revision 1.109  90/02/21  13:38:58  rcbc
 * Removed a comment.
 * 
 * ci -u1.109 makefile history.c

 * Revision 1.108  90/02/21  13:34:54  rcbc
 * Improved fix_mlib_for_gcc.
 * 
 * ci -u1.108 makefile fix_mlib_for_gcc history.c

 * Revision 1.107  90/02/21  11:48:09  rcbc
 * We now make sure that mlib contains the gcc builtins library, gnulib,
 * if mlib is compiled with gcc. This ensures that users can link
 * their non-gcc-compiled main programs with a gcc-compiled Isis.
 * This is implemented with a rather complicated shell script.
 * 
 * ci -u1.107 makefile fix_mlib_for_gcc history.c

 * Revision 1.106  90/02/20  13:57:34  rcbc
 * One more voidify...
 * 
 * ci -u1.106 msg_sa.c history.c

 * Revision 1.105  90/02/19  14:32:42  rcbc
 * Someone's been checking files in without checking in history.c!!!
 * So we have to wind history.c's version number up.
 * 
 * Revision 1.101  90/02/19  14:31:44  rcbc
 * VOIDified a lot of routines. Some of these are user-callable routines
 * so the man pages should probably be fixed some time!
 * Also removed some unused variables that gcc told me about.
 * 
 * ci -u1.101 msg_fmt.c msg.h msg_types.c msg_sa.c msg.c msg_fio.c msg_alloc.c history.c

 * Revision 1.100  90/02/07  16:29:30  ken
 * ...
 * 
 * ci -u1.100 msg.h history.c

 * Revision 1.99  90/02/07  16:28:30  ken
 * oops
 * 
 * ci -u1.99 msg.h history.c

 * Revision 1.98  90/02/07  16:22:45  ken
 * do_msg_dlelete changes
 * 
 * ci -u1.98 msg.c msg.h history.c

 * Revision 1.97  90/01/31  11:43:55  ken
 * add act_isequal for use instead of addr_isequal on activity id comparisons
 * 
 * ci -u1.97 msg.h msg_fmt.c history.c

 * Revision 1.96  90/01/30  20:42:55  ken
 * addr structure changes, mlib bug
 * 
 * ci -u1.96 msg.c msg.h msg_sa.c msg_types.c history.c

 * Revision 1.95  90/01/26  10:44:16  ken
 * V2.0 changes
 * 
 * ci -u1.95 msg.c msg.h msg_alloc.c msg_fio.c history.c

 * Revision 1.94  90/01/11  13:48:13  ken
 * Small alignment problem
 * 
 * ci -u1.94 msg.c msg.h history.c

 * Revision 1.93  90/01/10  11:06:29  ken
 * Minor change to msg_reconstruct
 * 
 * ci -u1.93 msg.c history.c

 * Revision 1.92  90/01/09  14:19:44  ken
 * Minor stuff
 * 
 * ci -u1.92 msg.c msg_sa.c msg_type.c history.c

 * Revision 1.91  90/01/04  13:36:18  ken
 * Minor function prototyping type
 * 
 * ci -u1.91 msg.h history.c

 * Revision 1.90  90/01/04  13:07:00  ken
 * Various minor V2.0 stuff
 * 
 * ci -u1.90 makefile msg_sa.c history.c

 * Revision 1.89  89/12/18  10:43:26  ken
 * Debug {%d,....}[] format type
 * 
 * ci -u1.89 msg_fmt.c history.c

 * Revision 1.88  89/12/15  14:27:26  ken
 * ...
 * 
 * ci -u1.88 msg_sa.c msg_fmt.c history.c

 * Revision 1.87  89/12/15  13:32:33  ken
 * ISIS V2.0 major changes checked in
 * 
 * ci -u1.87 isis_alloc.h msg.h sysfields.h history.c mlib-l.c msg.c msg_alloc.c msg_fio.c msg_fmt.c msg_sa.c msg_types.c history.c

 * Revision 1.86  89/11/06  17:07:04  ken
 * Accomodate larger fragment size
 * 
 * ci -u1.86 msg.h history.c

 * Revision 1.85  89/11/06  16:35:41  ken
 * V2.0 changes....
 * 
 * ci -u1.85 msg.c msg.h msg_fio.c msg_fmt.c msg_types.c sysfields.h history.c

 * Revision 1.84  89/10/27  10:30:30  patrick
 * fixed for C++
 * 
 * ci -u1.84 msg.h history.c

 * Revision 1.83  89/10/26  11:34:36  ken
 * ISIS V2.0 MLIB changes
 * 
 * ci -u1.83 history.c mlib-l.c msg.c msg_fio.c msg_fmt.c msg_sa.c msg_types.c msg.h sysfields.h history.c

 * Revision 1.82  89/10/09  15:30:59  patrick
 * Put in some varargs declarations for C++
 * 
 * ci -u1.82 msg.h history.c

 * Revision 1.81  89/09/30  08:40:03  ken
 * Damn!
 * 
 * ci -u1.81 msg_types.c history.c

 * Revision 1.80  89/09/30  08:39:44  ken
 * Damn!
 * 
 * ci -u1.80 msg.h history.c

 * Revision 1.79  89/09/27  09:03:53  ken
 * y
 * y
 * 
 * ci -u1.79 msg_types.c msg_new.c history.c

 * Revision 1.78  89/09/26  11:35:34  ken
 * Caught two dup declarations
 * 
 * ci -u1.78 msg_sa.c history.c

 * Revision 1.77  89/09/25  20:42:32  ken
 * ...
 * 
 * ci -u1.77 msg.h history.c

 * Revision 1.76  89/09/25  16:48:15  ken
 * Declare externals
 * 
 * ci -u1.76 msg_sa.c history.c

 * Revision 1.75  89/09/19  11:58:27  patrick
 * put in some c++ varargs declarations
 * 
 * ci -u1.75 msg.h history.c

 * Revision 1.74  89/09/14  12:50:50  rcbc
 * Now uses FUN_TYPES define to choose old or new style function declarations.
 * This is defined selectively for ANSI C compiler we "like".
 * Also fixed a number of APOLLO typing errors.
 * 
 * ci -u1.74 msg.h msg_fio.c msg_fmt.c msg_memory.c msg_msgs.c msg_sa.c msg_types.c msg_utils.c history.c

 * Revision 1.73  89/09/13  14:37:36  ken
 * AIX syntax problem
 * 
 * ci -u1.73 msg_fio.c history.c

 * Revision 1.72  89/09/13  12:30:11  ken
 * More minor type problems
 * 
 * ci -u1.72 msg_fields.c history.c

 * Revision 1.71  89/09/13  09:55:11  ken
 * VOID
 * 
 * ci -u1.71 msg_sa.c history.c

 * Revision 1.70  89/09/12  09:52:31  ken
 * ...
 * 
 * ci -u1.70 msg_sa.c history.c

 * Revision 1.69  89/09/12  09:05:18  ken
 * Arrghh
 * 
 * ci -u1.69 msg_sa.c history.c

 * Revision 1.68  89/09/12  09:00:06  ken
 * won't compile the way Robert and Pat coded it
 * 
 * ci -u1.68 msg_sa.c history.c

 * Revision 1.67  89/09/12  08:37:23  ken
 * mallocate change
 * 
 * ci -u1.67 msg_memory.c history.c

 * Revision 1.66  89/09/08  13:35:21  patrick
 * Fixed declarations so the Isis header files will pass through the ATT C++ compiler
 * 
 * ci -u1.66 msg.h msg_utils.c history.c

 * Revision 1.65  89/08/30  14:16:58  rcbc
 * Fixed some uninitialized variable errors flagged by gcc.
 * 
 * ci -u1.65 msg_fmt.c history.c

 * Revision 1.64  89/08/28  15:35:56  rcbc
 * Fixed prototype of msg_delete.
 * 
 * ci -u1.64 msg.h history.c

 * Revision 1.63  89/08/28  15:27:45  rcbc
 * Fixes to make sure ISIS still compiles under non-ansi compilers.
 * 
 * ci -u1.63 msg_sa.c msg_fmt.c msg_new.c msg.h history.c

 * Revision 1.62  89/08/25  14:07:55  rcbc
 * Now typechecks with gcc -W
 * 
 * ci -u1.62 RCS history.c makefile mlib-l.c msg.h msg_fields.c msg_fio.c msg_fmt.c msg_memory.c msg_msgs.c msg_new.c msg_sa.c msg_types.c msg_utils.c sysfields.h history.c

 * Revision 1.61  89/08/10  15:10:56  rcbc
 * Added $Revision RCS keyword to all files.
 * 
 * ci -u1.61 RCS history.c makefile mlib-l.c msg.h msg_fields.c msg_fio.c msg_fmt.c msg_memory.c msg_msgs.c msg_new.c msg_sa.c msg_types.c msg_utils.c sysfields.h history.c

 * Revision 1.60  89/08/09  14:08:11  ken
 * misc MACH stuff
 * 
 * ci -u1.60 msg_types.c msg_fmt.c history.c

 * Revision 1.59  89/08/09  09:17:34  ken
 * ...
 * 
 * ci -u1.59 makefile history.c

 * Revision 1.58  89/08/07  09:52:51  ken
 * ...
 * 
 * ci -u1.58 msg.h history.c

 * Revision 1.57  89/08/07  09:45:55  ken
 * ...
 * 
 * ci -u1.57 sysfields.h history.c

 * Revision 1.56  89/08/03  22:20:58  ken
 * cc refused
 * 
 * ci -u1.56 sysfields.h history.c

 * Revision 1.55  89/07/26  16:28:08  ken
 * oops
 * 
 * ci -u1.55 msg_fmt.c history.c

 * Revision 1.54  89/07/26  12:40:44  ken
 * Relocate fields!
 * 
 * ci -u1.54 msg_new.c history.c

 * Revision 1.53  89/07/25  14:34:31  ken
 * ...
 * 
 * ci -u1.53 msg_new.c history.c

 * Revision 1.52  89/07/24  14:06:56  ken
 * Clean V1.2 checkin
 * 
 * ci -u1.52 msg.h msg_fields.c msg_memory.c msg_new.c history.c

 * Revision 1.51  89/07/13  11:03:06  ken
 * y
 * 
 * ci -u1.51 sysfields.h history.c

 * Revision 1.50  89/07/06  13:53:25  rcbc
 * Added CALL macro for calling user callback routines.
 * 
 * ci -u1.50 msg_types.c history.c

 * Revision 1.49  89/07/05  20:31:11  ken
 * ...
 * 
 * ci -u1.49 sysfields.h history.c

 * Revision 1.48  89/07/05  11:29:12  ken
 * minor "oops"
 * 
 * ci -u1.48 msg_types.c history.c

 * Revision 1.47  89/07/03  20:16:19  ken
 * July 4th spooler checkin
 * 
 * ci -u1.47 sysfields.h history.c

 * Revision 1.46  89/07/03  14:23:34  rcbc
 * Added debugging options.
 * 
 * ci -u1.46 makefile history.c

 * Revision 1.45  89/07/03  13:23:35  ken
 * fix a few buggies
 * 
 * ci -u1.45 msg_fields.c msg_fio.c history.c

 * Revision 1.44  89/06/26  13:03:48  ken
 * ...
 * 
 * ci -u1.44 msg_types.c history.c

 * Revision 1.43  89/06/20  13:09:07  ken
 * ...
 * 
 * ci -u1.43 sysfields.h history.c

 * Revision 1.42  89/06/19  15:51:34  ken
 * ref count wasn't being inherited from parent block
 * 
 * ci -u1.42 msg_new.c msg_memory.c history.c

 * Revision 1.41  89/06/19  13:57:25  ken
 * ...
 * 
 * ci -u1.41 msg_sa.c history.c

 * Revision 1.40  89/06/19  13:52:52  ken
 * ...
 * 
 * ci -u1.40 msg_sa.c history.c

 * Revision 1.39  89/06/19  13:30:33  ken
 * Implement a standalone mlib
 * 
 * ci -u1.39 makefile msg_sa.c history.c

 * Revision 1.38  89/06/09  10:26:03  ken
 * Change to V1.2, attribute source
 * 
 * ci -u1.38 msg.h sysfields.h history.c mlib-l.c msg_fields.c msg_fio.c msg_fmt.c msg_memory.c msg_msgs.c msg_new.c msg_types.c msg_utils.c history.c

 * Revision 1.37  89/06/08  12:40:51  ken
 * ...
 * 
 * ci -u1.37 msg_memory.c msg_new.c history.c

 * Revision 1.36  89/06/07  14:23:45  ken
 * change malloc decl.
 * 
 * ci -u1.36 msg_fmt.c msg_memory.c msg_types.c history.c

 * Revision 1.35  89/05/31  09:35:18  ken
 * convert calls to printf into calls to print
 * 
 * ci -u1.35 history.c mlib-l.c msg_fields.c msg_fio.c msg_fmt.c msg_memory.c msg_msgs.c msg_new.c msg_types.c msg_utils.c history.c

 * Revision 1.34  89/05/30  20:17:06  ken
 * better message
 * 
 * ci -u1.34 msg_fio.c history.c

 * Revision 1.33  89/05/30  15:38:37  ken
 * Oops, try try again
 * 
 * ci -u1.33 msg_memory.c history.c

 * Revision 1.32  89/05/30  14:46:45  ken
 * fix minor block dup. problem
 * 
 * ci -u1.32 msg.h msg_fields.c msg_fio.c msg_memory.c msg_msgs.c msg_new.c msg_utils.c history.c

 * Revision 1.31  89/05/26  13:57:00  ken
 * catch msg utility problem.
 * 
 * ci -u1.31 msg_utils.c history.c

 * Revision 1.30  89/05/18  13:14:50  ken
 * ret. null pointer on 0-length fields.
 * 
 * ci -u1.30 msg_fmt.c history.c

 * Revision 1.29  89/05/18  11:05:50  ken
 * addr_isnull -> aptr_isnull
 * 
 * ci -u1.29 msg.h msg_types.c msg_utils.c history.c

 * Revision 1.28  89/05/18  09:21:01  ken
 * back to quick addr_isnull macro
 * 
 * ci -u1.28 msg.h history.c

 * Revision 1.27  89/05/17  14:26:08  ken
 * oops
 * 
 * ci -u1.27 msg.h history.c

 * Revision 1.26  89/05/05  09:40:16  ken
 * make add_isnull more flexible
 * 
 * ci -u1.26 msg.h history.c

 * Revision 1.25  89/05/05  08:50:05  ken
 * disallow %x where x is user-defined on msg_put
 * 
 * ci -u1.25 msg_fmt.c history.c

 * Revision 1.24  89/05/02  17:37:28  rcbc
 * Added ALLEGRO_CL define (just to be safe).
 * 
 * ci -u1.24 makefile history.c

 * Revision 1.23  89/05/01  13:20:34  rcbc
 * Added msg_getscantype.
 * 
 * ci -u1.23 msg_utils.c history.c

 * Revision 1.22  89/05/01  11:28:22  ken
 * change for APOLLO which has a limit on asynchronous writes...
 * 
 * ci -u1.22 msg_fio.c history.c

 * Revision 1.21  89/04/26  20:32:23  ken
 * use $(CC) as per Ho request
 * 
 * ci -u1.21 makefile history.c

 * Revision 1.20  89/04/26  09:43:46  ken
 * Checkin of V1.2 alpha release
 * 
 * ci -u1.20 msg_fmt.c msg_memory.c msg_new.c history.c

 * Revision 1.19  89/04/04  15:26:36  ken
 * Changes for ISIS V1.2
 * 
 * ci -u1.19 msg.h sysfields.h history.c mlib-l.c msg_fields.c msg_fio.c msg_fmt.c msg_memory.c msg_msgs.c msg_new.c msg_types.c msg_utils.c history.c

 * Revision 1.18  89/01/31  11:07:19  rcbc
 * Embed rcs version number in library file.
 * 
 * ci -u1.18 makefile history.c

 * Revision 1.17  89/01/31  11:06:45  rcbc
 * Fixed rcsid decl.
 * 
 * Revision 1.16  89/01/30  17:52:18  rcbc
 * Updated for new checkin program.
 * 
 * Revision 1.15  89/01/30  14:24:32  ken
 * Correct more things that MACH noticed and complained about
 * 
 * ci -u1.15 msg_fields.c msg_memory.c history

 * Revision 1.14  89/01/27  15:45:58  ken
 * MACH complained.
 * 
 * ci -u1.14 msg_fmt.c history

 * Revision 1.13  89/01/26  11:00:42  ken
 * Reconcile msg_getfld pointer with manual
 * 
 * ci -u1.13 msg_fmt.c history

 * Revision 1.12  89/01/24  14:08:38  ken
 * Oops.
 * 
 * ci -u1.12 msg_fmt.c history

 * Revision 1.11  89/01/24  12:16:16  ken
 * MACH caught some minor C problems
 * 
 * ci -u1.11 msg_fmt.c history

 * Revision 1.10  89/01/10  10:56:44  ken
 * Automate check for varargs() problem
 * 
 * ci -u1.10 msg_fmt.c history

 * Revision 1.9  89/01/09  15:10:06  ken
 * implement pushes_by_ref automatic test
 * 
 * ci -u1.9 msg_fmt.c history

 * Revision 1.8  89/01/09  12:34:05  ken
 * Tommy did a non-portable comparison.
 * 
 * ci -u1.8 msg_fields.c history

 * Revision 1.7  89/01/06  09:22:21  ken
 * add OPTIM flag
 * 
 * ci -u1.7 makefile history

 * Revision 1.6  89/01/02  11:06:45  ken
 * don't run ranlib on an HPUX system
 * 
 * ci -u1.6 makefile history

 * Revision 1.5  88/12/15  09:32:07  ken
 * Edit to expand tabs and include V1.1 disclaimer
 * 
 * ci -u1.5 msg.h sysfields.h mlib-l.c msg_fields.c msg_fmt.c msg_memory.c msg_msgs.c msg_new.c msg_types.c msg_utils.c history

 * Revision 1.4  88/12/08  13:24:23  rcbc
 * A couple of error messages.
 * 
 * ci -u1.4 msg_memory.c history

 * Revision 1.3  88/12/08  13:23:52  rcbc
 * Include file msg_sysfields.h changed to sysfields.h
 * 
 * ci -u1.3 msg.h history

 * Revision 1.2  88/12/07  10:37:10  rcbc
 * Checkin of makefile.
 * 
 * ci -u1.2 makefile history

 * Revision 1.1  88/11/21  11:43:11  rcbc
 * checkpoint in midst of building release-1-1
 * 
 * ci -u1.1 mlib-l.c msg.h msg_fields.c msg_fmt.c msg_memory.c msg_msgs.c msg_new.c msg_sysfields.h msg_types.c msg_utils.c msg_fields+.c history

 * Revision 1.0  88/11/21  10:46:55  rcbc
 * Check-in of ~isis/distrib
 * 
 
 */
