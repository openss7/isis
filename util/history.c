/* History file for util */
char util_rcsid[] = "$Revision: 2.37 $$Date: 90/09/17 09:11:31 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/history.c,v $";

/* $Log:	history.c,v $
 * Revision 2.37  90/09/17  09:11:31  ken
 * Oops: don't reset current sequence number
 * 
 * ci -u2.37 long_haul.c history.c

 * Revision 2.36  90/09/17  09:10:18  ken
 * This is what it should have been in the first place...
 * 
 * ci -u2.36 long_haul.c history.c

 * Revision 2.35  90/09/16  21:23:52  ken
 * This patches our new code when the line goes down 
 * just as the last block of a file is being sent
 * 
 * ci -u2.35 long_haul.c history.c

 * Revision 2.34  90/09/11  14:57:37  rcbc
 * fixed two uninitialized locals.
 * 
 * ci -u2.34 long_haul.c history.c

 * Revision 2.33  90/09/11  14:42:57  rcbc
 * Fixed prototype for do_lh_send
 * 
 * ci -u2.33 long_haul.h history.c

 * Revision 2.32  90/08/30  15:23:15  ken
 * Maintain counts on file transfer messages
 * 
 * ci -u2.32 long_haul.c long_haul.h spooler.c history.c

 * Revision 2.31  90/08/25  19:42:55  ken
 * extra '*'
 * 
 * ci -u2.31 lmgr.c history.c

 * Revision 2.30  90/08/17  14:02:42  patrick
 * the real checkin...
 * 
 * ci -u2.30 long_haul.c history.c

 * Revision 2.29  90/08/17  14:01:42  patrick
 * minor bugs, better log messages
 * 
 * ci -u2.29 long_haul.c history.c

 * Revision 2.28  90/08/14  16:08:59  ken
 * Use standard format to list members.
 * 
 * ci -u2.28 cmd.c history.c

 * Revision 2.27  90/08/14  12:53:18  ken
 * Syntax error
 * 
 * ci -u2.27 lmgr.c history.c

 * Revision 2.26  90/08/14  12:51:07  ken
 * directory is more standard than scandir
 * 
 * ci -u2.26 lmgr.c history.c

 * Revision 2.25  90/08/14  11:24:37  ken
 * Better check at startup
 * 
 * ci -u2.25 xmgr.c history.c

 * Revision 2.24  90/08/14  10:47:21  ken
 * Set socket options in a consistent way
 * 
 * ci -u2.24 isis.c history.c

 * Revision 2.23  90/08/14  09:29:27  ken
 * Better warnings in lmgr
 * 
 * ci -u2.23 lmgr.c history.c

 * Revision 2.22  90/08/13  20:33:36  ken
 * Renames old-format logs with this change (I hope)
 * 
 * ci -u2.22 lmgr.c history.c

 * Revision 2.21  90/08/13  20:27:55  ken
 * -Z flag wasn't working with multi-instance sites files
 * 
 * ci -u2.21 isis.c history.c

 * Revision 2.20  90/08/10  16:31:39  ken
 * Leave xmgr hung until lmgr is really finished
 * 
 * ci -u2.20 lmgr.c history.c

 * Revision 2.19  90/08/09  15:35:40  ken
 * Oops
 * 
 * ci -u2.19 isis.c history.c

 * Revision 2.18  90/08/09  15:21:59  ken
 * Fix the bind "loop"
 * 
 * ci -u2.18 isis.c history.c

 * Revision 2.17  90/08/08  11:30:16  tclark
 * Fix call to accept
 * 
 * ci -u2.17 long_haul.c history.c

 * Revision 2.16  90/08/07  16:06:16  rcbc
 * HP doesn't have setreuid, use setresuid instead.
 * 
 * ci -u2.16 rexec.c history.c

 * Revision 2.15  90/08/06  13:49:01  ken
 * AUX changes and xmgr fixes
 * 
 * ci -u2.15 isis.c prstat.c xmgr.c history.c

 * Revision 2.14  90/08/06  10:14:17  patrick
 * updates for "connect"
 * 
 * ci -u2.14 long_haul.c history.c

 * Revision 2.13  90/07/25  13:52:06  ken
 * V2.1 changes
 * 
 * ci -u2.13 isis.c history.c

 * Revision 2.12  90/07/11  14:37:56  ken
 * Extend isis.c handling of hostname
 * .,
 * 
 * ci -u2.12 isis.c history.c

 * Revision 2.11  90/07/05  11:51:43  ken
 * Put rexec.log in isisdir
 * 
 * ci -u2.11 rexec.c history.c

 * Revision 2.10  90/06/24  15:55:57  rcbc
 * Fixed prototype for do_send_file.
 * 
 * ci -u2.10 long_haul.h history.c

 * Revision 2.9  90/06/21  18:01:21  rcbc
 * Better fixes for isis.c duplicate sites on one machine mode.
 * 
 * ci -u2.9 isis.c history.c

 * Revision 2.8  90/06/21  17:54:11  rcbc
 * Interim fixes for multiple sites on a machine mode.
 * 
 * ci -u2.8 isis.c history.c

 * Revision 2.7  90/06/20  14:45:02  ken
 * fix setting of coord value in multi-site mode
 * .,`
 * 
 * ci -u2.7 isis.c history.c

 * Revision 2.6  90/06/20  14:36:43  ken
 * All Frank's strings are 80 bytes long!
 * 
 * ci -u2.6 rmgr.c news.c history.c

 * Revision 2.5  90/06/15  08:49:44  patrick
 * fixed connect being terminated by signals
 * 
 * ci -u2.5 long_haul.c history.c

 * Revision 2.4  90/06/13  15:01:35  patrick
 * bug fixes/updates
 * 
 * ci -u2.4 long_haul.c long_haul.h spooler.c history.c

 * Revision 2.3  90/05/15  16:12:59  patrick
 * These actually work for straightforward cases.  Much remains to be
 * done.
 * 
 * ci -u2.3 spooler.c long_haul.c history.c

 * Revision 2.2  90/05/09  17:28:42  rcbc
 * Fixed some silly but fatal errors in long_haul.c
 * 
 * ci -u2.2 long_haul.c history.c

 * Revision 2.1  90/05/07  17:34:53  rcbc
 * Added 5 second timeout to gethostbyname calls.
 * Does a better job of find the current host in the sites file (before
 * it would match the shortname of the current host against any prefix
 * of the full name in the host file. 
 * No longer does a gethostbyname on null site names.
 * *** However multiple sites at the same machine does not work
 *     and has not worked for some time it appears ***
 * 
 * ci -u2.1 isis.c history.c

 * Revision 2.0  90/05/04  15:23:42  rcbc
 * 2.0
 * 
 * Revision 1.167  90/05/03  12:07:03  rcbc
 * Added rcsid.
 * 
 * ci -u1.167 long_haul.c long_haul.h history.c

 * Revision 1.166  90/05/03  10:05:52  rcbc
 * If one sitename was a prefix of another then ISIS thought they were
 * the same site.
 * 
 * ci -u1.166 isis.c history.c

 * Revision 1.165  90/05/02  10:30:43  rcbc
 * Removed unused variable/uninitialized variables.
 * 
 * ci -u1.165 long_haul.c history.c

 * Revision 1.164  90/05/01  17:54:10  ken
 * oops
 * 
 * ci -u1.164 cmd.c history.c

 * Revision 1.163  90/05/01  10:57:28  ken
 * V2.0 copyright disclaimer
 * 
 * ci -u1.163 cmdy.h long_haul.h rexec.h rmgr.h cmd.c isis.c lmgr.c long_haul.c news.c prstat.c rexec.c rmgr.c rmgr_cmd.c spooler.c tabex.c xmgr.c history.c

 * Revision 1.162  90/05/01  09:54:41  ken
 * Use %d.%d notation for viewid's
 * 
 * ci -u1.162 cmd.c history.c

 * Revision 1.161  90/04/30  11:54:50  rcbc
 * Fixed VOID * (again) and lh_remove, lh_append (again!)
 * 
 * ci -u1.161 long_haul.c long_haul.h history.c

 * Revision 1.160  90/04/30  11:35:59  rcbc
 * Checkin of Mesaac's last set of changes. Compiles OK, but needs to
 * be tested with latest release.
 * 
 * ci -u1.160 long_haul.c long_haul.h history.c

 * Revision 1.159  90/04/27  15:18:11  ken
 * Copyright notice
 * 
 * Revision 1.158  90/04/26  16:38:52  ken
 * Improve formatting
 * 
 * ci -u1.158 prstat.c history.c

 * Revision 1.157  90/04/26  09:24:54  rcbc
 * Changed names of library files.
 * 
 * ci -u1.157 makefile history.c

 * Revision 1.156  90/04/25  14:33:10  ken
 * Oops
 * 
 * ci -u1.156 prstat.c history.c

 * Revision 1.155  90/04/25  14:22:15  ken
 * Better format
 * 
 * ci -u1.155 prstat.c history.c

 * Revision 1.154  90/04/24  14:43:06  ken
 * formating fixes
 * 
 * ci -u1.154 prstat.c history.c

 * Revision 1.153  90/04/20  15:23:33  ken
 * better stats
 * 
 * ci -u1.153 prstat.c history.c

 * Revision 1.152  90/04/20  14:23:45  ken
 * Various changes
 * 
 * ci -u1.152 isis.c history.c

 * Revision 1.151  90/04/17  13:29:56  ken
 * Various changes
 * 
 * ci -u1.151 isis.c history.c

 * Revision 1.150  90/04/16  09:11:29  ken
 * Weekend bug fixes
 * 
 * ci -u1.150 isis.c history.c

 * Revision 1.149  90/04/11  10:17:17  rcbc
 * Changes for make install.
 * 
 * ci -u1.149 makefile history.c

 * Revision 1.148  90/04/06  14:23:43  ken
 * V2.0 bug fixes
 * 
 * ci -u1.148 isis.c history.c

 * Revision 1.147  90/04/05  15:04:15  ken
 * Chug
 * 
 * ci -u1.147 isis.c history.c

 * Revision 1.146  90/04/05  11:11:06  ken
 * isi_remote
 * 
 * ci -u1.146 isis.c history.c

 * Revision 1.145  90/04/03  10:02:37  ken
 * HPUX changes
 * 
 * ci -u1.145 long_haul.c long_haul.h history.c

 * Revision 1.144  90/04/02  14:51:13  ken
 * Minor stuff
 * 
 * ci -u1.144 isis.c long_haul.c rexec.c history.c

 * Revision 1.143  90/03/29  10:05:11  rcbc
 * Fixed void * --> VOID * errors.
 * 
 * ci -u1.143 long_haul.c long_haul.h history.c

 * Revision 1.142  90/03/19  16:07:37  ken
 * ...
 * 
 * ci -u1.142 isis.c history.c

 * Revision 1.141  90/03/09  11:21:25  ken
 * Missing some void dec.
 * 
 * ci -u1.141 xmgr.c history.c

 * Revision 1.140  90/03/08  09:56:32  ken
 * Support for -H option
 * 
 * ci -u1.140 isis.c history.c

 * Revision 1.139  90/03/07  22:44:42  ken
 * Re-activate the old -Hxxx and site/[1,2,3,4] business
 * 
 * ci -u1.139 isis.c history.c

 * Revision 1.138  90/03/07  21:47:52  ken
 * ...
 * 
 * ci -u1.138 long_haul.h history.c

 * Revision 1.137  90/03/07  10:44:22  rcbc
 * Fixed a few name clashes between spooler.c routines and tk_spool2.c 
 * routines.
 * 
 * ci -u1.137 long_haul.c spooler.c long_haul.h history.c

 * Revision 1.136  90/03/06  15:58:54  ken
 * *** empty log message ***
 * 
 * ci -u1.136 isis.c history.c

 * Revision 1.135  90/02/16  17:33:22  rcbc
 * Minor syntax errors found with gcc.
 * 
 * ci -u1.135 xmgr.c lmgr.c history.c

 * Revision 1.134  90/02/16  16:49:15  rcbc
 * ..
 * 
 * ci -u1.134 long_haul.h history.c

 * Revision 1.133  90/02/16  16:44:32  rcbc
 * ...
 * 
 * ci -u1.133 long_haul.c history.c

 * Revision 1.132  90/02/16  11:15:49  ken
 * ...
 * 
 * ci -u1.132 cmd.c history.c

 * Revision 1.131  90/02/13  13:43:09  ken
 * Use a vanilla timeout mechanism
 * 
 * ci -u1.131 xmgr.c history.c

 * Revision 1.130  90/02/06  14:51:34  ken
 * ...
 * 
 * ci -u1.130 lmgr.c xmgr.c history.c

 * Revision 1.129  90/01/30  20:46:32  ken
 * address alignment changes
 * 
 * ci -u1.129 cmd.c news.c rexec.c spooler.c rmgr.c history.c

 * Revision 1.128  90/01/26  11:00:54  ken
 * suntools_loaded bug
 * 
 * ci -u1.128 isis.c history.c

 * Revision 1.127  89/12/15  13:40:53  ken
 * ISIS V2.0 major checkin
 * 
 * ci -u1.127 cmd.c isis.c long_haul.c spooler.c xmgr.c history.c

 * Revision 1.126  89/11/15  16:27:47  rcbc
 * Now loops at startup upon IE_MUSTJOIN errors from pg_join.
 * 
 * ci -u1.126 xmgr.c history.c

 * Revision 1.125  89/11/15  09:25:43  ken
 * ....
 * 
 * ci -u1.125 cmd.c history.c

 * Revision 1.124  89/11/15  09:21:13  ken
 * ...
 * 
 * ci -u1.124 news.c history.c

 * Revision 1.123  89/11/13  14:02:22  ken
 * 
 * 
 * ci -u1.123 isis.c history.c

 * Revision 1.122  89/11/08  09:58:34  mak
 * *** empty log message ***
 * 
 * ci -u1.122 spooler.c history.c

 * Revision 1.121  89/11/08  09:58:07  mak
 * *** empty log message ***
 * 
 * ci -u1.121 long_haul.c history.c

 * Revision 1.120  89/11/08  09:55:52  mak
 * Just to update the DEVEL version after checkin in the stable one.
 * 
 * ci -u1.120 spooler.c history.c

 * Revision 1.119  89/11/08  09:50:20  mak
 * Just to update the version in DEVEL/util
 * 
 * ci -u1.119 long_haul.c history.c

 * Revision 1.118  89/11/08  09:45:08  mak
 * Addition of new entry pooints to interact directly 
 * with the long haul package. Also SP_SPOOL entry
 * triggers lh_spool invocation instead of spool.
 * 
 * ci -u1.118 spooler.c history.c

 * Revision 1.117  89/11/08  09:40:22  mak
 * Radical change from the previous version. However
 * the only new functionality is the long haul file
 * transfer.
 * 
 * ci -u1.117 long_haul.c history.c

 * Revision 1.116  89/10/26  12:14:39  ken
 * ISIS V2.0 mlib changes
 * 
 * ci -u1.116 cmd.c isis.c history.c

 * Revision 1.115  89/09/26  10:18:24  ken
 * ....
 * 
 * ci -u1.115 cmd.c history.c

 * Revision 1.114  89/09/26  10:14:05  ken
 * ....
 * 
 * ci -u1.114 cmd.c history.c

 * Revision 1.113  89/09/26  09:40:45  ken
 * Change to not fork "more" off directlky
 * 
 * ci -u1.113 cmd.c history.c

 * Revision 1.112  89/09/26  09:17:51  ken
 * Add a little security
 * 
 * ci -u1.112 isis.c history.c

 * Revision 1.111  89/09/25  16:29:35  ken
 * Add FORK_BROKEN option
 * 
 * ci -u1.111 isis.c rexec.c rmgr.c history.c

 * Revision 1.110  89/09/14  10:13:46  rcbc
 * Removed spurious exit call from isis.c
 * 
 * ci -u1.110 isis.c history.c

 * Revision 1.109  89/09/13  10:09:15  ken
 * SP_INFINITY feature
 * 
 * ci -u1.109 spooler.c history.c

 * Revision 1.108  89/09/13  10:00:14  ken
 * VOID
 * 
 * ci -u1.108 isis.c history.c

 * Revision 1.107  89/09/12  12:40:48  ken
 * ..
 * 
 * ci -u1.107 long_haul.c spooler.c history.c

 * Revision 1.106  89/09/12  09:19:34  ken
 * More minor problems
 * 
 * ci -u1.106 cmd.c history.c

 * Revision 1.105  89/09/11  11:30:34  rcbc
 * Fixed typo causing isis -t not to work.
 * Moved version string to isis.h, and updated to say 1.3
 * 
 * ci -u1.105 isis.c history.c

 * Revision 1.104  89/09/07  12:15:44  rcbc
 * Changed auto restart processing. Now takes integer number of minutes
 * to wait before restart argument following -A option.
 * Now imposes limit of 5 restarts per 24 hours, not 5 restarts total.
 * Removed all but one goto/label.
 * 
 * ci -u1.104 isis.c history.c

 * Revision 1.103  89/09/05  15:00:09  rcbc
 * Removed typedef bool since thats now in cl_typedefs.h
 * 
 * ci -u1.103 news.c rmgr.c history.c

 * Revision 1.102  89/08/30  14:17:44  rcbc
 * Fixed some uninitialized variable errors flagged by gcc.
 * 
 * ci -u1.102 long_haul.c history.c

 * Revision 1.101  89/08/25  14:10:27  rcbc
 * Now typechecks with gcc -W
 * 
 * ci -u1.101 RCS cmd.c cmdl.l cmdy.h cmdy.y fork.h history.c isis.c lmgr.c long_haul.c makefile news.c prstat.c rexec.c rexec.h rmgr.c rmgr.h rmgr_cmd.c spooler.c tabex.c xmgr.c history.c

 * Revision 1.100  89/08/18  14:38:27  ken
 * ...
 * 
 * ci -u1.100 rexec.c history.c

 * Revision 1.99  89/08/15  11:06:51  ken
 * wouldn't compile as it was
 * 
 * ci -u1.99 spooler.c long_haul.c history.c

 * Revision 1.98  89/08/10  15:00:51  rcbc
 * Added $Revision rcs keyword to those files that didn't have it already.
 * 
 * ci -u1.98 RCS cmd.c cmdl.l cmdy.h cmdy.y history.c isis.c lmgr.c long_haul.c makefile news.c prstat.c rexec.c rexec.h rmgr.c rmgr.h rmgr_cmd.c spooler.c tabex.c xmgr.c history.c

 * Revision 1.97  89/08/08  14:46:05  mak
 * Suppression of TRACE, use of db_spool, introduction of localNet
 * 
 * ci -u1.97 long_haul.c history.c

 * Revision 1.96  89/08/08  09:19:13  ken
 * Add -v flag
 * 
 * ci -u1.96 spooler.c history.c

 * Revision 1.95  89/08/04  13:35:56  ken
 * *** empty log message ***
 * 
 * ci -u1.95 spooler.c history.c

 * Revision 1.94  89/07/29  10:32:18  ken
 * ...
 * 
 * ci -u1.94 long_haul.c history.c

 * Revision 1.93  89/07/28  23:10:13  ken
 * ...
 * 
 * ci -u1.93 spooler.c history.c

 * Revision 1.92  89/07/27  10:40:32  ken
 * Eliminate spurious spooler rename error messages
 * 
 * ci -u1.92 spooler.c history.c

 * Revision 1.91  89/07/26  12:45:30  ken
 * ....
 * 
 * ci -u1.91 lmgr.c spooler.c history.c

 * Revision 1.90  89/07/25  17:12:58  mak
 * 
 * 
 * ci -u1.90 long_haul.c history.c

 * Revision 1.89  89/07/25  10:42:40  ken
 * ...
 * 
 * ci -u1.89 spooler.c history.c

 * Revision 1.88  89/07/24  20:32:11  mak
 * Major change in the treatment of failures and member departuer.
 * 
 * ci -u1.88 long_haul.c history.c

 * Revision 1.87  89/07/24  14:05:52  ken
 * V1.2 cleanup
 * 
 * ci -u1.87 long_haul.c spooler.c history.c

 * Revision 1.86  89/07/21  14:06:48  ken
 * ...
 * 
 * ci -u1.86 spooler.c history.c

 * Revision 1.85  89/07/19  17:54:02  mak
 * Removing uneccerary comments from the long_haul.c file.
 * 
 * ci -u1.85 long_haul.c history.c

 * Revision 1.84  89/07/14  15:43:18  ken
 * Integrate with Messac's code
 * 
 * ci -u1.84 long_haul.c history.c

 * Revision 1.83  89/07/13  11:11:44  ken
 * oops
 * 
 * ci -u1.83 makefile history.c

 * Revision 1.82  89/07/12  22:00:37  ken
 * y
 * y
 * y
 * Nightly checkin
 * 
 * ci -u1.82 lmgr.c isis.c spooler.c history.c

 * Revision 1.81  89/07/07  12:43:40  ken
 * ..
 * 
 * ci -u1.81 long_haul.c makefile history.c

 * Revision 1.80  89/07/06  22:37:18  ken
 * Various minor hacks
 * 
 * ci -u1.80 isis.c spooler.c history.c

 * Revision 1.79  89/07/03  20:16:00  ken
 * July 4th spooler checkin
 * 
 * ci -u1.79 spooler.c history.c

 * Revision 1.78  89/07/03  13:25:04  ken
 * fix some bugs
 * 
 * ci -u1.78 spooler.c history.c

 * Revision 1.77  89/06/27  10:37:49  ken
 * ...
 * 
 * ci -u1.77 spooler.c history.c

 * Revision 1.76  89/06/26  15:50:17  ken
 * ...
 * 
 * ci -u1.76 spooler.c history.c

 * Revision 1.75  89/06/26  13:01:58  ken
 * ...
 * 
 * ci -u1.75 makefile spooler.c history.c

 * Revision 1.74  89/06/22  11:08:55  ken
 * ....
 * 
 * ci -u1.74 spooler.c history.c

 * Revision 1.73  89/06/16  15:53:25  ken
 * y
 * ...
 * 
 * ci -u1.73 spooler.c history.c

 * Revision 1.72  89/06/15  14:57:51  ken
 * ...
 * 
 * ci -u1.72 isis.c history.c

 * Revision 1.71  89/06/15  14:39:35  ken
 * 
 * 
 * ci -u1.71 cmd.c history.c

 * Revision 1.70  89/06/15  14:27:26  ken
 * ...
 * 
 * ci -u1.70 cmd.c rmgr.c history.c

 * Revision 1.69  89/06/15  14:26:05  ken
 * ...
 * 
 * ci -u1.69 cmd.c rmgr.c history.c

 * Revision 1.68  89/06/15  14:22:20  ken
 * ...
 * 
 * ci -u1.68 rmgr.c cmd.c history.c

 * Revision 1.67  89/06/15  14:16:59  ken
 * ...
 * 
 * ci -u1.67 news.c history.c

 * Revision 1.66  89/06/15  12:27:41  ken
 * ...
 * 
 * ci -u1.66 makefile spooler.c history.c

 * Revision 1.65  89/06/13  15:21:57  ken
 * ...
 * 
 * ci -u1.65 spooler.c history.c

 * Revision 1.64  89/06/13  08:33:12  ken
 * HP sprintf doesn't return addr of first arg
 * 
 * ci -u1.64 rmgr.h rmgr.c history.c

 * Revision 1.63  89/06/12  09:37:45  ken
 * change VERSION number in isis.c
 * 
 * ci -u1.63 isis.c history.c

 * Revision 1.62  89/06/09  11:31:17  ken
 * V1.2 header and attribution changes
 * 
 * ci -u1.62 cmdy.h rexec.h rmgr.h cmd.c history.c isis.c lmgr.c news.c prstat.c rexec.c rmgr.c rmgr_cmd.c tabex.c xmgr.c history.c

 * Revision 1.61  89/06/08  09:34:00  ken
 * ISIS spooler
 * 
 * ci -u1.61 spooler.c history.c

 * Revision 1.60  89/06/07  14:24:13  ken
 * checkin malloc decl.
 * 
 * ci -u1.60 isis.c lmgr.c history.c

 * Revision 1.59  89/06/01  11:15:10  ken
 * ...
 * 
 * ci -u1.59 isis.c history.c

 * Revision 1.58  89/06/01  10:13:44  ken
 * ...
 * 
 * ci -u1.58 isis.c history.c

 * Revision 1.57  89/06/01  09:47:37  ken
 * ...
 * 
 * ci -u1.57 xmgr.c isis.c history.c

 * Revision 1.56  89/06/01  09:41:05  ken
 * slow down the xmgr startup to avoid race with lmgr
 * 
 * ci -u1.56 xmgr.c history.c

 * Revision 1.55  89/05/30  20:18:24  ken
 * ...
 * 
 * ci -u1.55 isis.c history.c

 * Revision 1.54  89/05/26  10:05:54  ken
 * wait -> wait3
 * 
 * ci -u1.54 isis.c rexec.c history.c

 * Revision 1.53  89/05/24  14:01:14  rcbc
 * Fixed command line prompting (broken by isis_accept_events changes).
 * 
 * ci -u1.53 cmd.c history.c

 * Revision 1.52  89/05/22  14:33:12  ken
 * fix wait3 problem
 * 
 * ci -u1.52 rexec.c isis.c history.c

 * Revision 1.51  89/05/18  13:32:33  ken
 * eureka!
 * 
 * ci -u1.51 prstat.c history.c

 * Revision 1.50  89/05/18  13:00:50  ken
 * make pretty termination messages: isis: detected termination of <../bin/lmgr>
 * 
 * ci -u1.50 isis.c history.c

 * Revision 1.49  89/05/18  12:30:26  ken
 * ...
 * 
 * ci -u1.49 prstat.c history.c

 * Revision 1.48  89/05/18  12:24:34  ken
 * try and end up with something that will compile elsewhere...
 * 
 * ci -u1.48 prstat.c history.c

 * Revision 1.47  89/05/18  12:24:09  ken
 * ...
 * 
 * ci -u1.47 prstats.c history.c

 * Revision 1.46  89/05/12  09:20:44  ken
 * fix fork
 * 
 * ci -u1.46 vi co -l rmgr.c rexec.c isis.c cmd.c history.c

 * Revision 1.45  89/05/12  09:09:44  ken
 * cd ../clib
 * co -l isis.h
 * include time.h
 * /.
 * 
 * ci -u1.45 prstat.c history.c

 * Revision 1.44  89/05/03  14:59:27  ken
 * nothing
 * 
 * ci -u1.44 isis.c history.c

 * Revision 1.43  89/05/01  16:40:58  ken
 * Apollo fork(0 doesn't work
 * 
 * ci -u1.43 cmd.c history.c

 * Revision 1.42  89/05/01  14:27:38  ken
 * invoke chwait
 * 
 * ci -u1.42 isis.c history.c

 * Revision 1.41  89/05/01  14:27:14  ken
 * fork back to vfork
 * 
 * ci -u1.41 rexec.c history.c

 * Revision 1.40  89/05/01  14:09:58  ken
 * Improve an error message
 * 
 * ci -u1.40 rmgr.c history.c

 * Revision 1.39  89/05/01  13:59:19  ken
 *  oops
 * .,
 * 
 * ci -u1.39 cl_isis.c history.c

 * Revision 1.38  89/04/26  10:25:51  ken
 * define MAXNAMELEN
 * 
 * ci -u1.38 rmgr.h history.c

 * Revision 1.37  89/04/26  10:01:03  ken
 * V1.2 master checkin
 * 
 * ci -u1.37 isis.c history.c

 * Revision 1.36  89/04/26  10:00:29  ken
 * V1.2 master checkin
 * 
 * ci -u1.36 isis.c news.c history.c

 * Revision 1.35  89/04/26  09:59:35  ken
 * V1.2 master checkin
 * 
 * ci -u1.35 cmd.c isis.c makefile new.c prstat.c rexec.c rmgr.c rmgr.h rmgr_cmd.c xmgr.c history.c

 * Revision 1.34  89/01/31  09:56:04  rcbc
 * Updated to use history.c file, and embed rcs version number in object file.
 * 
 * ci -u1.34 cmd.c isis.c lmgr.c makefile news.c prstat.c rexec.c rmgr.c rmgr_cmd.c xmgr.c history.c

 * Revision 1.33  89/01/31  09:48:36  rcbc
 * *** empty log message ***
 * 
 * Revision 1.32  89/01/30  17:53:53  rcbc
 * Updated for new checkin program.
 * 
 * Revision 1.31  89/01/30  14:27:57  ken
 * Correct more things that MACH cared about
 * 
 * ci -u1.31 isis.c history

 * Revision 1.30  89/01/24  15:39:52  ken
 * MACH complained
 * 
 * ci -u1.30 isis.c history

 * Revision 1.29  89/01/09  15:11:24  ken
 * implement pushes_by_ref automatic test
 * 
 * ci -u1.29 isis.c history

 * Revision 1.28  89/01/09  12:53:56  ken
 * Create fork.h
 * 
 * ci -u1.28 fork.h history

 * Revision 1.27  89/01/09  12:49:38  ken
 * fix something HPUX complains about
 * 
 * ci -u1.27 cmd.c history

 * Revision 1.26  89/01/06  11:05:46  ken
 * Improve auto-restart mechanism
 * 
 * ci -u1.26 isis.c history

 * Revision 1.25  89/01/06  09:53:32  ken
 * Add OPTIM option
 * 
 * ci -u1.25 makefile history

 * Revision 1.24  89/01/06  09:35:46  ken
 * HPUX lacks strings.h
 * 
 * ci -u1.24 cmd.c isis.c news.c rmgr.c rmgr_cmd.c history

 * Revision 1.23  88/12/19  14:04:00  rcbc
 * Turned off debug mode on compiles.
 * 
 * ci -u1.23 makefile history

 * Revision 1.22  88/12/19  14:02:38  rcbc
 * Fixed bug in xfer_out calls. 
 * Turned off debug defines.
 * 
 * ci -u1.22 xmgr.c history

 * Revision 1.21  88/12/19  13:43:03  ken
 * eliminate in-line mention of #pragma in favor of an include
 * 
 * ci -u1.21 isis.c cmd.c rexec.c rmgr.c history

 * Revision 1.20  88/12/19  11:17:03  ken
 * add pragrma on registers in vfork
 * 
 * ci -u1.20 cmd.c cmdl.l cmdy.y isis.c rexec.c rmgr.c history

 * Revision 1.19  88/12/19  10:20:50  ken
 * complain if port numbers are 0,0,0 in sites file
 * ./
 * 
 * ci -u1.19 isis.c history

 * Revision 1.18  88/12/15  14:35:13  rcbc
 * Got it to recognize new XMGR process. 
 * 
 * ci -u1.18 cmd.c history

 * Revision 1.17  88/12/15  09:38:04  ken
 * Edit to expand tabs and include V1.1 disclaimer
 * .,
 * 
 * ci -u1.17 cmdy.h rexec.h rmgr.h cmd.c isis.c lmgr.c news.c prstat.c rexec.c rmgr.c rmgr_cmd.c tabex.c xmgr.c history

 * Revision 1.16  88/12/09  16:48:31  rcbc
 * Changed to a presumed-abort scheme.
 * 
 * ci -u1.16 xmgr.c history

 * Revision 1.15  88/12/09  13:59:18  rcbc
 * checkpoint.
 * 
 * ci -u1.15 xmgr.c history

 * Revision 1.14  88/12/08  13:14:32  rcbc
 * I was confused about vector arguments and pointers.
 * 
 * ci -u1.14 xmgr.c history

 * Revision 1.13  88/12/08  13:13:37  rcbc
 * Fixed link of tabex to use LDFLAGS.
 * 
 * ci -u1.13 makefile history

 * Revision 1.12  88/12/08  13:04:49  rcbc
 * Replaced use of wait3 by wait. Fixed HPUX version to use SIGCLD for
 * SIGCHLD.
 * 
 * ci -u1.12 rexec.c rmgr.c history

 * Revision 1.11  88/12/07  10:38:10  rcbc
 * First checkin of makefile.
 * 
 * ci -u1.11 makefile history

 * Revision 1.10  88/11/30  20:25:30  rcbc
 * Simple version of recovery manager.
 * 
 * ci -u1.10 xmgr.c history

 * Revision 1.9  88/11/30  15:17:14  ken
 * 
 * ci -u1.9 isis.c history

 * 
 * Revision 1.8  88/11/30  15:12:04  ken
 * oops....
 * 
 * ci -u1.8 isis.c history

 * Revision 1.7  88/11/30  15:09:02  ken
 * detect case where someone is running an incompatible version of ISIS
 * 
 * ci -u1.7 isis.c history

 * Revision 1.6  88/11/29  12:22:09  ken
 * improve an error message in case of quick restart after failure
 * 
 * ci -u1.6 isis.c history

 * Revision 1.5  88/11/29  12:20:08  ken
 * improve error messages on failed rexec
 * 
 * ci -u1.5 rexec.c history

 * Revision 1.4  88/11/29  12:14:17  ken
 * *** empty log message ***
 * 
 * ci -u1.4 rexec.c history

 * Revision 1.3  88/11/29  12:05:45  ken
 * fix race condition during startup query sequence
 * 
 * ci -u1.3 isis.c history

 * Revision 1.2  88/11/28  13:03:29  ken
 * change to eliminate race on startup
 * 
 * ci -u1.2 isis.c history

 * Revision 1.1  88/11/21  13:12:53  rcbc
 * 'checkpoint
 * 
 * ci -u1.1 -f -m'checkpoint in midst of building release-1-1' cmd.c cmdl.l cmdy.h cmdy.y isis.c news.c rexec.c rexec.h rmgr.c lmgr.c prstat.c rmgr.h rmgr_cmd.c tabex.c history

 * Revision 1.0  88/11/21  10:47:23  rcbc
 * Check-in of ~isis/distrib
 * 
 
*/
