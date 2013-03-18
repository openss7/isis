/* History file for demos. */
extern char demos_rcsid[] =
    "$Revision: 2.28 $$Date: 90/09/15 20:36:04 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/demos/RCS/history.c,v $";

/* $Log:	history.c,v $
 * Revision 2.28  90/09/15  20:36:04  ken
 * Go back to old interface, because it worked fine.
 * 
 * ci -u2.28 grid.c display11.c history.c

 * Revision 2.27  90/09/15  14:50:19  ken
 * Change rule for sync. mode
 * 
 * ci -u2.27 display11.c grid.c history.c

 * Revision 2.26  90/09/15  14:27:41  ken
 * Oops
 * 
 * ci -u2.26 grid.c history.c

 * Revision 2.25  90/09/15  14:10:36  ken
 * Eliminate exclude option
 * 
 * ci -u2.25 grid.c history.c

 * Revision 2.24  90/09/15  08:52:23  ken
 * Eliminate exclude mode
 * 
 * ci -u2.24 display11.c history.c

 * Revision 2.23  90/09/14  14:24:11  ken
 * Check for "bypass in use" flag
 * 
 * ci -u2.23 grid.c history.c

 * Revision 2.22  90/09/14  13:24:48  ken
 * Oops
 * 
 * ci -u2.22 testjoins.c history.c

 * Revision 2.21  90/09/14  13:24:09  ken
 * Modify to support 'x' option (exclude)
 * 
 * ci -u2.21 testjoins.c history.c

 * Revision 2.20  90/09/13  20:06:36  ken
 * Inhibit "stop" display for receive-only processes in stream mode
 * 
 * ci -u2.20 display11.c history.c

 * Revision 2.19  90/09/13  19:14:09  ken
 * Avoid silly combinations of parameters
 * 
 * ci -u2.19 display11.c history.c

 * Revision 2.18  90/08/27  13:37:14  rcbc
 * Added $revision.
 * 
 * ci -u2.18 lisp-test.lisp history.c

 * Revision 2.17  90/08/14  09:24:24  ken
 * Ralph says this works better under SUN OS 4.1
 * 
 * ci -u2.17 makefile history.c

 * Revision 2.16  90/08/13  14:43:12  ken
 * bad declaration
 * 
 * ci -u2.16 spread.h history.c

 * Revision 2.15  90/08/09  15:11:31  ken
 * unmatched endif
 * 
 * ci -u2.15 spread.h history.c

 * Revision 2.14  90/08/09  14:07:31  ken
 * Minor datatype problems
 * 
 * ci -u2.14 spread.blurb spread.c spread.h spreadparse.y history.c

 * Revision 2.13  90/08/07  16:19:14  rcbc
 * Fixed HPUX include file errors.
 * 
 * ci -u2.13 spread.h spread.c history.c

 * Revision 2.12  90/08/07  09:25:01  ken
 * MIPS has predefined values for these system calls too
 * In fact, almost everyone does
 * 
 * ci -u2.12 spread.h history.c

 * Revision 2.11  90/08/07  09:21:39  rcbc
 * Removed lh_xfer_client and lh_xfer_server since wanServer and wanClient
 * are supersets of those demos.
 * 
 * ci -u2.11 makefile history.c

 * Revision 2.10  90/08/06  13:55:07  ken
 * blurb for spreedsheet demo
 * 
 * ci -u2.10 spread.blurb history.c

 * Revision 2.9  90/08/02  13:28:18  rcbc
 * Improved handling of IE_AGAIN errors from x_begin.
 * 
 * ci -u2.9 bank.h bank.c teller.c history.c

 * Revision 2.8  90/08/02  11:57:24  ken
 * Better main loop
 * 
 * ci -u2.8 display11.c history.c

 * Revision 2.7  90/07/25  13:52:34  ken
 * minor changes
 * 
 * ci -u2.7 grid.c testjoins.c history.c

 * Revision 2.6  90/07/12  10:50:24  ken
 * Add Bob's spreadsheet
 * 
 * ci -u2.6 makefile make_grid spread.c spread.h spreadparse.y history.c

 * Revision 2.5  90/06/14  14:35:52  ken
 * Eliminate obselete programs
 * 
 * ci -u2.5 makefile history.c

 * Revision 2.4  90/06/13  15:02:24  patrick
 * bug fixes/updates
 * 
 * ci -u2.4 wanServer.c wanClient.c history.c

 * Revision 2.3  90/06/11  10:32:16  ken
 * pmk OPLIBS issue
 * 
 * ci -u2.3 makefile history.c

 * Revision 2.2  90/06/11  10:28:06  ken
 * compile ftest.F with CFLAGS too
 * 
 * ci -u2.2 ftest.F twenty.c makefile history.c

 * Revision 2.1  90/05/15  17:03:02  patrick
 * Cleaned up the interface
 * 
 * ci -u2.1 wanClient.c wanServer.c history.c

 * Revision 2.0  90/05/04  15:24:14  rcbc
 * 2.0
 * 
 * Revision 1.98  90/05/03  14:53:05  rcbc
 * Build of pmake now test whether rpcsvc library is present first.
 * 
 * ci -u1.98 makefile history.c

 * Revision 1.97  90/05/02  19:24:00  ken
 * minor
 * 
 * ci -u1.97 qa.c history.c

 * Revision 1.96  90/05/02  18:29:26  rcbc
 * Fix call to make pmake
 * 
 * ci -u1.96 makefile history.c

 * Revision 1.95  90/05/02  18:29:03  rcbc
 * Removed spurious malloc declaration.
 * 
 * ci -u1.95 testtasks.c history.c

 * Revision 1.94  90/05/02  09:24:36  ken
 * Fix "run/stop" button labelling.
 * 
 * ci -u1.94 display11.c history.c

 * Revision 1.93  90/05/01  16:25:23  ken
 * 4 format itenms but had 3 fields to print
 * 
 * ci -u1.93 cmd.c history.c

 * Revision 1.92  90/05/01  12:29:08  ken
 * V2.0 copyright disclaimer
 * 
 * ci -u1.92 bank.blurb bank.c bank.h display10.c display11.c ftest.F grid.blurb grid.c grid.h lh_sp_test.c lisp-test.lisp lisp_test.c make_grid makefile pmk qa.c questions.dat selftest.c silentjoins.c sun_grid.c teller.c testjoins.c testtasks.c twenty.blurb twenty.c twenty.h history.c

 * Revision 1.91  90/05/01  10:07:33  rcbc
 * Makefile fixes for HPUX and MIPS
 * 
 * ci -u1.91 grid.c make_grid makefile history.c

 * Revision 1.90  90/05/01  10:06:31  rcbc
 * Changed from using FD_SET and FD_CLR (which only exist on 4.3 derivatives
 * to using Isis's own Bvec bit vector types.
 * 
 * ci -u1.90 display11.c history.c

 * Revision 1.89  90/05/01  09:54:19  ken
 * Change to do viewid's as %d.%d
 * 
 * ci -u1.89 selftest.c twenty.c history.c

 * Revision 1.88  90/04/30  11:53:28  rcbc
 * Fixed VOID * .
 * 
 * ci -u1.88 wanClient.c wanServer.c lh_xfer_client.c lh_xfer_server.c history.c

 * Revision 1.87  90/04/30  11:38:23  rcbc
 * Checkin on Mesaac's last changes. Does anyone know how to run these.
 * 
 * ci -u1.87 lh_xfer_client.c lh_xfer_server.c wanServer.c wanClient.c history.c

 * Revision 1.86  90/04/26  09:23:46  rcbc
 * Changed names of library files.
 * 
 * ci -u1.86 makefile make_grid history.c

 * Revision 1.85  90/04/18  16:31:47  ken
 * ...
 * 
 * ci -u1.85 display11.c history.c

 * Revision 1.84  90/04/17  13:30:22  ken
 * *** empty log message ***
 * 
 * ci -u1.84 twenty.c history.c

 * Revision 1.83  90/04/17  09:32:02  rcbc
 * Removed unnecessary and wrong declaration of nullreply.
 * 
 * ci -u1.83 qa.c history.c

 * Revision 1.82  90/04/17  09:06:14  rcbc
 * Changes for make install.
 * 
 * ci -u1.82 make_grid history.c

 * Revision 1.81  90/04/17  09:04:40  rcbc
 * Fixed call to isis_select.
 * 
 * ci -u1.81 teller.c history.c

 * Revision 1.80  90/04/16  12:19:04  ken
 * Forgot to transfer new state variables
 * 
 * ci -u1.80 grid.c history.c

 * Revision 1.79  90/04/16  11:22:07  ken
 * ...
 * 
 * ci -u1.79 grid.c history.c

 * Revision 1.78  90/04/16  11:09:54  ken
 * Oops
 * 
 * ci -u1.78 grid.c history.c

 * Revision 1.77  90/04/16  10:57:00  ken
 * Fix pat's excluded streaming mode bug
 * 
 * ci -u1.77 display11.c grid.c history.c

 * Revision 1.76  90/04/16  09:15:08  ken
 * Weekend bug fixes....
 * y
 * 
 * ci -u1.76 grid.c display11.c qa.c history.c

 * Revision 1.75  90/04/13  14:55:47  ken
 * V2.0 fixes
 * 
 * ci -u1.75 grid.c display11.c history.c

 * Revision 1.74  90/04/13  14:55:32  ken
 * V2.0 fixes
 * 
 * ci -u1.74 grid.c display11.c history.c

 * Revision 1.73  90/04/12  08:51:58  rcbc
 * Fixed infinite memory alloc loop bug on stop button.
 * Restructured code to stop using isis_input and have an explicitly
 * code mainloop instead.
 * 
 * ci -u1.73 display11.c grid.c history.c

 * Revision 1.72  90/04/11  17:05:00  rcbc
 * syntax error.
 * 
 * ci -u1.72 makefile history.c

 * Revision 1.71  90/04/11  10:14:46  rcbc
 * Changes for make install.
 * 
 * ci -u1.71 makefile history.c

 * Revision 1.70  90/04/06  14:46:39  ken
 * ...
 * 
 * ci -u1.70 display11.c grid.c history.c

 * Revision 1.69  90/04/04  12:49:02  rcbc
 * Restructured to have an X oriented main loop rather than
 * using isis_mainloop. This exercises an ISIS/X starvation problem
 * which is currently fixed by a kluge in display11.c. 
 * Also changed a few variable names.
 * 
 * ci -u1.69 grid.c display11.c history.c

 * Revision 1.68  90/04/03  10:08:56  ken
 * V2.0 mods 
 * 
 * ci -u1.68 twenty.c twenty.h qa.c history.c

 * Revision 1.67  90/03/19  15:58:28  ken
 * fbcast,mbcast changes
 * 
 * ci -u1.67 display11.c grid.c grid.h history.c

 * Revision 1.66  90/03/19  15:57:48  ken
 * *** empty log message ***
 * 
 * ci -u1.66 display11.c grid.c grid.h history.c

 * Revision 1.65  90/03/14  17:06:19  rcbc
 * pmk target now checks whether there's a pmk directory.
 * 
 * ci -u1.65 makefile history.c

 * Revision 1.64  90/03/14  17:00:57  rcbc
 * Fixed typechecking problems.
 * 
 * ci -u1.64 grid.h selftest.c twenty.c history.c

 * Revision 1.63  90/03/09  11:21:51  ken
 * Make this a more vigorous test
 * 
 * ci -u1.63 testtasks.c history.c

 * Revision 1.62  90/02/16  11:16:06  ken
 * ....
 * 
 * ci -u1.62 teller.c history.c

 * Revision 1.61  90/02/07  16:25:00  ken
 * ...
 * 
 * ci -u1.61 sun_grid.c history.c

 * Revision 1.60  90/01/31  17:56:58  rcbc
 * Fixed wrong number of args to find_outcome call.
 * 
 * ci -u1.60 bank.c history.c

 * Revision 1.59  90/01/30  20:49:19  ken
 * address alignment change
 * 
 * ci -u1.59 lisp_test.c selftest.c sun_grid.c twenty.c history.c

 * Revision 1.58  90/01/26  10:48:31  ken
 * V2.0 editing changes
 * 
 * ci -u1.58 sun_grid.c history.c

 * Revision 1.57  90/01/04  13:09:44  ken
 * Various minor V2.0 stuff
 * 
 * ci -u1.57 display10.c history.c

 * Revision 1.56  89/11/30  17:29:18  rcbc
 * Fixed call to exit.
 * 
 * ci -u1.56 display11.c history.c

 * Revision 1.55  89/11/06  16:42:04  ken
 * ISIS V2.0 mlib changes
 * 
 * ci -u1.55 selftest.c sun_grid.c testjoins.c history.c

 * Revision 1.54  89/10/05  20:44:17  ken
 * Fix overwritten digit
 * 
 * ci -u1.54 sun_grid.c history.c

 * Revision 1.53  89/09/22  14:17:25  ken
 * ...
 * 
 * ci -u1.53 selftest.c testjoins.c history.c

 * Revision 1.52  89/09/13  12:32:30  ken
 * *** empty log message ***
 * 
 * ci -u1.52 bank.h history.c

 * Revision 1.51  89/09/13  10:01:11  ken
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
 * cd ../run_demos
 * 
 * ci -u1.51 bank.h grid.h lh_sp_test.c qa.c testtasks.c history.c

 * Revision 1.50  89/09/12  09:32:49  ken
 * ...
 * 
 * ci -u1.50 qa.c bank.h history.c

 * Revision 1.49  89/08/30  14:21:19  rcbc
 * Fixed some uninitialized variable errors flagged by gcc. 
 * Fixed makefile target for fortran test to avoid using gcc since
 * gcc doesn't compile fortran!
 * 
 * ci -u1.49 testjoins.c silentjoins.c grid.c makefile history.c

 * Revision 1.48  89/08/25  14:19:02  rcbc
 * Now typechecks with gcc -Wall  (actually sun_grid doesn't
 * because the SUN include files are non-standard.
 * 
 * ci -u1.48 RCS bank.blurb bank.c bank.h display10.c display11.c ftest.F grid.blurb grid.c grid.h history.c lh_sp_test.c lisp-test.lisp lisp_test.c make_grid makefile qa.c questions.dat selftest.c silentjoins.c sun_grid.c teller.c testjoins.c testtasks.c twenty.blurb twenty.c twenty.h history.c

 * Revision 1.47  89/08/17  16:48:25  ken
 * ...
 * 
 * ci -u1.47 make_grid makefile history.c

 * Revision 1.46  89/08/16  17:50:32  rcbc
 * Added group change monitor.
 * 
 * ci -u1.46 lisp-test.lisp history.c

 * Revision 1.45  89/08/15  13:07:23  rcbc
 * Added lisp_test.c target.
 * 
 * ci -u1.45 makefile history.c

 * Revision 1.44  89/08/15  11:36:42  rcbc
 * First checkin of lisp test programs.
 * 
 * ci -u1.44 lisp-test.lisp lisp_test.c history.c

 * Revision 1.43  89/08/10  14:46:47  rcbc
 * Added $Revision: 2.28 $ rcs keyword
 * 
 * ci -u1.43 RCS bank.blurb bank.c bank.h display10.c display11.c ftest.F grid.blurb grid.c grid.h history.c lh_sp_test.c make_grid makefile pmk qa.c questions.dat selftest.c silentjoins.c sun_grid.c teller.c testjoins.c testtasks.c twenty.blurb twenty.c twenty.h history.c

 * Revision 1.42  89/08/09  13:36:41  rcbc
 * Added lh_sp_test long haul spooler test target.
 * 
 * ci -u1.42 lh_sp_test.c makefile history.c

 * Revision 1.41  89/08/08  14:41:47  ken
 * add "selftest" to demos
 * 
 * ci -u1.41 selftest.c makefile history.c

 * Revision 1.40  89/08/04  12:25:27  ken
 * y
 * y
 * y
 * y
 * 
 * ci -u1.40 selftest.c makefile history.c

 * Revision 1.39  89/07/12  22:11:06  ken
 * deltee isis_logging calls
 * 
 * ci -u1.39 grid.c testjoins.c history.c

 * Revision 1.38  89/07/12  21:58:55  ken
 * ...
 * 
 * ci -u1.38 sun_grid.c history.c

 * Revision 1.37  89/07/10  10:57:27  rcbc
 * just testing checkin program.
 * 
 * ci -u1.37 grid.h history.c

 * Revision 1.36  89/07/10  09:51:08  rcbc
 * Removed silly "completed 1000 updates" behaviour.
 * Fixed update speed degradation after stop/go.
 * 
 * ci -u1.36 display10.c display11.c grid.c grid.h history.c

 * Revision 1.35  89/07/05  13:24:21  ken
 * ...
 * 
 * ci -u1.35 makefile history.c

 * Revision 1.34  89/07/05  13:21:41  ken
 * fix a minor compilation complaint
 * 
 * ci -u1.34 makefile history.c

 * Revision 1.33  89/06/26  10:44:24  ken
 * ...
 * 
 * ci -u1.33 bank.c history.c

 * Revision 1.32  89/06/16  15:55:13  ken
 * ...
 * 
 * ci -u1.32 sun_grid.c testjoins.c history.c

 * Revision 1.31  89/06/16  15:54:31  ken
 * ...
 * 
 * ci -u1.31 sun_grid.c testjoins.c history.c

 * Revision 1.30  89/06/15  14:40:58  ken
 * ...
 * 
 * ci -u1.30 bank.c history.c

 * Revision 1.29  89/06/15  14:37:32  ken
 * ..
 * 
 * ci -u1.29 bank.c history.c

 * Revision 1.28  89/06/07  14:52:28  ken
 * ...
 * 
 * ci -u1.28 make_grid makefile history.c

 * Revision 1.27  89/06/07  14:24:52  ken
 * various minor changes
 * 
 * ci -u1.27 sun_grid.c testjoins.c history.c

 * Revision 1.26  89/05/30  20:19:27  ken
 * ...
 * 
 * ci -u1.26 sun_grid.c testjoins.c history.c

 * Revision 1.25  89/05/30  14:17:27  rcbc
 * Added spreadsheet demo to build for X11, and then commented it out
 * because it doesn't work.
 * 
 * ci -u1.25 make_grid history.c

 * Revision 1.24  89/05/18  10:57:43  ken
 * don't compile F77 demo except on SUN3/4
 * 
 * ci -u1.24 makefile history.c

 * Revision 1.23  89/05/18  09:28:31  ken
 * switch to modern style
 * 
 * ci -u1.23 sun_grid.c history.c

 * Revision 1.22  89/05/12  16:12:42  ken
 * V1.2 checkin
 * 
 * ci -u1.22 makefile history.c

 * Revision 1.21  89/05/12  16:11:03  ken
 * V1.23 checkin
 * 
 * ci -u1.21 makefile testtasks.c history.c

 * Revision 1.20  89/05/10  20:46:26  ken
 * play with startup bug
 * 
 * ci -u1.20 twenty.c twenty.h history.c

 * Revision 1.19  89/05/05  10:12:35  ken
 * Modify to obey the new rules...
 * 
 * ci -u1.19 testtasks.c history.c

 * Revision 1.18  89/05/05  09:47:00  ken
 * Was better the old way after all....
 * 
 * ci -u1.18 sun_grid.c history.c

 * Revision 1.17  89/05/05  09:40:45  ken
 * Eliminate unreasonable gaddr assignment.
 * 
 * ci -u1.17 sun_grid.c history.c

 * Revision 1.16  89/05/04  10:19:22  ken
 * 1623 is the port in run_demos
 * 
 * ci -u1.16 ftest.F history.c

 * Revision 1.15  89/05/04  09:50:41  ken
 * test dropping in and out of ISIS
 * 
 * ci -u1.15 testtasks.c history.c

 * Revision 1.14  89/05/03  22:02:15  ken
 * call ISIS_ENTER/ISIS_EXIT to ensure that urgent test will run
 * 
 * ci -u1.14 testtasks.c history.c

 * Revision 1.13  89/05/03  16:54:52  rcbc
 * Checkin of ken's version before hacking.
 * 
 * ci -u1.13 testtasks.c history.c

 * Revision 1.12  89/05/01  14:41:17  ken
 * typo
 * 
 * ci -u1.12 bank.c history.c

 * Revision 1.11  89/04/27  09:55:22  ken
 * test new isis_accept_events mechanism
 * 
 * ci -u1.11 twenty.c history.c

 * Revision 1.10  89/04/26  10:12:46  ken
 * V1.2 master checkin
 * 
 * ci -u1.10 bank.blurb bank.c bank.h ftest.F grid.blurb grid.c grid.h make_grid makefile qa.c questions.dat silentjoins.c sun_grid.c teller.c testjoins.c testtasks.c twenty.blurb history.c

 * Revision 1.9  89/04/26  10:03:20  ken
 * V1.2 master checkin
 * 
 * ci -u1.9 bank.h grid.h twenty.h bank.c display10.c display11.c grid.c history.c qa.c silentjoins.c sun_grid.c teller.c testjoins.c testtasks.c twenty.c history.c

 * Revision 1.8  89/04/24  11:13:05  rcbc
 * Added include isis.h 
 * 
 * ci -u1.8 display10.c history.c

 * Revision 1.7  89/04/24  10:56:58  rcbc
 * Added include isis.h
 * 
 * ci -u1.7 display11.c history.c

 * Revision 1.6  89/01/31  11:40:58  rcbc
 * Reinstalled lost bug fix.
 * 
 * ci -u1.6 bank.c history.c

 * Revision 1.5  89/01/31  11:33:47  rcbc
 * Embedded rcs version number.
 * 
 * ci -u1.5 bank.c history.c

 * Revision 1.4  89/01/31  11:26:03  rcbc
 * Syntax error.
 * 
 * ci -u1.4 sun_grid.c grid.c teller.c history.c

 * Revision 1.3  89/01/31  10:15:02  rcbc
 * Embedded rcs version numbers in some of the demo programs.
 * 
 * ci -u1.3 grid.c sun_grid.c teller.c history.c

 * Revision 1.2  89/01/30  17:55:32  rcbc
 * Updated for new checkin program.
 * 
 * Revision 1.1  89/01/30  13:18:42  rcbc
 * Initial checkin of all demos.
 * 
 * ci -u1.1 bank.c display.c display10.c display11.c grid.c qa.c silentjoins.c sun_grid.c teller.c testjoins.c testtasks.c twenty.c bank.h grid.h twenty.h makefile make_grid history
 */
