/*  $RCSfile: rexec.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:03 $  */
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
 */
/* Rexec request code */
#define REXEC_REQ       0

/* Field names used in rexec system call */
#define RE_PROG         1
#define RE_ARGS         2
#define RE_ENV          3
#define RE_USER         4
#define RE_PASSWD       5

/* Limits on number of arguments, environment variables */
#define MAX_ARGS        5000
#define MAX_ENV         200
