/*  $RCSfile: pr_address.h,v $ $Revision: 2.12 $ $Date: 90/08/03 10:20:22 $  */
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
 */
/* Header for addressing the various processes and routines */
#ifndef PR_ADDRESS
#define PR_ADDRESS

/* Entry points for clients calling ISIS */

#define PR_STGPARTICIPANT       1
#define PR_ABRECV1              2
#define PR_ABRECV2              3
#define PR_GBRECV1              4
#define PR_GBRECV2              5
#define PR_GBRECVABORT          7
#define PR_GBRECVCONFIRM        8
#define PR_CBRECVPKT            9
#define PR_PGWANTVIEW           10
#define PR_PGLOOKUP             12
#define PR_ADDCACHE             13
#define PR_PGLIST               14
#define PR_STATS                15
#define CL_PGLIST               16
#define CL_REGISTER             17
#define CL_CREATE               18
#define CL_FBCAST               19
#define CL_CBCAST               21
#define CL_BCAST                23      /* BCAST is an alias for ABCAST */
#define CL_ABCAST               23
#define CL_GBCAST               25
#define CL_GBCAST_GROW          27
#define CL_GETVIEW              28
#define CL_LOOKUP               29
#define CL_WANTDUMP             30
#define CL_WANTFLUSH            31
#define CL_FDRESTART            32
#define CL_ECHO                 33
#define PR_ADD_GROUPVIEW        34
#define CL_PMONITOR             35
#define PR_FRAGMAN              36
#define PR_CLFAILED             37
#define FD_RECVNEWVIEW          38
#define FD_RECVACK              39
#define FD_RECVNEGACK           40
#define FD_RECVCOMMIT           41
#define FD_RECVRESTART          42
#define PR_IS_ALIVE             43
#define CL_SHUTDOWN             44
#define FD_RECVINCARN           45
#define PR_CREATE_CHK           46
#define FD_RECVDEADLIST         47
#define PR_RESCANSITES          48
#define PR_DOSCANSITES          49
#define PR_WANTDUMP             50
#define CL_PROBE                51
#define CL_HELLO                52
#define CL_DIED                 53

#ifdef  ISIS_ENTRY_NAMES
char	*isis_entry_names[] ={
	0,			/* 0 */
	0,			/* 1 */
	0,			/* 2 */
	0,			/* 3 */
	0,			/* 4 */
	0,			/* 5 */
	0,			/* 6 */
	0,			/* 7 */
	0,			/* 8 */
	0,     			/* 9 */
	0,			/* 10 */
	0,			/* 11 */
	0,			/* 12 */
	0,			/* 13 */
	0,			/* 14 */
	0,			/* 15 */
	"list",			/* 16 */
	"register",		/* 17 */
	"pg_create",		/* 18 */
	"fbcast",		/* 19 */
	0,			/* 20 */
	"cbcast",		/* 21 */
	0,			/* 22 */
	"abcast",		/* 23 */
	0,			/* 24 */
	"gbcast",		/* 25 */
	0,			/* 26 */
	"pg_addmemb",		/* 27 */
	"pg_getview",		/* 28 */
	"pg_lookup",		/* 29 */
	"cl_dump",		/* 30 */
	"flush",		/* 31 */
	"restart",		/* 32 */
	"echo",			/* 33 */
	0,			/* 34 */
	"pmonitor",		/* 35 */
	0,			/* 36 */
	0,			/* 37 */
	0,			/* 38 */
	0,			/* 39 */
	0,			/* 40 */
	0,			/* 41 */
	0,			/* 42 */
	0,			/* 43 */
	"shutdown",		/* 44 */
	0,			/* 45 */
	0,			/* 46 */
	0,			/* 47 */
	"rescan",		/* 48 */
	0,			/* 49 */
	"pr_dump",		/* 50 */
	"probe",		/* 51 */
	"hello",		/* 52 */
	"died"			/* 53 */
};
#endif

/*   <---- GENERIC entries start at 100 ----> */

#include "generic.h"

#define ADDR_LEN                128     /* Length of an alist */

extern address         NULLADDRESS;

#endif
