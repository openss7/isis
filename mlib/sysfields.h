/*  $RCSfile: sysfields.h,v $ $Revision: 2.5 $ $Date: 90/06/13 15:02:45 $  */
/*
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
 *	Originally coded by Ken Birman
 */
#ifndef     MSG_SYSFIELDS
#define     MSG_SYSFIELDS

#define     SYSFLD_USERLIMIT    0x7f	/* Highest value for user field names */
#define     SYSFLD_DELETED      0xff	/* Deleted field */

#define     SYSFLD_DESTS        0x81	/* True dests */
#define     SYSFLD_BYPASS       0x82	/* Bypass header */
#define     SYSFLD_PGVIEW       0x83	/* piggybacked pgview */
#define     SYSFLD_VIEWID       0x84	/* viewid for pg_verify */
#define     SYSFLD_GID          0x85	/* gid for pg_verify */
#define     SYSFLD_REPLYTO      0x86	/* proxy for replies if set */
#define     SYSFLD_PROTO        0x87	/* Protocol, if traced */
#define     SYSFLD_UNSPEC1      0x88	/* Unspecified, for toolkit routines */
#define     SYSFLD_UNSPEC2      0x89	/* Unspecified, for toolkit routines */
#define     SYSFLD_VCHANGE      0x8a	/* Changes the view of some group */
#define     SYSFLD_VREAD        0x8b	/* Reads the view of some group */
#define     SYSFLD_ACT          0x8c	/* Gives the activity id */
#define     SYSFLD_TRACE        0x8d	/* Trace this message through protos */
#define     SYSFLD_ERRNO        0x8e	/* Error number, in a reply to a client bcast */
#define     SYSFLD_SCAN         0x8f	/* Used by msg_putf and msg_getf */
#define     SYSFLD_GUARD        0x90	/* Used for guarded messages */
#define     SYSFLD_SCOPE        0x91	/* Scope of a bcast */
#define     SYSFLD_TRUEVIEW     0x92	/* True pgview, for iterated bcasts that fail */
#define     SYSFLD_CCREFUSED    0x93	/* cc_refused */
#define     SYSFLD_PROTID       0x94	/* ID used within broadcast protocols */
#define     SYSFLD_CBID         0x95	/* Cbcast id */
#define     SYSFLD_VERIFY       0x96	/* For verifying gbcast delivery */
#define     SYSFLD_LAZY         0x97	/* Set for lazy bcast */
#define     SYSFLD_SPSEQN       0x98	/* Spooler sequence number */
#define     SYSFLD_SPSCAN       0x99	/* Spooler version of SYSFLD_SCAN */
#define     SYSFLD_NETWORK      0x9a	/* Remote network */
#define     SYSFLD_EXCLUDE      0x9b	/* Don't deliver to this address */
#define     SYSFLD_FORWARD      0x9c	/* Forwarding info: if any */
#define     SYSFLD_ALIST        0x9d	/* Alist, used when sending to group dests */
#define     SYSFLD_FILE         0x9e	/* File, for spooler */
#define     SYSFLD_LHOPCODE     0x9f	/* Long-haul hop count */
#define     SYSFLD_DOFLUSH      0xa0	/* for bypass */
#define     SYSFLD_LH_FILEREP   0xa1	/* also for spooler */

#endif				/* MSG_SYSFIELDS */
