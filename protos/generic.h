/*  $RCSfile: generic.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:35 $  */
/*
 *      Basic breakdown of entry points:
 *              0-SYS_BASE are different for each program
 *              SYS_BASE..255 are the "generic" entry points used by toolkit routines
 *              MAXENTRIES is the maximum entry number
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
#ifndef GEN_ADDRESS
#define GEN_ADDRESS

#define SYS_BASE                100

#define PROTOCOLS               -2
#define REXEC                   -3
#define RMGR                    -4
#define ISIS                    -5
#define NEWS                    -6
#define XMGR                    -7
#define LMGR                    -8

/* Generic entry points are always the same */
#define GENERIC_RCV_REPLY       (SYS_BASE+0)	/* For receiving a reply */
#define GENERIC_NEW_VIEW        (SYS_BASE+1)	/* Message contains a new view */
#define GENERIC_DEL_PGROUP      (SYS_BASE+2)	/* Delete entire sys_groupview */

/* Spy routines trap these requests */
#define GENERIC_ADDMEMB         (SYS_BASE+3)	/* Add a member */
#define GENERIC_ADDCLIENT       (SYS_BASE+4)	/* Add a client */
#define GENERIC_DELETE          (SYS_BASE+5)	/* Delete a member/client */
#define GENERIC_SIGNAL          (SYS_BASE+6)	/* Signal thyself */
#define GENERIC_IGNORE          (SYS_BASE+7)	/* Not really delivered */

/* Other entry points */
#define GENERIC_PROC_FAILED     (SYS_BASE+8)	/* A process you were monitoring has failed */
#define GENERIC_XFER_WHERE      (SYS_BASE+9)	/* Query: how far did xfer get */
#define GENERIC_XFER_REQ        (SYS_BASE+10)	/* Transfer request */
#define GENERIC_XFER_STATE      (SYS_BASE+11)	/* Transfer state */
#define GENERIC_XFER_BLOCK_ACT  (SYS_BASE+12)	/* xfer blocks activities */
#define GENERIC_CC_RESULT       (SYS_BASE+13)	/* Coordinator-cohort result known */
#define GENERIC_JOIN_REQ        (SYS_BASE+14)	/* Request permission to join */
#define GENERIC_TOKEN_REQ       (SYS_BASE+15)	/* Request a token */
#define GENERIC_TOKEN_PASS      (SYS_BASE+16)	/* Pass a token */
#define GENERIC_NEW_SVIEW       (SYS_BASE+17)	/* New sview */
#define GENERIC_RM_UP           (SYS_BASE+18)	/* news about site recovery */
#define GENERIC_RM_NEWS         (SYS_BASE+19)	/* news about group restarts */
#define GENERIC_CONGESTED       (SYS_BASE+20)	/* Protos has become congested */
#define GENERIC_DECONGESTED     (SYS_BASE+21)	/* Protos is decongested */
#define GENERIC_CLIENT_REQ      (SYS_BASE+22)	/* pg_client() */
#define GENERIC_G_EVAL          (SYS_BASE+23)	/* Guarded broadcast */
#define GENERIC_G_CANCEL        (SYS_BASE+24)	/* Cancel guarded broadcast */
#define GENERIC_G_NULL          (SYS_BASE+25)	/* For guard() statement */
#define GENERIC_LOG_FLUSH       (SYS_BASE+26)	/* Receive a log_flush request */
#define GENERIC_NEW_SNAMES      (SYS_BASE+27)	/* Defined new site_names[] array */
#define GENERIC_ENDLOGREPLAY    (SYS_BASE+28)	/* End of log replay processing */
#define GENERIC_WANTDUMP        (SYS_BASE+29)	/* Request for a client dump */
#define GENERIC_CL_FRAGMAN      (SYS_BASE+30)	/* Reassemble fragmented message */
#define GENERIC_BYPASS          (SYS_BASE+31)	/* Receive a message that bypassed protos */
#define GENERIC_BYFLSH          (SYS_BASE+32)	/* Used during bypass flush protocol */
#define GENERIC_BYINACTIVE      (SYS_BASE+34)	/* Garbage collect for a cbcast */
#define GENERIC_X_PHASE         (SYS_BASE+35)	/* Do a prepare, commit or abort. */
#define GENERIC_SP_REPLAY       (SYS_BASE+36)	/* Spooler replay */
#define GENERIC_SP_FORWARD      (SYS_BASE+37)	/* Reply to spool_bcast */
#define GENERIC_ISIS_CONNECT    (SYS_BASE+38)	/* Connect to you */
#define GENERIC_BYWAKEUP        (SYS_BASE+39)	/* BYPASS internal `wakeup' */
#define GENERIC_BYPROBE         (SYS_BASE+40)	/* Is you alive? */
#define GENERIC_BYORDER         (SYS_BASE+41)	/* Abcast order */
#define GENERIC_WATCHING        (SYS_BASE+42)	/* pglookup cache refresh mechanism */
#define GENERIC_META            (SYS_BASE+43)	/* META entry points start here */
/*      XXXXXXX_META ......     (SYS_BASE+52)  */

#define NMETAENTRIES            10

#define MAXENTRIES              (GENERIC_META+NMETAENTRIES)

#ifdef  GENERIC_ENTRIES
char *generic_entries[]
    = {
	"rcv_reply",		/* 0 */
	"new_view",		/* 1 */
	"del_sys_groupview",	/* 2 */
	"addmemb",		/* 3 */
	"addclient",		/* 4 */
	"delete",		/* 5 */
	"signal",		/* 6 */
	"ignore",		/* 7 */
	"proc_failed",		/* 8 */
	"xfer_where",		/* 9 */
	"xfer_req",		/* 10 */
	"xfer_state",		/* 11 */
	"xfer_block_act",	/* 12 */
	"cc_result",		/* 13 */
	"join_req",		/* 14 */
	"token-req",		/* 15 */
	"token-pass",		/* 16 */
	"new-sview",		/* 17 */
	"rm_up",		/* 18 */
	"rm_news",		/* 19 */
	"congested",		/* 20 */
	"decongested",		/* 21 */
	"pg_client",		/* 22 */
	"g_eval",		/* 23 */
	"g_cancel",		/* 24 */
	"g_null",		/* 25 */
	"l_rec_flush",		/* 26 */
	"new_snames",		/* 27 */
	"l_endlogreplay",	/* 28 */
	"wantdump",		/* 29 */
	"asmfragment",		/* 30 */
	"bypass",		/* 31 */
	"byflsh",		/* 32 */
	"bycntl",		/* 33 */
	"byinactive",		/* 34 */
	"x_phase",		/* 35 */
	"spool_replay",		/* 36 */
	"spool_forward",	/* 37 */
	"isis_connect",		/* 38 */
	"bywakeup",		/* 39 */
	"byprobe",		/* 40 */
	"byorder",		/* 41 */
	"watching",		/* 42 */
	"meta-1",		/* 43 */
	"meta-2",		/* 44 */
	"meta-3",		/* 45 */
	"meta-4",		/* 46 */
	"meta-5",		/* 47 */
	"meta-6",		/* 48 */
	"meta-7",		/* 49 */
	"meta-8",		/* 50 */
	"meta-9",		/* 51 */
	"meta-10",		/* 52 */
};
#endif
#endif
