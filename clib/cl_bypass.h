/*  $RCSfile: cl_bypass.h,v $ $Revision: 2.14 $ $Date: 90/06/24 14:49:32 $  */
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

#if ! ( __cplusplus || c_plusplus )
typedef struct by_info by_info;
#endif

struct bc_node {
	int bc_viewid;
	int bc_seqn;
	int bc_ndests;
	int bc_flag;
	ginfo *bc_gip;
	qnode *bc_queue;
};

struct by_info {
	address by_group;		/* Group sent to */
	address by_dest;		/* Dest, may be single process */
	int by_viewid;			/* Viewid */
	int by_myseqn;			/* Senders seqn */
	int by_protocol;		/* Protocol used */
	int by_bseq[PG_ALEN];		/* Group sequence number vector */
};

#define byi_size(nmembs)	(sizeof(by_info)-PG_ALEN*sizeof(int)+nmembs*sizeof(int))

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif					/* __cplusplus */
/*** External routines ***/
	int isis_transport(char *, ifunc *, ifunc *, ifunc *);
	int transport_lookup(char *);
/*** Internal routines ***/
	void by_dump();
	void bypass_checkview(message * mp);
	void by_flushfirst(message * mp);
	void bypass_del_pgroup(ginfo * gip);
	void bypass_flush(message * mp);
	void by_flush();
	void bypass_inactive(message * mp);
	void bypass_init();
	int bypass_piggyback();
	void bypass_precheck(message * mp, int entry);
	void bypass_recv(by_info * byaddr, message * mp, int pn);
	message *bypass_send(int protocol, int exmode, address * dests,
			     message * msg, int nwant, int flag, int pn);
	void bypass_unblock(ginfo * gip);
	void pbuf_dirty();
	int isis_querydead(address * who);
#ifdef __cplusplus
}
#endif					/* __cplusplus */
#else				/* FUN_TYPES */
int isis_transport();
int transport_lookup();
void bypass_checkview();
void by_flushfirst();
void bypass_flush(), bypass_inactive(), bypass_unblock();
#endif				/* FUN_TYPES */

#define  MAY_REPLY(mp)   (msg_getid(mp)&1)
