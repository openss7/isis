/*  $RCSfile: pr_intersite.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:48 $  */
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
/* Intersite information, added to message */
struct intersite {
	site_id is_from;		/* Sender */
	site_id is_dest;		/* Dest */
	short is_seqn;			/* Seqn for this packet */
	short is_aseqn;			/* Seqn acknowledged */
	short is_viewid;		/* Sender's view id */
	short is_xxxxxx;		/* Unused, inserted to ensure padding */
	long is_abits;			/* Ack bits */
	long is_abprio;			/* ABCAST priority maintained as logical clock */
};
