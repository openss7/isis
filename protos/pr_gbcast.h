/*  $RCSfile: pr_gbcast.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:27 $  */
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
#ifndef PR_GBCAST
#define PR_GBCAST

#include "pr.h"
#include "pr_abcast.h"
#include "pr_cbcast.h"

struct gb_answer {
	int priority;
	int idlist[1];
};

qnode *wait_queues, *wait1, *upgrade_queue;

int pr_gbcast(), gb_send1(), gb_send2(), gb_recv1(), gb_recv2(), gb_callback(), gb_takeover();

#endif				/* PR_GBCAST */
