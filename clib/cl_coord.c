/*  $RCSfile: cl_coord.c,v $ $Revision: 2.3 $ $Date: 90/06/11 10:13:01 $  */
/*
 *	Originally coded by Ken Birman
 *      ISIS distributed systems toolkit: coordinator-cohort algorithm
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

#include "isis.h"

qnode *cc_watching();
static qnode *cc_list, *res_list;

void
coord_init()
{
	static initflag;

	if (!initflag) {
		++initflag;
		cc_list = qu_null();
		res_list = qu_null();
		isis_entry(GENERIC_CC_RESULT, (vfunc *) cc_result, "coord_cohort:cc_result");
		isis_task((vfunc *) cohort, "coord_cohort:cohort");
	}
}

int
coord_cohort(msg, gaddr, action, got_result, arg)
	message *msg;
	address *gaddr;
	vfunc *action, *got_result;
	VOID *arg;
{
	extern address *coordinator();

	return (coord_cohort_l(msg, gaddr, action, got_result, arg, coordinator));
}

int
coord_cohort_l(msg, gaddr, action, got_result, arg, croutine)
	message *msg;
	address *gaddr, *(*croutine) ();
	vfunc *action, *got_result;
	VOID *arg;
{
	register message *rmsg;
	address coord, *players;
	register address *clist, *ap, *cp;
	int how, wid;
	register groupview *gv;

	ISIS_ENTER();
	begin {
		/* Make a copy for safekeeping */
		register len;

		if (msg) {
			ap = msg_getdests(msg);
			len = sizeof(address) * (4 + alist_len(ap));
			cp = clist = (address *) malloc(len);
			*cp = *msg_getreplyto(msg);
			(cp++)->addr_entry = GENERIC_RCV_REPLY;
			players = cp;
		} else {
			gv = pg_getlocalview(gaddr);
			if (gv == (groupview *) 0)
				ISIS_RETURN(-1);
			ap = gv->gv_members;
			len = sizeof(address) * (4 + alist_len(ap));
			players = cp = clist = (address *) malloc(len);
		}
		while (!aptr_isnull(ap)) {
			*cp = *ap++;
			(cp++)->addr_entry = GENERIC_CC_RESULT;
		}
		*cp = NULLADDRESS;
	}
	how = ORIGINAL;
	do {
		address *sender, *coord_p;
		int msgid = -1;

		if (msg)
			sender = msg_getsender(msg);
		else
			sender = &gv->gv_members[0];
		if (msg)
			msgid = msg_getid(msg);
		coord_p = (*croutine) (gaddr, sender, players, arg);
		coord = *coord_p;	/* Needed to avoid compiler error. */
		coord.addr_entry = 0;
		if (addr_isnull(&coord)) {
			/* left group just when cc was running */
			if (msg)
				nullreply(msg);
			ISIS_RETURN(0);
		}
		if (addr_ismine(&coord)) {
			for (cp = ap = clist; !aptr_isnull(cp); cp++)
				if (!addr_ismine(cp) || cp->addr_entry != GENERIC_CC_RESULT)
					*ap++ = *cp;
			*ap = NULLADDRESS;
			isis_ctp->task_cohorts = clist;
			isis_ctp->task_ccmsgid = msgid;
			isis_ctp->task_truesender = *sender;
			if (isis_state & ISIS_LEAVING)
				cc_refuse();
			else {
				static coord_active;

				++coord_active;
				isis_state |= ISIS_COORD;
				ISISCALL4(action, msg, gaddr, how, arg);
				isis_ctp->task_cohorts = 0;
				if (--coord_active == 0) {
					isis_state &= ~ISIS_COORD;
					t_sig_all(&isis_wantleave, 0);
				}
			}
			free(clist);
			ISIS_RETURN(0);
		} else {
			/* Save information under <sender,msgid> */
			register qnode *wp, *qp;

			wp = cc_watching(sender, msgid, &coord);
			how = TAKEOVER;
			rmsg = NULLMP;
			if ((wid = pg_watch(gaddr, &coord, W_LEAVE, (vfunc *) cohort,
					    (void *) &wp->qu_cond)) > 0) {
			      wait_again:
				rmsg =
				    (message *) t_wait_l(&wp->qu_cond,
							 "isis system: cohort waiting for coord termination message");
				pg_watch_cancel(wid);
				if (rmsg && msg_getfield(rmsg, SYSFLD_CCREFUSED, 1, (int *) 0)) {
					register address *ap, *refused = msg_getsender(rmsg);

					for (ap = players; !aptr_isnull(ap); ap++)
						if (addr_isequal(ap, refused)) {
							ap->addr_incarn = ILLEGAL_INCARN;
							break;
						}
					msg_delete(rmsg);
					rmsg = NULLMP;
					if (!addr_isequal(refused, &coord)) {
						qu_free(wp);
						wp = cc_watching(sender, msgid, &coord);
						goto wait_again;
					}
				}
			}
			qu_free(wp);
			/* Now clean up qnodes */
			for (wp = cc_list->qu_next; wp != cc_list; wp = qp) {
				qp = wp->qu_next;
				if (qu_head(wp->qu_queue) == 0)
					qu_free(wp);
			}
		}
	}
	while (rmsg == NULLMP);
	if (rmsg) {
		if (got_result)
			ISISCALL1(got_result, rmsg);
		msg_delete(rmsg);
	}
	free(clist);
	ISIS_RETURN(0);
}

/*
 * Lookup session info, which is stored under <sender,msgid>.  Sender will
 * be the first listed address.  Might not find result, but this is no big deal.
 * It occurs when proceses join a group, in which case a new member might  
 * not have been a participant in a previously started coordinator-cohort computation.
 */
void
cc_result(msg)
	register message *msg;
{
	register qnode *qp, *wp;
	register msgid;
	address sender;

	cc_gotres(0);
	msgid = msg_getid(msg);
	sender = *(address *) msg_getfield(msg, FLD_TRUESENDER, 1, NULLIARG);
	if ((qp = pg_find(cc_list, &sender)) == 0)
		goto not_fnd;
	if ((wp = qu_find(qp->qu_queue, msgid)) == 0)
		goto not_fnd;
	if (wp->qu_cond == 0)
		goto not_fnd;
	qu_remove(wp);
	msg_increfcount(msg);
	cc_gotres(1);
	t_sig(&wp->qu_cond, (char *) msg);
	return;
      not_fnd:
	msg_increfcount(msg);
	if ((qp = pg_find(res_list, &sender)) == 0)
		qp = pg_add_qu(res_list, &sender, qu_null());
	qu_add_mp(qp->qu_queue, msgid, msg, (vfunc *) MSG_DELETE);
}

void
cc_panic(msg, who, msgid, why)
	message *msg;
	address *who;
	int msgid;
	char *why;
{
	register qnode *qp, *wp;

	print("%d: cc_panic: %s, sender ", my_process_id, why);
	paddr(who);
	pmsg(msg);
	print(", cc_queue:");
	for (qp = cc_list->qu_next; qp != cc_list; qp = qp->qu_next) {
		print("\n  CC ");
		paddr(&qp->qu_pname);
		for (wp = qp->qu_queue->qu_next; wp != qp->qu_queue; wp = wp->qu_next)
			print(" = msgid %d... ", wp->qu_name);
	}
	print("\n");
	exit(0);
}

/* Figure out who the coordinator should be */
address *
coordinator(gaddr, senderp, players, arg)
	register address *players;
	address *senderp, *gaddr;
	VOID *arg;			/* PFFS */
{
	register groupview *gv = pg_getlocalview(gaddr);
	register address *ap, *bp;
	address sender;
	register alen;

	if (gv == 0)
		return (&NULLADDRESS);
	sender = *senderp;
	sender.addr_entry = 0;
	for (ap = players; !aptr_isnull(ap); ap++)
		if (addr_ismine(ap))
			break;
	if (aptr_isnull(ap)) {
		print("I am ");
		paddr(&my_address);
		print(" players ");
		paddrs(players);
		print("\n");
		panic("coordinator computation: didn't find self in player list");
	}
	for (ap = players; !aptr_isnull(ap); ap++)
		if (addr_cmp(ap, &sender) == 0)
			return (ap);
	alen = ap - players;
	for (ap = players; !aptr_isnull(ap); ap++)
		if (ap->addr_site == sender.addr_site)
			break;
	if (ap->addr_site == 0)
		/* In this case, share the load */
		ap = &players[sender.addr_site % alen];
	bp = ap;
	do {
		register address *vp;

		/* See if this guy is up */
		for (vp = gv->gv_members; !aptr_isnull(vp); vp++)
			if (addr_cmp(vp, ap) == 0)
				return (vp);
		/* Nope, move on to the next one */
		if ((++ap)->addr_site == 0)
			ap = players;
	}
	while (ap != bp);
	return (&NULLADDRESS);
}

/* Watch goes off: pass a null message pointer to the waiting task */
void
cohort(gaddr, paddr, event, cond)
	address *gaddr, *paddr;
	int event;
	register condition *cond;
{
	t_sig(cond, NULLARG);
}

/* Maintain qnode of <session,msgid> = coord */
qnode *
cc_watching(sender, msgid, coord)
	address *sender;
	int msgid;
	address *coord;
{
	register qnode *qp, *wp;

	/* First check to see if we got the result early */
	if (qp = pg_find(res_list, sender))
		for (wp = qp->qu_queue->qu_next; wp != qp->qu_queue; wp = wp->qu_next)
			if (wp->qu_name == msgid) {
				if (!addr_isequal(msg_getsender(wp->qu_msg), coord))
					continue;
				t_fork_msg((vfunc *) cc_result, wp->qu_msg);
				qu_free(wp);
				if (qu_head(qp->qu_queue) == 0)
					qu_free(qp);
				break;
			}
	if ((qp = pg_find(cc_list, sender)) == 0)
		qp = pg_add_qu(cc_list, sender, qu_null());
	if ((wp = qu_find(qp->qu_queue, msgid)) == 0)
		wp = qu_add_cond(qp->qu_queue, msgid);
	else
		panic("cc_watching %d/%d", sender->addr_process, msgid);
	return (wp);
}

/* Sends a reply to cohorts only */
void
cc_terminate(va_alist)
	va_dcl
{
	va_list ap;

	va_start(ap);
	do_ccterminate((address *) 0, &ap);
	va_end(ap);
}

void
cc_terminate_l(va_alist)
	va_dcl
{
	address *dest;
	va_list ap;

	va_start(ap);
	dest = VA_ARG(ap, address *);
	do_ccterminate(dest, &ap);
	va_end(ap);
}

void
cc_terminate_msg(msg)
	register message *msg;
{
	ISIS_ENTER();
	if (isis_ctp->task_cohorts)
		panic("cc_terminate: cohorts unknown or multiple invocation\n");
	msg_getid(msg) = isis_ctp->task_ccmsgid;
	msg_insertfield(msg, FLD_TRUESENDER, (char *) &isis_ctp->task_truesender, FTYPE_ADDRESS,
			sizeof(address));
	BEGINFROMC(void) cbcast_l("lm", isis_ctp->task_cohorts, msg, 0);

	ENDFROMC isis_ctp->task_cohorts = 0;

	ISIS_EXIT();
}

void
do_ccterminate(dest, ap)
	register address *dest;
	va_list *ap;
{
	register message *msg;
	register address *adp;

	ISIS_ENTER();
	msg = msg_newmsg();
	if ((adp = isis_ctp->task_cohorts) == 0)
		panic("cc_terminate: cohorts unknown or multiple invocation\n");
	if (dest) {
		while (!aptr_isnull(adp))
			++adp;
		*adp = *dest;
		(++adp)->addr_site = 0;
	}
	msg_getid(msg) = isis_ctp->task_ccmsgid;
	msg_insertfield(msg, FLD_TRUESENDER, (char *) &isis_ctp->task_truesender, FTYPE_ADDRESS,
			sizeof(address));
	msg_doputf(msg, SYSFLD_SCAN, ap);
	BEGINFROMC(void) cbcast_l("lm", isis_ctp->task_cohorts, msg, 0);

	ENDFROMC msg_delete(msg);

	isis_ctp->task_cohorts = 0;
	ISIS_EXIT();
}

/* Refuse to be coordinator -- nullreply to caller, CCREFUSED to others */
void
cc_refuse()
{
	register message *msg;

	ISIS_ENTER();
	msg = msg_newmsg();
	if (isis_ctp->task_cohorts == 0)
		panic("cc_refuse: cohorts unknown or multiple invocation\n");
	msg_getid(msg) = isis_ctp->task_ccmsgid;
	msg_addfield(msg, FLD_TRUESENDER, (char *) &isis_ctp->task_truesender, FTYPE_ADDRESS,
		     sizeof(address));
	msg_addfield(msg, FLD_ISNULLREP, 0, 0, 0);
	msg_insertfield(msg, SYSFLD_CCREFUSED, 0, 0, 0);
	BEGINFROMC(void) cbcast_l("lm", isis_ctp->task_cohorts, msg, 0);

	ENDFROMC msg_delete(msg);

	isis_ctp->task_cohorts = 0;
	ISIS_EXIT();
}
