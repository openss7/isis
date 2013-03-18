/*  $RCSfile: pr_abcast.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:45 $  */
/*
 *	Originally coded by Tommy Joseph
 *
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
#include "pr_abcast.h"

long ab_priority = NULLPRIO;
adesc abq_adesc = { sizeof(abq_item), 0, 16 };

pr_abcast(msg)
	message *msg;

{
	int msg_id, priority, n_dests, *idp;
	site_id slist[MAX_SITES + 1], *sp;
	address *sender;
	bitvec scope;

	msg_increfcount(msg);
	if ((idp = (int *) msg_getfield(msg, SYSFLD_PROTID, 1, (int *) 0)) != 0)
		msg_id = *idp;
	else {
		msg_id = GENMSGID;
		msg_insertfield(msg, SYSFLD_PROTID, (char *) &msg_id, FTYPE_LONG, sizeof(int));
	}
#   ifdef AB_DEBUG
	print("pr_abcast: called, id %x = ", msg_id);
	pmsg(msg);
	msg_printaccess(msg);
#   endif  AB_DEBUG

	n_dests = ab_makeslist(slist, msg);
	bclr(&scope);
	for (sp = slist; *sp; sp++)
		bis(&scope, SITE_NO(*sp));
	msg_replacefield(msg, SYSFLD_SCOPE, (char *) &scope, FTYPE_BITVEC, sizeof(bitvec));
	if (n_dests == 0 || (n_dests == 1 && *slist == my_site_id))
		msg_id |= LOCALFLAG;
	priority = ab_send1(msg_id, msg, slist);
	if (priority != -1)
		ab_send2(msg_id, priority, slist);
	if (pg_readsview(msg)) {
		sender = msg_getsender(msg);
		shr_gunlock((msg_id & ~LOCALFLAG), sender->addr_process);
	}
	msg_delete(msg);

#   ifdef AB_DEBUG
	print("pr_abcast: terminated\n");
#   endif AB_DEBUG

	return (0);
}

ab_send1(msg_id, msg, slist)
	int msg_id;
	message *msg;
	site_id *slist;

{
	int ab_answ[MAX_SITES];
	message *ab_msg;
	register int i, max, n_replies;

	ab_msg = msg_newmsg();

#   ifdef AB_DEBUG
	print("ab_send1: sending message id %x, msg %x\n", msg_id, ab_msg);
	pmsg(ab_msg);
	msg_printaccess(ab_msg);
#   endif AB_DEBUG

	(void) msg_addfield(ab_msg, FLD_MSGID, (char *) &msg_id, FTYPE_LONG, sizeof(msg_id));
	(void) msg_addmsg(ab_msg, FLD_MSG, msg);
	n_replies = BCAST_SL(slist, PROTOCOLS, PR_ABRECV1, ab_msg, ALL,
			     collect_answ, (char *) ab_answ, sizeof(*ab_answ));
	max = -1;
	if (n_replies != 0)
		for (i = 1, max = ab_answ[0]; i < n_replies; i++)
			if (prio_is_gt(ab_answ[i], max))
				max = ab_answ[i];
	msg_delete(ab_msg);

#   ifdef AB_DEBUG
	print("ab_send1: max. priority for message %x = %x\n", msg_id, max);
#   endif AB_DEBUG

	return (max);
}

ab_send2(msg_id, priority, slist)
	int msg_id, priority;
	site_id *slist;

{
	message *ab_msg;
	register site_id *sp;
	register address *dp;
	address dests[MAX_SITES + 1];

	ab_msg = msg_newmsg();
	(void) msg_addfield(ab_msg, FLD_MSGID, (char *) &msg_id, FTYPE_LONG, sizeof(msg_id));
	(void) msg_addfield(ab_msg, FLD_PRIORITY, (char *) &priority, FTYPE_LONG, sizeof(priority));

#   ifdef AB_DEBUG
	print("ab_send2: sending phase 2 message for %x\n", msg_id);
#   endif AB_DEBUG

	for (sp = slist, dp = dests; *sp; sp++, dp++)
		*dp = ADDRESS(SITE_NO(*sp), SITE_INCARN(*sp), PROTOCOLS, PR_ABRECV2);
	*dp = NULLADDRESS;
	msg_setdests(ab_msg, dests);
	pr_bcast(ab_msg);
	msg_delete(ab_msg);
}

ab_recv1(ab_msg)
	message *ab_msg;

{
	int msg_id, priority, plist[MAX_PROCS + 1];
	register int *proc;
	int watch_off();
	message *msg;
	address *coordinator;
	int watch_id;
	bitvec *sp;

	msg_increfcount(ab_msg);
	msg_id = *(int *) msg_getfield(ab_msg, FLD_MSGID, 1, (int *) 0);

#   ifdef AB_DEBUG
	print("ab_recv1: received message %x, ", msg_id);
	pmsg(ab_msg);
#   endif AB_DEBUG

	if (!(priority = st_find_int(msg_id, QU_PRIORITY))) {
		msg = msg_getmsg(ab_msg, FLD_MSG, 1);
		(void) ab_makeplist(msg, plist);
		if (pg_readsview(msg))
			for (proc = plist; *proc; proc++)
				register_shrglock((msg_id & ~LOCALFLAG), *proc);
		priority = ab_addtoqueues(msg_id, msg, plist, AB_UNDELIVERABLE);

		if (msg_id & LOCALFLAG)
			qu_add(ablocalq, msg_id, (char *) msg, MSG_DELETE);
		else {
			(void) st_add_int(msg_id, QU_PHASE, 1);
			(void) st_add(msg_id, QU_MSG, (char *) msg, MSG_DELETE);
			if (sp = (bitvec *) msg_getfield(msg, SYSFLD_SCOPE, 1, (int *) 0))
				st_add_bitvec(msg_id, AS_SCOPE, *sp);
			(void) st_add_int(msg_id, QU_PRIORITY, priority);
		}

#   ifdef AB_DEBUG
		print("ab_recv1: msg_id = %x, priority = %x, ", msg_id, priority);
		pmsg(msg);
		msg_printaccess(msg);
#   endif AB_DEBUG
	}

	reply(ab_msg, (char *) &priority, FTYPE_LONG, sizeof(priority));

	if ((msg_id & LOCALFLAG) == 0) {
		coordinator = msg_getsender(ab_msg);
		if (coordinator->addr_site != my_site_no) {
			if (watch_id = watch_on((int) coordinator->addr_site,
						(int) coordinator->addr_incarn, ab_takeover,
						(char *) msg_id))
				(void) st_add(msg_id, QU_WATCH, (char *) watch_id, watch_off);
			else
				t_fork(ab_takeover, (char *) msg_id, (message *) 0);
		}
	}
	msg_delete(ab_msg);

#   ifdef AB_DEBUG
	print("ab_recv1: sent reply\n");
	dump_abq();
#   endif AB_DEBUG
}

ab_recv2(ab_msg)
	message *ab_msg;

{
	int msg_id, new_priority, plist[MAX_PROCS + 1];
	int watch_off();
	message *msg;
	address *coordinator;
	qnode *qp;
	int watch_id;
	register int i;

	msg_increfcount(ab_msg);
	msg_id = *(int *) msg_getfield(ab_msg, FLD_MSGID, 1, (int *) 0);

#   ifdef AB_DEBUG
	print("ab_recv2: received message about %x\n", msg_id);
#   endif AB_DEBUG

	if (msg_id & LOCALFLAG)
		if (qp = qu_find(ablocalq, msg_id))
			msg = (message *) qp->qu_data;
		else
			panic("ab_recv2: can't find local message %x", msg_id);
	else if (st_deleteable(msg_id)) {
		msg_delete(ab_msg);
		return;
	} else if ((msg = (message *) st_find(msg_id, QU_MSG)) == 0) {
		print("ab_recv2: WARNING! Can't find message %x\n", msg_id);
		msg_delete(ab_msg);
		return;
	}
#   ifdef AB_DEBUG
	print("ab_recv2: retreived ");
	pmsg(msg);
#   endif AB_DEBUG

	if ((msg_id & LOCALFLAG) || st_find_int(msg_id, QU_PHASE) != 2) {
		new_priority = *(int *) msg_getfield(ab_msg, FLD_PRIORITY, 1, (int *) 0);
#       ifdef AB_DEBUG
		print("ab_recv2: new priority = %x\n", new_priority);
#       endif AB_DEBUG

		if ((msg_id & LOCALFLAG) == 0) {
			(void) st_add_int(msg_id, QU_PHASE, 2);
			(void) st_add_int(msg_id, QU_PRIORITY, new_priority);
		}

		ab_makeplist(msg, plist);
		if (prio_is_gt(new_priority, ab_priority))
			ab_priority = new_priority;
		for (i = 0; plist[i] != 0; i++) {
			abq_changeprops(plist[i], msg_id, new_priority, AB_DELIVERABLE);
			ab_deliver(plist[i]);
		}
	}

	if (msg_id & LOCALFLAG) {
		qu_free(qp);
	} else {
		coordinator = msg_getsender(ab_msg);
		if (coordinator->addr_site != my_site_no) {
			if (watch_id = watch_on((int) coordinator->addr_site,
						(int) coordinator->addr_incarn, ab_takeover,
						(char *) msg_id))
				(void) st_add(msg_id, QU_WATCH, (char *) watch_id, watch_off);
			else
				t_fork(ab_takeover, (char *) msg_id, (message *) 0);
		}
		ab_free(msg_id);
	}
	msg_delete(ab_msg);

#   ifdef AB_DEBUG
	print("ab_recv2: terminated\n");
	dump_abq();
#   endif AB_DEBUG
}

ab_takeover(msg_id)
	int msg_id;

{
	message *msg;
	site_id slist[MAX_SITES + 1];
	int priority, phase;

#   ifdef AB_DEBUG
	print("ab_takeover: taking over for msg %x\n", msg_id);
#   endif AB_DEBUG
	if (!st_deleteable(msg_id) && (msg = (message *) st_find(msg_id, QU_MSG)) != 0) {
		ab_makeslist(slist, msg);
		phase = (int) st_find(msg_id, QU_PHASE);
		if (phase == 1)
			priority = ab_send1(msg_id, msg, slist);
		else
			priority = (int) st_find(msg_id, QU_PRIORITY);
		if (priority != -1)
			ab_send2(msg_id, priority, slist);
	}
}

ab_makeslist(slist, msg)
	site_id *slist;
	message *msg;

{
	address *dests;
	site_id dest;
	int n_dests;
	register int i, j;

	dests = msg_getdests(msg);
	for (i = 0, n_dests = 0; dests[i].addr_site != 0; i++)
		if (SITE_IS_UP(dests[i].addr_site, dests[i].addr_incarn)) {
			dest = MAKE_SITE_ID(dests[i].addr_site, dests[i].addr_incarn);
			for (j = 0; j < n_dests && slist[j] != dest; j++)
				continue;
			if (j == n_dests)
				slist[n_dests++] = dest;
		}
	slist[n_dests] = 0;

	return (n_dests);
}

ab_makeplist(msg, plist)
	register int *plist;
	message *msg;

{
	register int n_procs, i, j;
	register address *dests;

	dests = msg_getdests(msg);
	for (i = 0, n_procs = 0; dests[i].addr_site; i++)
		if (dests[i].addr_site == my_site_no && dests[i].addr_incarn == my_site_incarn) {
			for (j = 0; j < n_procs && plist[j] != dests[i].addr_process; j++)
				continue;
			if (j == n_procs)
				plist[n_procs++] = dests[i].addr_process;
		}
	plist[n_procs] = 0;

	return (n_procs);
}

ab_addtoqueues(msg_id, msg, plist, tag)
	int msg_id;
	message *msg;
	register int *plist;
	char tag;

{
	register int priority, *pid;
	register qnode *msgq, *priorityq, *node;
	register abq_item *item;

	priority = INCR_PRIO(ab_priority);

	for (pid = plist; *pid; pid++) {
		if (!(node = qu_find(abq, *pid))) {
			msgq = qu_null();
			priorityq = qu_null();
			node = qu_add(abq, *pid, (char *) 0, nullroutine);
			node->qu_queues[0] = msgq;
			node->qu_queues[1] = priorityq;
		}

		msgq = node->qu_queues[0];
		priorityq = node->qu_queues[1];
		if (!(node = qu_find(msgq, msg_id))) {
			msg_increfcount(msg);

			item = abq_alloc();
			item->msg_id = msg_id;
			item->msg = msg;
			item->priority = priority;
			item->tag = tag;

			node = qu_add(priorityq, priority, (char *) item, nullroutine);
			ab_resort(priorityq, node);
			qu_add(msgq, msg_id, (char *) item, abq_free);
		}
	}
	return (priority);
}

ab_deliver(pid)
	int pid;

{
	qnode *msgq, *priorityq, *node, *m_node, *p_node;
	int msg_id, plist[2];
	char tag;
	message *msg;
	abq_item *item;

	/* Careful! Must take care of the case where task is swapped */
	/* out in the middle of this routine, and the abq is changed */

	while (node = qu_find(abq, pid)) {
		msgq = node->qu_queues[0];
		priorityq = node->qu_queues[1];

		p_node = qu_head(priorityq);
		item = (abq_item *) p_node->qu_data;
		if (!(item->tag & AB_DELIVERABLE))
			return;

		msg_id = item->msg_id;
		msg = item->msg;
		tag = item->tag;
		m_node = qu_find(msgq, msg_id);
		qu_free(m_node);
		qu_free(p_node);

		if (qu_head(msgq) == 0) {
			qu_free(msgq);
			qu_free(priorityq);
			qu_free(node);
		}
#       ifdef AB_DEBUG
		print("ab_deliver: delivering to pid %d, msg_id = %x, ", pid, msg_id);
		pmsg(msg);
#       endif
		plist[0] = pid;
		plist[1] = 0;
		pr_local_delivery(msg, plist);
		if (tag & AB_GBCAST)
			pr_waitq_remove(pid);
		if (pg_readsview(msg))
			shr_gunlock((msg_id & ~LOCALFLAG), pid);
		msg_delete(msg);
	}
}

abq_free(item)
	abq_item *item;

{
	mdeallocate((char *) item, &abq_adesc);
}

abq_item *
abq_find(pid, msg_id)
	int pid, msg_id;

{
	register qnode *node;

	if ((node = qu_find(abq, pid)) && (node = qu_find(node->qu_queues[0], msg_id)))
		return ((abq_item *) node->qu_data);
	return ((abq_item *) 0);
}

abq_remove(pid, msg_id)
	int pid, msg_id;

{
	qnode *msgq, *priorityq, *node, *m_node, *p_node;
	int priority;
	abq_item *item;
	message *msg;

	if (node = qu_find(abq, pid)) {
		msgq = node->qu_queues[0];
		priorityq = node->qu_queues[1];

		if (m_node = qu_find(msgq, msg_id)) {
			item = (abq_item *) m_node->qu_data;
			msg = item->msg;
			priority = item->priority;
			p_node = qu_find(priorityq, priority);
			qu_free(m_node);
			qu_free(p_node);
			if (qu_head(msgq) == 0) {
				qu_free(msgq);
				qu_free(priorityq);
				qu_free(node);
			}
			msg_delete(msg);
			ab_deliver(pid);
		}
	}
}

abq_changeprops(pid, msg_id, priority, tag)
	int pid, msg_id, priority;
	char tag;

{
	register abq_item *item;
	abq_item *abq_find();
	register qnode *priorityq, *pq_node;

	if (!(item = abq_find(pid, msg_id)))
		return (-1);

	priorityq = qu_find(abq, pid)->qu_queues[1];
	pq_node = qu_find(priorityq, item->priority);

	item->priority = priority;
	item->tag = tag;

	if (pq_node->qu_name != priority) {
		pq_node->qu_name = priority;
		ab_resort(priorityq, pq_node);
	}

	return (0);
}

/* priority comparison rules: true if first > second */
prio_is_gt(first, second)
	register first, second;

{
	register long fdiff;
	register long bdiff;

	if (first == NULLPRIO)
		return (FALSE);
	if (second == NULLPRIO)
		return (TRUE);
	fdiff = first - second;
	bdiff = -fdiff;
	if (fdiff < 0)
		fdiff += 0x7fffffff;
	if (bdiff < 0)
		bdiff += 0x7fffffff;
	if (fdiff < bdiff)
		return (TRUE);
	return (FALSE);
}

/* Resort qnode qp when name of node np has changed */
ab_resort(qp, np)
	register qnode *qp, *np;

{
	register qnode *op;

	qu_remove(np);
	op = qp->qu_next;
	while (op != qp) {
		if (prio_is_gt(op->qu_name, np->qu_name))
			break;
		op = op->qu_next;
	}
	op->qu_last->qu_next = np;
	np->qu_last = op->qu_last;
	op->qu_last = np;
	np->qu_next = op;
}

ab_init()
{
	abq = qu_null();
	ablocalq = qu_null();
}
