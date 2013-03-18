/*  $RCSfile: pr_cbcast.c,v $ $Revision: 2.21 $ $Date: 90/08/15 09:50:27 $  */
/*
 *      Originally coded by Tommy Joseph, later modified by Ken Birman
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
 */
#include "pr_cbcast.h"

adesc pbuf_adesc = { sizeof(pbuf_item), 0, 16 };

int cb_count;

pr_fbcast(msg)
	message *msg;

{
	return (pr_docbcast(0, msg));
}

pr_cbcast(msg)
	message *msg;

{
	return (pr_docbcast(1, msg));
}

/* Do cbcast/fbcast.  Flag tells which */
pr_docbcast(piggy_flag, msg)
	message *msg;

{
	int sending_proc, msg_id, plist[MAX_PROCS + 1], splist[2];
	int *idp;
	site_id ext_dests[MAX_SITES];
	pbuf_item *pb_item;
	register qnode *node, *pbuf;
	address *sender;

	msg_increfcount(msg);
	if ((idp = (int *) msg_getfield(msg, SYSFLD_PROTID, 1, (int *) 0)) != 0)
		msg_id = *idp;
	else {
		msg_id = GENMSGID;
		msg_insertfield(msg, SYSFLD_PROTID, (char *) &msg_id, FTYPE_LONG, sizeof(int));
	}

	msg_replacefield(msg, SYSFLD_CBID, (char *) &msg_id, FTYPE_LONG, sizeof(int));

#   ifdef CB_DEBUG
	print("pr_cbcast: called, id %x ", msg_id);
	pmsg(msg);
#   endif CB_DEBUG

	sender = msg_getsender(msg);
	sending_proc = sender->addr_process;
	if (pg_readsview(msg))
		shr_gunlock(msg_id, sending_proc);

	splist[0] = sending_proc;
	splist[1] = 0;
	cb_splitdests(msg, plist, ext_dests);

	if (*ext_dests) {
		pb_item = cb_createpbitem(msg_id, msg, ext_dests, piggy_flag);
		cb_addtopbuf(splist, msg_id, pb_item);
		pbuf = pbuf_find(sending_proc, msg_id);
		for (node = pbuf->qu_next; node != pbuf; node = node->qu_next)
			cb_addtopbuf(plist, node->qu_name, (pbuf_item *) node->qu_data);
		cb_deliver(plist, msg_id, msg);
		cb_count = 0;
		if (!piggy_flag || msg_islazy(msg) != ULTRA_LAZY)
			cb_sendpkt(sending_proc, msg_id, (wait_struct *) 0);
	} else
		cb_deliver(plist, msg_id | LOCALFLAG, msg);

	msg_delete(msg);

#   ifdef CB_DEBUG
	print("pr_cbcast: terminated\n");
#   endif CB_DEBUG

	return (0);
}

cb_sendpkt(sending_proc, msg_id, w_struct)
	int sending_proc, msg_id;
	wait_struct *w_struct;

{
	register qnode *rdq, *pb_node, *current_pb_node, *d_node;
	register pbuf_item *current_pb_item;
	qnode *d_next, *pbuf, *pbs_node, *doneq, *pb_next;
	pbuf_item *item;
	bitvec *scope;
	site_id dest_id;
	char rescan, blocked;

	if (++cb_count > 9950) {
		if (cb_count == 10000)
			panic("cbcast: recursion problem!");
		print("cbcast(pid %d, msgid %x, w_struct %x): recursion problem?\n", sending_proc,
		      msg_id, w_struct);
		if (cb_count == 9951)
			pr_dump(-1, "cbcast in a recursion loop?");
	}
	doneq = qu_null();
	blocked = FALSE;
	do {
		rescan = FALSE;
		if (!st_deleteable(msg_id) &&
		    (pbs_node = qu_find(pbufs, sending_proc)) &&
		    (current_pb_node = qu_find((pbuf = pbs_node->qu_queue), msg_id))) {
			current_pb_item = (pbuf_item *) current_pb_node->qu_data;
			for (d_node = current_pb_item->rem_dests->qu_next; !rescan &&
			     d_node != current_pb_item->rem_dests; d_node = d_next) {
				dest_id = d_node->qu_name;
				d_next = d_node->qu_next;
				if (!qu_find(doneq, dest_id) &&
				    !bit(&current_pb_item->sent, SITE_NO(dest_id))) {
					for (pb_node = pbuf->qu_next; !rescan &&
					     pb_node->qu_name != msg_id; pb_node = pb_next) {
						if (pb_node == pbuf)
							panic("cb_sendpkt: %x has disappeared!",
							      msg_id);

						pb_next = pb_node->qu_next;
						item = (pbuf_item *) pb_node->qu_data;
						rdq = item->rem_dests;
						if (item->piggy_flag && rdq->qu_next != rdq) {
							register bitvec *scope;
							wait_struct w_str;

							if (bit(&item->sent, SITE_NO(dest_id))
							    || st_deleteable(pb_node->qu_name))
								continue;
							scope =
							    (bitvec *) msg_getfield(item->msg,
										    SYSFLD_SCOPE, 1,
										    (int *) 0);
							if (scope == (bitvec *) 0
							    || bit(scope, SITE_NO(dest_id)))
								continue;
							W_INIT(w_str);
							cb_sendpkt(sending_proc, pb_node->qu_name,
								   &w_str);
							W_WAIT(w_str);
							blocked = rescan = TRUE;
						}
					}
					if (!rescan)
						qu_add(doneq, dest_id, (char *) 0, nullroutine);
				}
			}
			if (!rescan)
				cb_dosendpkt(sending_proc, msg_id, w_struct);
		}
	}
	while (rescan);
	qu_freeall(doneq);
	return (blocked);

}

cb_dosendpkt(sending_proc, msg_id, w_struct)
	register int sending_proc, msg_id;
	wait_struct *w_struct;

{
	register qnode *pbuf, *d_node, *rdq, *current_pb_node;
	qnode *rdestq, *pbs_node, *next, *pb_node, *pb_next;
	address dest;
	register pbuf_item *current_pb_item, *item;
	site_id dest_id;
	int mid, cb_msg_id, dsize, dlist[MAX_SITES], n_piggy, *piggyids, piggy_size, lazy;
	bitvec *scope;
	int cb_callback();
	message *cb_msg;

	/* Careful! This code assumes that this task will not be swapped */
	/* out after a call to pr_send, so that the pbuf data structure */
	/* remains unchanged.  */

	/* However, the code takes care of the case where cb_callback is */
	/* called immediately when pr_send is called, and this may lead */
	/* to some rem_dest nodes being removed.  */

	if (++cb_count > 9950) {
		if (cb_count == 10000)
			panic("cb_dosendpkt: recursion problem!");
		print("cb_dosendpkt(pid %d, msgid %x, w_struct %x): recursion problem?\n",
		      sending_proc, msg_id, w_struct);
		if (cb_count == 9951)
			pr_dump(-1, "cb_dosendpkt in a recursion loop?");
	}
	if (!st_deleteable(msg_id) &&
	    (pbs_node = qu_find(pbufs, sending_proc)) &&
	    (current_pb_node = qu_find((pbuf = pbs_node->qu_queue), msg_id))) {
		current_pb_item = (pbuf_item *) current_pb_node->qu_data;
		rdestq = current_pb_item->rem_dests;
		if (w_struct)
			lazy = FALSE;
		else
			lazy = msg_islazy(current_pb_item->msg);

		for (d_node = rdestq->qu_next; !st_deleteable(msg_id) &&
		     d_node != rdestq; d_node = next) {
			next = d_node->qu_next;
			dest_id = d_node->qu_name;
			if (!bit(&current_pb_item->sent, SITE_NO(dest_id))) {
				cb_msg = msg_newmsg();
				cb_msg_id = GENMSGID;

#               ifdef CB_DEBUG
				print("cb_dosendpkt: preparing packet %x[%x] to ",
				      cb_msg_id, cb_msg);
				psid(dest_id);
				print("\n");
#               endif CB_DEBUG

				dest = ADDRESS(SITE_NO(dest_id), SITE_INCARN(dest_id),
					       PROTOCOLS, PR_CBRECVPKT);
				msg_setdest(cb_msg, &dest);
				if (lazy)
					msg_makelazy(cb_msg, LAZY_ALWAYS);
				n_piggy = 0;
				pr_allocinit((char **) &piggyids, &piggy_size);
				pb_next = pbuf->qu_next;
				do {
					pb_node = pb_next;
					pb_next = pb_node->qu_next;

					if (pb_node == pbuf)
						panic("cb_dosendpkt: %x has disappeared!", msg_id);

					mid = pb_node->qu_name;
					item = (pbuf_item *) pb_node->qu_data;
					rdq = item->rem_dests;
					if ((item->piggy_flag || mid == msg_id) &&
					    !st_deleteable(mid) && rdq->qu_next != rdq) {
						if (mid != msg_id && (scope =
								      (bitvec *)
								      msg_getfield(item->msg,
										   SYSFLD_SCOPE, 1,
										   (int *)
										   0))
						    && !bit(scope, SITE_NO(dest_id)))
							panic
							    ("cb_dosendpkt: unsuccesful flush (%x-%x)",
							     mid, msg_id);
						pr_assign((char *) &piggyids[n_piggy++],
							  (char *) &mid, sizeof(mid),
							  (char **) &piggyids, &piggy_size);
						if (!bit(&item->sent, SITE_NO(dest_id))) {
							dsize = cb_makelist(dlist, rdq);
							(void) msg_addfield(cb_msg, FLD_MSGID,
									    (char *) &mid,
									    FTYPE_LONG,
									    sizeof(mid));
							(void) msg_addmsg(cb_msg, FLD_MSG,
									  item->msg);
							(void) msg_addfield(cb_msg, FLD_REMDESTS,
									    (char *) dlist,
									    FTYPE_LONG,
									    (dsize +
									     1) * sizeof(*dlist));
							(void) msg_addfield(cb_msg, FLD_RECEIVED,
									    (char *)
									    &item->received,
									    FTYPE_LONG,
									    sizeof(item->received));
							(void) msg_addfield(cb_msg, FLD_PIGGYFLAG,
									    (char *)
									    &item->piggy_flag,
									    FTYPE_SHORT,
									    sizeof(item->received));
							bis(&item->sent, SITE_NO(dest_id));
							cb_piggylist_add(cb_msg_id, mid);
							item->refcount++;
							EVENT(S_CBCOUNT);

#                           ifdef CB_DEBUG
							print("cb_dosendpkt: packed %x ", mid);
							pmsg(item->msg);
#                           endif CB_DEBUG
						} else if (w_struct &&
							   !bit(&item->received, SITE_NO(dest_id)))
						{
							if (item->refcount <= 1)
								pr_dump(DUMP_ALL,
									"cb_dosendpkt: bad %x",
									msg_id);
							w_struct->n_events++;
							if (item->wakeup == (qnode *) 0)
								item->wakeup = qu_null();
							qu_add(item->wakeup, (int) w_struct,
							       (char *) 0, nullroutine);
						}

					}
				}
				while (pb_node->qu_name != msg_id);
				pr_assign((char *) &piggyids[n_piggy++], (char *) 0,
					  sizeof(*piggyids), (char **) &piggyids, &piggy_size);
				msg_addfield(cb_msg, FLD_PIGGYLIST, (char *) piggyids,
					     FTYPE_LONG, n_piggy * sizeof(*piggyids));
				free((char *) piggyids);

				EVENT(S_CBSENT);
				pr_send(cb_msg, (address *) 0, cb_callback,
					(char *) cb_msg_id, (char *) 0);

#               ifdef CB_DEBUG
				print("cb_dosendpkt: sent %x ", cb_msg_id);
				pmsg(cb_msg);
#               endif CB_DEBUG

				msg_delete(cb_msg);
			}
		}
		if (w_struct && !st_deleteable(msg_id) && current_pb_item->refcount > 1) {
			w_struct->n_events++;
			if (current_pb_item->wakeup == (qnode *) 0)
				current_pb_item->wakeup = qu_null();
			qu_add(current_pb_item->wakeup, (int) w_struct, (char *) 0, nullroutine);
		}
	} else
		panic("cb_dosendpkt: Something fishy here!");

#   ifdef CB_DEBUG
	print("cb_dosendpkt: terminated\n");
#   endif CB_DEBUG
}

cb_recvpkt(cb_msg)
	register message *cb_msg;

{
	int *msg_idp, localprocs[MAX_PROCS + 1], *destp, *piggyids;
	site_id rem_dests[MAX_SITES];
	bitvec received;
	short piggy_flag;
	message *msg;
	register int i, j, *id, *iid;
	register qnode *pb_node;
	register pbuf_item *pb_item;

#   ifdef CB_DEBUG
	print("cb_recvpkt: received ");
	pmsg(cb_msg);
#   endif CB_DEBUG

	msg_increfcount(cb_msg);
	piggyids = (int *) msg_getfield(cb_msg, FLD_PIGGYLIST, 1, (int *) 0);
	for (i = 1; (msg_idp = (int *) msg_getfield(cb_msg, FLD_MSGID, i, (int *) 0)); i++) {
		msg = msg_getmsg(cb_msg, FLD_MSG, i);

#       ifdef CB_DEBUG
		print("cb_recvpkt: unpacked message %x ", *msg_idp);
		if (msg_ismsg(msg))
			pmsg(msg);
		else
			panic("bad message");
#       endif CB_DEBUG

		for (destp = (int *) msg_getfield(cb_msg, FLD_REMDESTS, i, (int *) 0),
		     j = 0; *destp; destp++, j++)
			rem_dests[j] = (site_id) * destp;
		rem_dests[j] = 0;
		received = *(bitvec *) msg_getfield(cb_msg, FLD_RECEIVED, i, (int *) 0);
		piggy_flag = *(short *) msg_getfield(cb_msg, FLD_PIGGYFLAG, i, (int *) 0);
		cb_updatepbitem(*msg_idp, msg, rem_dests, received, piggy_flag);
		msg_delete(msg);
	}

	for (id = piggyids; *id; id++)
		if (!st_deleteable(*id) && (pb_node = qu_find(pb_itemlist, *id))) {
			pb_item = (pbuf_item *) pb_node->qu_data;
			msg = pb_item->msg;
			cb_makeplist(localprocs, msg, my_site_no, my_site_incarn);
			iid = piggyids;
			do {
				register qnode *qp;

				if (!st_deleteable(*iid) && (qp = qu_find(pb_itemlist, *iid)))
					cb_addtopbuf(localprocs, *iid, (pbuf_item *) qp->qu_data);
			}
			while (iid++ != id);
			cb_deliver(localprocs, *id, msg);
		}
	msg_delete(cb_msg);

#   ifdef CB_DEBUG
	print("cb_recvpkt: terminated\n");
#   endif CB_DEBUG
}

cb_deliver(plist, msg_id, msg)
	register *plist, msg_id;
	register message *msg;

{
	int pplist[MAX_PROCS + 1];
	register int *pr, *ppr;
	register qnode *pb_node, *rdq, *rd_node;
	register pbuf_item *pb_item;

#   ifdef CB_DEBUG
	print("cb_deliver: delivering to");
	dump_list(plist);
	print(" * id %x ", msg_id);
	pmsg(msg);
#   endif CB_DEBUG

	EVENT(S_CBDELIV);
	for (pr = plist, ppr = pplist; *pr; pr++)
		if (!idlist_find(*pr, msg_id) && !pr_waitq_add(*pr, msg_id, msg)) {
			idlist_add(*pr, msg_id);
			*(ppr++) = *pr;
		}
	*ppr = 0;

	if (*pplist)
		pr_local_delivery(msg, pplist);
	if (pb_node = qu_find(pb_itemlist, msg_id)) {
		pb_item = (pbuf_item *) pb_node->qu_data;
		bis(&pb_item->sent, my_site_no);
		bis(&pb_item->received, my_site_no);
		rdq = pb_item->rem_dests;
		if (rd_node = qu_find(rdq, (int) my_site_id))
			qu_free(rd_node);
		if (rdq->qu_next == rdq)
			(void) cb_free(msg_id);
	} else if ((msg_id & LOCALFLAG) == 0)
		panic("cb_deliver: cannot find id %x in pb_itemlist", msg_id);
}

pbuf_item *
cb_createpbitem(msg_id, msg, rem_dests, piggy_flag)
	int msg_id;
	message *msg;
	site_id *rem_dests;
	short piggy_flag;

{
	register qnode *rdestq;
	register pbuf_item *pb_item;
	register site_id *id;
	bitvec *sp;

	/* WARNING: This routine must be called only if !st_deleteable(msg_id) */
	/* otherwise the message will be added to the pbuf twice */

	if (qu_find(pb_itemlist, msg_id) == 0) {
		rdestq = qu_null();
		for (id = rem_dests; *id; id++)
			(void) qu_add(rdestq, *id, (char *) 0, nullroutine);

		(void) st_add(msg_id, QU_CBCLEANUP, (char *) msg_id, cb_cleanup);
		if (sp = (bitvec *) msg_getfield(msg, SYSFLD_SCOPE, 1, (int *) 0))
			st_add_bitvec(msg_id, AS_SCOPE, *sp);

		msg_increfcount(msg);
		pb_item = pbuf_alloc();
		pb_item->msg = msg;
		pb_item->rem_dests = rdestq;
		bclr(&pb_item->sent);
		bclr(&pb_item->received);
		pb_item->piggy_flag = piggy_flag;
		pb_item->refcount = 1;
		pb_item->wakeup = (qnode *) 0;
		(void) qu_add(pb_itemlist, msg_id, (char *) pb_item, nullroutine);

#       ifdef CB_DEBUG
		print("cb_createpbitem: created new item");
		dump_pbitem(pb_item);
		print("\n");
#       endif CB_DEBUG

		return (pb_item);
	} else
		panic("cp_createpbitem: item for id %x already exists!", msg_id);
	return ((pbuf_item *) 0);
}

cb_addtopbuf(plist, msg_id, pb_item)
	register *plist, msg_id;
	register pbuf_item *pb_item;

{
	register int *i;
	register qnode *pbuf;

	/* WARNING: This routine must be called only if !st_deleteable(msg_id) */
	/* otherwise the message will be added to the pbuf twice */

	for (i = plist; *i; i++) {
		pbuf = pbuf_find(*i, msg_id);
		if (!qu_find(pbuf, msg_id))
			(void) qu_add(pbuf, msg_id, (char *) pb_item, nullroutine);
	}
}

cb_updatepbitem(msg_id, msg, rem_dests, received, piggy_flag)
	register msg_id;
	message *msg;
	site_id *rem_dests;
	bitvec received;
	short piggy_flag;

{
	register qnode *rdestq, *pb_node, *rd_node, *next;
	register pbuf_item *pb_item;
	register site_id *rd;
	bitvec *sp;

	if (!st_deleteable(msg_id)) {
		if ((pb_node = qu_find(pb_itemlist, msg_id)) == 0) {
			if (!*rem_dests) {
				if (sp = (bitvec *) msg_getfield(msg, SYSFLD_SCOPE, 1, (int *) 0))
					st_add_bitvec(msg_id, AS_SCOPE, *sp);
				(void) cb_free(msg_id);
				return;
			}
			pb_item = cb_createpbitem(msg_id, msg, rem_dests, piggy_flag);
		} else {
			EVENT(S_CBDUP);
			pb_item = (pbuf_item *) pb_node->qu_data;
			rdestq = pb_item->rem_dests;
			for (rd_node = rdestq->qu_next; rd_node != rdestq; rd_node = next) {
				next = rd_node->qu_next;
				for (rd = rem_dests; *rd && *rd != rd_node->qu_name; rd++) ;
				if (!*rd)
					qu_free(rd_node);
				if (rdestq->qu_next == rdestq) {
					(void) cb_free(msg_id);
					return;
				}
			}
		}
		bisv(&pb_item->sent, &received);
		bisv(&pb_item->received, &received);
		pb_item->piggy_flag = piggy_flag;
#       ifdef CB_DEBUG
		{
			print("cb_updatepbitem: item");
			dump_pbitem(pb_item);
			print("\n");
		}
#       endif CB_DEBUG
	} else
		EVENT(S_CBDUP);

}

cb_callback(site, cb_msg_id)
	site_id site;
	int cb_msg_id;

{
	register qnode *node, *piglist, *pl_node, *rdq, *qp, *pb_node;
	register pbuf_item *pb_item;

	if ((node = qu_find(piggylists, cb_msg_id)) == 0)
		panic("cb_callback: I don't know anything about cb_message %x", cb_msg_id);
	else {
		piglist = node->qu_queue;
		while (pl_node = qu_head(piglist)) {
			if (pb_node = qu_find(pb_itemlist, pl_node->qu_name)) {
				pb_item = (pbuf_item *) pb_node->qu_data;
				bis(&pb_item->received, SITE_NO(site));
				rdq = pb_item->rem_dests;
				if ((qp = qu_find(rdq, (int) site)) != 0)
					qu_free(qp);
				if (--pb_item->refcount == 1) {
					if (pb_item->wakeup) {
						while (qp = qu_head(pb_item->wakeup)) {
							register wait_struct *w_struct;

							w_struct = (wait_struct *) qp->qu_name;
							if (--w_struct->n_events == 0)
								t_sig(&(w_struct->cond), 0);
							qu_free(qp);
						}
						qu_free(pb_item->wakeup);
						pb_item->wakeup = 0;
					}
					if (rdq->qu_next == rdq)
						(void) cb_free(pl_node->qu_name);
				}
			} else if (!st_deleteable(pl_node->qu_name))
				print("cb_callback: WARNING! %x has disappeared!\n",
				      pl_node->qu_name);
			qu_free(pl_node);
		}
		qu_free(node);
	}
}

cb_splitdests(msg, localprocs, ext_dests)
	message *msg;
	int *localprocs;
	site_id *ext_dests;

{
	address *dests;
	site_id dest_id;
	int n_procs, n_dests;
	register int i, j;

	dests = msg_getdests(msg);
	for (i = 0, n_procs = 0, n_dests = 0; dests[i].addr_site != 0; i++) {
		if (dests[i].addr_site == my_site_no && dests[i].addr_incarn == my_site_incarn) {
			for (j = 0; j < n_procs && localprocs[j] != dests[i].addr_process; j++) ;
			if (j == n_procs)
				localprocs[n_procs++] = dests[i].addr_process;
		} else {
			dest_id = MAKE_SITE_ID(dests[i].addr_site, dests[i].addr_incarn);
			for (j = 0; j < n_dests && ext_dests[j] != dest_id; j++) ;
			if (j == n_dests)
				ext_dests[n_dests++] = dest_id;
		}
	}
	localprocs[n_procs] = 0;
	ext_dests[n_dests] = 0;
}

cb_makeplist(plist, msg, site_no, site_incarn)
	int *plist, site_no, site_incarn;
	message *msg;

{
	address *dests;
	int n_procs;
	register int i, j;

	dests = msg_getdests(msg);
	for (i = 0, n_procs = 0; !aptr_isnull(&dests[i]); i++)
		if (dests[i].addr_site == site_no && dests[i].addr_incarn == site_incarn) {
			for (j = 0; j < n_procs && plist[j] != dests[i].addr_process; j++) ;
			if (j == n_procs)
				plist[n_procs++] = dests[i].addr_process;
		}
	plist[n_procs] = 0;

	return (n_procs);
}

cb_makelist(list, q)
	int *list;
	qnode *q;

{
	qnode *node;
	int lsize;

	for (node = q->qu_next, lsize = 0; node != q; node = node->qu_next)
		list[lsize++] = node->qu_name;
	list[lsize] = 0;

	return (lsize);
}

cb_cleanup(msg_id)
	int msg_id;

{
	register qnode *que, *next, *parent, *node;
	pbuf_item *pb_item;

#   ifdef CB_DEBUG
	print("cb_cleanup: %x\n", msg_id);
#   endif CB_DEBUG
	for (parent = pbufs->qu_next; parent != pbufs; parent = next) {
		next = parent->qu_next;

		que = parent->qu_queue;
		if (node = qu_find(que, msg_id)) {
			qu_free(node);
			if (que->qu_next == que)
				qu_free(parent);
		}
	}

	if (node = qu_find(pb_itemlist, msg_id)) {
		pb_item = (pbuf_item *) node->qu_data;
		msg_delete(pb_item->msg);
		qu_freeall(pb_item->rem_dests);
		if (pb_item->wakeup) {
			while (que = qu_head(pb_item->wakeup)) {
				register wait_struct *w_struct;

				w_struct = (wait_struct *) que->qu_name;
				if (--w_struct->n_events == 0)
					t_sig(&(w_struct->cond), 0);
				qu_free(que);
			}
			qu_free(pb_item->wakeup);
			pb_item->wakeup = 0;
		}
		pbuf_free(pb_item);
		qu_free(node);
	}

	for (parent = wait_queues->qu_next; parent != wait_queues; parent = parent->qu_next) {
		que = parent->qu_queue;
		if (node = qu_find(que, msg_id))
			(node->qu_witem).deleted = TRUE;
	}
}

#define	FEW	100

/* Removes id from idlists */
cb_idlisted(qp)
	register qnode *qp;

{
	register qnode *que, *parent;
	qnode *next, *nqp;
	register count = 1, *lp, *list, *overflow, *op;
	int len, few[FEW * 2];

	for (que = qp->qu_next; que != qp; que = que->qu_next)
		++count;
	len = count * sizeof(int);
	if (count > FEW)
		list = (int *) malloc(len << 1);
	else
		list = few;
	bzero(list, len);
	op = overflow = &list[count];
	while (que = qu_head(qp)) {
		lp = &list[que->qu_name % count];
		if (*lp)
			*op++ = que->qu_name;
		else
			*lp = que->qu_name;
		qu_free(que);
	}
	*op = 0;
#   ifdef CB_DEBUG
	print("cb_idlist cleanup: ");
	for (lp = list; *lp; lp++)
		print("%x ", *lp);
	print("\n");
#   endif CB_DEBUG

	for (parent = idlists->qu_next; parent != idlists; parent = next) {
		que = parent->qu_queue;
		for (qp = que->qu_next; qp != que; qp = nqp) {
			nqp = qp->qu_next;
			lp = &list[qp->qu_name % count];
			if (*lp == 0)
				continue;
			if (*lp == qp->qu_name)
				goto found;
			for (op = overflow; *op; op++)
				if (*op == qp->qu_name)
					goto found;
			continue;

		      found:
			qu_free(qp);
		}
		next = parent->qu_next;
		if (que->qu_next == que)
			qu_free(parent);
	}
	if (count > FEW)
		free(list);
}

cb_procflush(proc)
	int proc;

{
	register qnode *pbs_node, *pb_node, *pbuf;
	wait_struct w_struct;
	char blocked;

	cb_count = 0;
	do {
		W_INIT(w_struct);
		blocked = FALSE;
		if (pbs_node = qu_find(pbufs, proc)) {
			pbuf = pbs_node->qu_queue;
			for (pb_node = pbuf->qu_last; !blocked &&
			     pb_node != pbuf; pb_node = pb_node->qu_last)
				blocked = cb_sendpkt(proc, pb_node->qu_name, &w_struct);
		}
	}
	while (blocked);
	W_WAIT(w_struct);
}

cb_siteflush(site)
	site_id site;

{
	site_id source_id;
	register qnode *pbuf, *pb_node, *pbs_node;
	message *msg;
	address *source;
	int mid, proc;
	wait_struct w_struct;
	char blocked;

	cb_count = 0;
	do {
		W_INIT(w_struct);
		blocked = FALSE;
		for (pbs_node = pbufs->qu_next; !blocked && pbs_node != pbufs;
		     pbs_node = pbs_node->qu_next) {
			proc = pbs_node->qu_name;
			pbuf = pbs_node->qu_queue;
			for (pb_node = pbuf->qu_last; !blocked && pb_node != pbuf;
			     pb_node = pb_node->qu_last) {
				mid = pb_node->qu_name;
				msg = ((pbuf_item *) pb_node->qu_data)->msg;
				source = msg_getsender(msg);
				source_id = MAKE_SITE_ID(source->addr_site, source->addr_incarn);
				if (source_id == site)
					blocked = cb_sendpkt(proc, mid, &w_struct);
			}
		}
	}
	while (blocked);
	W_WAIT(w_struct);
}

cb_flushall()
{
	register qnode *pbs_node, *pb_node, *pbuf;
	int proc;
	wait_struct w_struct;
	char blocked;

	cb_count = 0;
	do {
		W_INIT(w_struct);
		blocked = FALSE;
		for (pbs_node = pbufs->qu_next; !blocked && pbs_node != pbufs;
		     pbs_node = pbs_node->qu_next) {
			proc = pbs_node->qu_name;
			pbuf = pbs_node->qu_queue;
			for (pb_node = pbuf->qu_last; !blocked && pb_node != pbuf;
			     pb_node = pb_node->qu_last)
				blocked = cb_sendpkt(proc, pb_node->qu_name, &w_struct);
		}
	}
	while (blocked);
	W_WAIT(w_struct);
}

qnode *
pbuf_find(pid, msg_id)
	register pid, msg_id;

{
	register qnode *node, *pbuf;

	if ((node = qu_find(pbufs, pid)) == 0) {
		pbuf = qu_null();
		node = qu_add_qu(pbufs, pid, pbuf);
		node->qu_msgid = msg_id;
	} else if (node->qu_msgid < msg_id)
		node->qu_msgid = msg_id;
	return (node->qu_queue);
}

idlist_add(pid, msg_id)
	int pid, msg_id;

{
	register qnode *node, *list;

	if (msg_id & LOCALFLAG)
		return;
	if ((node = qu_find(idlists, pid)) == 0) {
		list = qu_null();
		node = qu_add_qu(idlists, pid, list);
	}
	list = node->qu_queue;
	qu_add(list, msg_id, (char *) 0, nullroutine);
	st_on_cb_idlist(msg_id);

}

cb_piggylist_add(cb_msg_id, msg_id)
	int cb_msg_id, msg_id;

{
	register qnode *node, *list;

	if ((node = qu_find(piggylists, cb_msg_id)) == 0) {
		list = qu_null();
		node = qu_add_qu(piggylists, cb_msg_id, list);
	}
	list = node->qu_queue;
	qu_add(list, msg_id, (char *) 0, nullroutine);
}

idlist_find(pid, msg_id)
	int pid, msg_id;

{
	qnode *node;

	if (node = qu_find(idlists, pid))
		if (qu_find(node->qu_queue, msg_id))
			return (TRUE);

	return (FALSE);
}

pbuf_free(item)
	pbuf_item *item;

{
	mdeallocate((char *) item, &pbuf_adesc);
}

msg_islazy(msg)
	register message *msg;

{
	register long *modep;
	register address *dp;

	if ((modep = (long *) msg_getfield(msg, SYSFLD_LAZY, 1, (int *) 0)) == 0)
		return (NOT_LAZY);
	if (*modep == ULTRA_LAZY)
		return (ULTRA_LAZY);
	if (*modep == LAZY_ALWAYS)
		return (LAZY_ALWAYS);

	dp = msg_getdests(msg);
	while (dp && !aptr_isnull(dp)) {
		if (dp->addr_site == my_site_no && dp->addr_incarn == my_site_incarn)
			return (LAZY_IFLOCALDEST);
		dp++;
	}
	return (NOT_LAZY);
}

cb_init()
{
	pbufs = qu_null();
	pb_itemlist = qu_null();
	idlists = qu_null();
	piggylists = qu_null();
}

pb_checkempty(pid)
{
	if (qu_find(pbufs, pid))
		return (-1);
	return (0);
}
