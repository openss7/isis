/*  $RCSfile: pr_astore.c,v $ $Revision: 2.21 $ $Date: 90/08/15 09:50:17 $  */
/*
 *      Associative storage data structure and management routines
 *
 *	Originally coded by Ken Birman
 *	Some changes and extensions by Tommy Joseph
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

#include "pr.h"

#define  TIMEOUT_SEC    180	/* Sweep every 3 minutes */

#define  sec            1000

qnode *cb_ids;

cb_idfree(id)
{
	enqueue_cbid(id);
	flush_cbids();
}

enqueue_cbid(id)
{
	qu_add(cb_ids, id, 0, nullroutine);
}

flush_cbids()
{
	if (qu_head(cb_ids))
		cb_idlisted(cb_ids);
}

static st_idlist st_ids[AS_MCOLLECT];

/* Add tuple <fname,value,routine> to this id */
qnode *
st_add(id, fname, data, routine)
	char *data;
	int (*routine) ();
{
	register qnode *qp, *np;

	if ((qp = qu_find(as_root, id)) == 0) {
		qp = qu_add_qu(as_root, id, qu_null());
		qp->qu_flag = AS_LOCAL;
		qp->qu_queue->qu_flag = 0;
#ifdef      AS_DEBUG
		print("create astore node for %x\n", id);
#endif				/* AS_DEBUG */
	}
	if (np = qu_find(qp->qu_queue, fname))
		qu_free(np);
	qp = qu_add(qp->qu_queue, fname, data, routine);
	return (qp);
}

st_on_cb_idlist(id)
	register id;
{
	register qnode *qp;

	if ((qp = qu_find(as_root, id)) == 0)
		timeout(2000, cb_idfree, id);
	else
		qp->qu_flag |= AS_IDLISTED;
}

char *
st_find(id, fname)
{
	register qnode *qp, *np;

	if ((qp = qu_find(as_root, id)) && (np = qu_find(qp->qu_queue, fname)))
		return (np->qu_data);
	return ((char *) 0);
}

st_abort(id)
{
	register qnode *qp;

	if ((qp = qu_find(as_root, id)) && (qp->qu_flag & AS_DELETE) == 0) {
		if (qp->qu_flag & AS_IDLISTED) {
			enqueue_cbid(id);
			flush_cbids();
		}
		qu_free(qp);
	}
}

gb_free(id)
{
	extern qnode *cb_free();

	if (qu_find(as_root, id))
		(void) cb_free(id);
}

qnode *
cb_free(id)
{
	register qnode *qp, *ip, *np;

#       if(AS_DEBUG)
	print("cb_free: marking %x as deleteable\n", id);
#       endif
	if ((qp = qu_find(as_root, id)) == 0) {
		print("cb_free: free %d -- id unknown!\n", id);
		return ((qnode *) 0);
	}
	if ((qp->qu_flag & AS_DELETE) == 0) {
		/* Delete all storage associated with this node */
		for (ip = qp->qu_queue->qu_next; ip != qp->qu_queue; ip = np) {
			np = ip->qu_next;
			if (ip->qu_name != AS_SCOPE)
				qu_free(ip);
		}
		if ((ip = qu_head(qp->qu_queue)) == 0) {
			if (qp->qu_flag & AS_LOCAL)
				--as_nlocdelete;
			if (qp->qu_flag & AS_IDLISTED) {
				enqueue_cbid(id);
				flush_cbids();
			}
			qu_free(qp);
			return ((qnode *) 0);
		}
		begin {
			register site_id *sp;
			register n = 0;

			for (sp = current_view.sv_slist; *sp; ++sp)
				if (bit(&ip->qu_bitvec, SITE_NO(*sp)) && n++)
					break;
			if (n <= 1) {
				if (qp->qu_flag & AS_IDLISTED) {
					enqueue_cbid(id);
					flush_cbids();
				}
				qu_free(qp);
				return ((qnode *) 0);
			}
		}
		qp->qu_flag |= AS_DELETE;
		++as_ndelete;
		if (qp->qu_flag & AS_LOCAL)
			++as_nlocdelete;
		did_delete(0);
		ip = qu_add_bits(qp->qu_queue, AS_IGNORE);
	} else
		ip = qu_find(qp->qu_queue, AS_IGNORE);
	return (ip);
}

ab_free(id)
{
	ab_dofree(id, my_site_no);
}

ab_dofree(id, sno)
{
	register n;
	register qnode *qp, *ip, *dp;

	if ((qp = qu_find(as_root, id)) == 0) {
		print("ab_free -- id %x unknown\n", id);
		return;
	}
	if (qp->qu_flag & AS_DELETE)
		return;
	if ((ip = qu_find(qp->qu_queue, AS_FREED)) == 0)
		ip = qu_add_bits(qp->qu_queue, AS_FREED);
	if (bit(&ip->qu_bitvec, sno))
		return;
	bis(&ip->qu_bitvec, sno);
	if ((dp = qu_find(qp->qu_queue, AS_SCOPE)) == 0)
		panic("ab_free: AS_SCOPE for %x not found", id);
	begin {
		bitvec isdown;
		register site_id *sp;

		isdown = ones;
		for (sp = current_view.sv_slist; *sp; sp++)
			bic(&isdown, SITE_NO(*sp));
		bicv(&dp->qu_bitvec, &isdown);
	}
	for (n = 0; n < ISIS_BVL; n++)
		if (ip->qu_bitvec.bv_data[n] != dp->qu_bitvec.bv_data[n])
			break;
	if (n == ISIS_BVL)
		cb_free(id);
	else if (sno == my_site_no) {
		qp->qu_flag |= AS_ADELETE;
		++as_nlocdelete;
		did_delete(0);
	}
}

st_newview()
{
	register qnode *qp, *nqp;

	for (qp = as_root->qu_next; qp != as_root; qp = nqp) {
		register qnode *sc = qu_find(qp->qu_queue, AS_SCOPE), *fp;

		nqp = qp->qu_next;
		if (sc) {
			bicv(&sc->qu_bitvec, &current_view.sv_failed);
			if (fp = qu_find(qp->qu_queue, AS_FREED)) {
				register n;

				bicv(&fp->qu_bitvec, &current_view.sv_failed);
				for (n = 0; n < ISIS_BVL; n++)
					if (sc->qu_bitvec.bv_data[n] != fp->qu_bitvec.bv_data[n])
						break;
				if (n == ISIS_BVL)
					cb_free(qp->qu_name);
			}
		}
	}
}

static flushing, timeout_running, timeout_id;
int st_flush();

#define TIMEOUT_SLOW    1
#define TIMEOUT_FAST    2

st_timeout()
{
	if (flushing == 0 && (as_ndelete || as_nlocdelete)) {
		flushing = 1;
		t_fork(st_flush, 0, (message *) 0);
	}
	timeout_running = TIMEOUT_SLOW;
	timeout_id = timeout(TIMEOUT_SEC * sec, st_timeout, 0, 0);
}

did_delete(urgent)
{
	if (urgent || as_ndelete >= AS_MAXDELETE || as_nlocdelete >= AS_THRESHOLD) {
		if (flushing == 0) {
			if (timeout_running != TIMEOUT_FAST) {
				if (timeout_running == TIMEOUT_SLOW)
					timeout_cancel(timeout_id);
				timeout_running = TIMEOUT_FAST;
				timeout_id = timeout(sec, st_timeout, 0, 0);
			}
		}
	} else if (!timeout_running && flushing == 0) {
		timeout_running = TIMEOUT_SLOW;
		timeout_id = timeout(TIMEOUT_SEC * sec, st_timeout, 0, 0);
	}
}

st_deleteable(id)
{
	register qnode *qp;

	if ((qp = qu_find(as_root, id)) && (qp->qu_flag & AS_DELETE))
		return (TRUE);
	else
		return (FALSE);
}

/* Garbage collection algorithm as in the paper */
st_flush()
{
	register qnode *qp, *nqp;
	register nid;
	register st_idlist *sip;
	char ansvec[MAX_SITES];

#       if(AS_DEBUG)
	print("st_flush: starting to flush...\n");
#       endif

	do {
		bitvec gscope, dscope;
		site_id slist[MAX_SITES];

		sip = st_ids;
		bclr(&gscope);
		for (qp = as_root->qu_next; qp != as_root; qp = nqp) {
			nqp = qp->qu_next;
			if (qp->qu_flag & AS_DELETE) {
				sip->idl_id = qp->qu_name;
				sip->idl_ignore = qu_find(qp->qu_queue, AS_IGNORE)->qu_bitvec;
				sip->idl_scope = qu_find(qp->qu_queue, AS_SCOPE)->qu_bitvec;
				dscope = sip->idl_scope;
				bic(&dscope, my_site_no);
				bicv(&dscope, &sip->idl_ignore);
				if (btst(&dscope) == 0) {
					/* All sites in the scope will ignore this anyhow! */
					if (qp->qu_flag & AS_IDLISTED) {
						enqueue_cbid(qp->qu_name);
						flush_cbids();
					}
					qu_free(qp);
					continue;
				}
				bisv(&gscope, &sip->idl_scope);
				if (++sip == &st_ids[AS_MCOLLECT - 1])
					break;
			} else if ((qp->qu_flag & (AS_ADELETE | AS_ABFSENT)) == AS_ADELETE) {
				qp->qu_flag |= AS_ABFSENT;
				sip->idl_id = qp->qu_name | ABFREED;
				sip->idl_scope = qu_find(qp->qu_queue, AS_SCOPE)->qu_bitvec;
				bisv(&gscope, &sip->idl_scope);
				if (++sip == &st_ids[AS_MCOLLECT - 1])
					break;
			}
		}
		if ((nid = sip - st_ids) == 0)
			break;
		bzero(sip, sizeof(*sip));
#           if(AS_DEBUG)
		print("st_flush: sending flush messages\n");
#           endif
		bic(&gscope, my_site_no);
		if (btst(&gscope)) {
			register site_id *sp, *sl;

			sl = slist;
			for (sp = current_view.sv_slist; *sp; sp++)
				if (bit(&gscope, SITE_NO(*sp)))
					*sl++ = *sp;
			if (sl != slist) {
				register message *msg;
				register len = (nid + 1) * sizeof(st_idlist);

				*sl = 0;
				acquire_view_r_lock(gscope);
				EVENT(S_ASROUNDS);
				msg = msg_genmsg(AS_IGNORES, (char *) st_ids, FTYPE_IDL, len, 0);
				BCAST_SL(slist, PROTOCOLS, PR_STGPARTICIPANT, msg, ALL,
					 collect_answ, ansvec, 1);
				msg_delete(msg);
				release_view_r_lock(gscope);
			}
		}
#           if(AS_DEBUG)
		print("st_flush: got all flush replies\n");
#           endif
		while (--sip >= st_ids)
			if ((sip->idl_id & ABFREED) == 0) {
				register qnode *qp;

				if (qp = qu_find(as_root, sip->idl_id)) {
					if (qp->qu_flag & AS_IDLISTED)
						enqueue_cbid(sip->idl_id);
#ifdef                  AS_DEBUG
					print("delete astore node for %x\n", sip->idl_id);
#endif				/* AS_DEBUG */
					qu_free(qp);
				}
			}
		flush_cbids();
	}
	while (nid == AS_MCOLLECT - 1);
	flushing = 0;
	as_nlocdelete = as_ndelete = 0;
	for (qp = as_root->qu_next; qp != as_root; qp = qp->qu_next) {
		if (qp->qu_flag & AS_DELETE)
			++as_ndelete;
		if ((qp->qu_flag & (AS_DELETE | AS_ADELETE)) && (qp->qu_flag & AS_LOCAL))
			++as_nlocdelete;
	}
	if (as_ndelete)
		did_delete(0);
#       if(AS_DEBUG)
	print("st_flush: done flushing.\n");
#       endif
}

st_gparticipant(mp)
	register message *mp;
{
	register from;

	from = (msg_getsender(mp))->addr_site;
#       if(AS_DEBUG)
	print("st_gparticipant: received flush message from site %d.\n", from);
#       endif
	if (from != my_site_no) {
		register st_idlist *st_ids, *sip;
		register qnode *qp, *np;

		st_ids = (st_idlist *) msg_getfield(mp, AS_IGNORES, 1, (int *) 0);
		sip = st_ids;

		while (sip->idl_id) {
			if (bit(&sip->idl_scope, my_site_no) == 0) {
				++sip;
				continue;
			}
			if (sip->idl_id & ABFREED) {
				sip->idl_id &= ~ABFREED;
				ab_dofree(sip->idl_id, from);
			} else if (bit(&sip->idl_ignore, my_site_no) == 0) {
#ifdef              AS_DEBUG
				print("Site %d: got phase-1 delete from %d for %x\n", from,
				      sip->idl_id);
#endif				/* AS_DEBUG */
				if (qu_find(as_root, sip->idl_id) == 0) {
					qp = qu_add_qu(as_root, sip->idl_id, qu_null());
					qp->qu_flag = 0;
					qp->qu_queue->qu_flag = 0;
					qu_add_bits(qp->qu_queue, AS_SCOPE)->qu_bitvec =
					    sip->idl_scope;
				}
				if (np = cb_free(sip->idl_id))
					bis(&np->qu_bitvec, from);
			}
			++sip;
		}
	}
	reply(mp, "*", FTYPE_CHAR, 1);
#       if(AS_DEBUG)
	print("st_gparticpant: replied to flush msg\n");
#       endif
}
