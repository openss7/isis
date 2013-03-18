/*  $RCSfile: pr_addr.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:49 $  */
/*
 *	Originally coded by Ken Birman
 *
 *      Routines providing addressing support of various kinds
 *      Macros are used to avoid having lots of nearly identical code
 *
 *      SENDMSG loops if an iterated delivery fails and must be retried
 *      The address expansion routines return a negative count in cases
 *      where iterated delivery must be done.
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

sys_groupview *view_get(), *cache_get();

#define         AD_GBGROW       0x1
#define         AD_GBCAST       0x2
#define         AD_ITER         0x4

#define         DIRECT          0
#define         INDIRECT        1
#define         STARTEV         2
#define         ENDEV           3

int pr_fbcast(), pr_cbcast(), pr_abcast(), pr_gbcast(), pr_bcast();
int direct_cbcast(), direct_fbcast(), direct_abcast(), direct_gbcast(), direct_bcast();
int collect_answ();

/* Routines to use in normal (direct) and iterated (indirect) deliveries */
int (*using_cbcast[]) () = {
direct_cbcast, pr_cbcast, (ifunc *) S_CBSTART, (ifunc *) S_CBDONE};

int (*using_fbcast[]) () = {
direct_fbcast, pr_fbcast, (ifunc *) S_CBSTART, (ifunc *) S_CBDONE};

int (*using_abcast[]) () = {
direct_abcast, pr_abcast, (ifunc *) S_ABSTART, (ifunc *) S_ABDONE};

int (*using_gbcast[]) () = {
direct_gbcast, pr_gbcast, (ifunc *) S_GBSTART, (ifunc *) S_GBDONE};

int (*using_bcast[]) () = {
direct_bcast, pr_bcast, (ifunc *) S_BBSTART, (ifunc *) S_BBDONE};

#define ITERATED        0x10000

DO_DIRECT(tp, routine)
	register task *tp;
	int (*routine) ();
{
	register status;

	status = (*routine) (tp->task_msg);
	t_sig(&tp->task_iwait, status);
}

#define NONINTERATED(name,routine)                                      \
name(tp)                                                                \
  register task *tp;                                                    \
  {                                                                     \
        DO_DIRECT(tp, routine);                                         \
  }

/* Direct ones */
NONINTERATED(direct_cbcast, pr_cbcast)
    NONINTERATED(direct_fbcast, pr_fbcast)
    NONINTERATED(direct_abcast, pr_abcast)
    NONINTERATED(direct_gbcast, pr_gbcast)
    NONINTERATED(direct_bcast, pr_bcast)

/*
 * This code is quite tricky due to the handling of iterated deliveries.
 * The iterated delivery routine signals the sending task to tell it
 * if it can safely run or not.  Meanwhile, messages can pile up
 * for it, but this shouldn't pose any problems
 */
    SENDMSG(msg, alist, ndests, routine, died, nwanted, collect, answ, alen)
	register ndests;
	register message *msg;
	address *alist;
	char *answ;
	char *died;
	register (*routine[]) ();
	int (*collect) ();
{
	int iflag;

	if (died)
		bzero(died, MAX_PROCS);
	if (ndests < 0)
		return (ndests);
	iflag = ndests & ITERATED;
	switch (ndests &= ~ITERATED) {
	case 0:
		return (0);

	case 1:
		if (routine == using_abcast)
			routine = using_fbcast;
	}
	if (routine == using_cbcast || routine == using_fbcast) {
		register bitvec *scope_ptr;
		register address *ap;
		register sno = 0;

		if ((scope_ptr = (bitvec *) msg_getfield(msg, SYSFLD_SCOPE, 1, 0)) == (bitvec *) 0) {
			static bitvec nullscope;

			scope_ptr =
			    (bitvec *) msg_insertfield(msg, SYSFLD_SCOPE, (char *) &nullscope,
						       FTYPE_BITVEC, sizeof(nullscope));
		}

		for (ap = alist; !aptr_isnull(ap); ap++) {
			if (sno && sno != ap->addr_site)
				sno = -1;
			else
				sno = ap->addr_site;
			bis(scope_ptr, ap->addr_site);
		}
		/* Optimizations: to one remote site and/or my_site */
		if (sno == -1)
			bis(scope_ptr, my_site_no);
		else {
			/* To one site, no need to set my_site_no in this case(!) */
			bclr(scope_ptr);
			bis(scope_ptr, sno);
		}
	}
	EVENTV(S_FANOUT, ndests);
	msg_setreplyto(msg, &my_address);
	if (iflag == 0) {
		int msgid = (CONJURE += 2);

		if (nwanted)
			++msgid;
		if (nwanted || msg_getid(msg) == 0)
			msg_getid(msg) = msgid;
		msg_setdests(msg, alist);
#           if (AD_DEBUG)
		print("SENDMSG(direct): ");
		pmsg(msg);
#           endif
		if (msg_tracemsgs) {
			int pno;

			if (routine == using_bcast)
				pno = PN_BCAST;
			else if (routine == using_cbcast)
				pno = PN_CBCAST;
			else if (routine == using_fbcast)
				pno = PN_FBCAST;
			else if (routine == using_abcast)
				pno = PN_ABCAST;
			else if (routine == using_gbcast)
				pno = PN_GBCAST;
			msg_replacefield(msg, SYSFLD_PROTO, (char *) &pno, FTYPE_LONG, sizeof(int));
		}
		ctp->task_msg = msg;
		ctp->task_msgid = msgid;
		EVENT((int) routine[STARTEV]);
		if (nwanted) {
			t_fork(routine[DIRECT], (char *) ctp, msg);
			if (t_wait(&ctp->task_iwait, "direct") == -1) {
				EVENT((int) routine[ENDEV]);
				return (IE_AGAIN);
			}
			ndests = collect_replies(msgid, alist, nwanted, collect, answ, alen, died);
		} else {
			/* Make a valiant effort to avoid blocking... */
			if ((*routine[INDIRECT]) (msg) == -1) {
				EVENT((int) routine[ENDEV]);
#                   if (AD_DEBUG)
				print("task %x direct broadcast must run again...\n", ctp);
#                   endif
				return (IE_AGAIN);
			}
			ndests = 0;
		}
		EVENT((int) routine[ENDEV]);
	} else {
		int msgid = (CONJURE += 2);

		if (nwanted)
			++msgid;
		msg_getid(msg) = msgid;
		msg_setdests(msg, alist);
#           if (AD_DEBUG)
		print("SENDMSG(iterated): ");
		pmsg(msg);
#           endif
		if (msg_tracemsgs) {
			int pno;

			if (routine == using_bcast)
				pno = PN_IBCAST;
			else if (routine == using_cbcast)
				pno = PN_ICBCAST;
			else if (routine == using_fbcast)
				pno = PN_IFBCAST;
			else if (routine == using_abcast)
				pno = PN_IABCAST;
			else if (routine == using_gbcast)
				pno = PN_IGBCAST;
			msg_replacefield(msg, SYSFLD_PROTO, (char *) &pno, FTYPE_LONG, sizeof(int));
		}
		ctp->task_msg = msg;
		ctp->task_msgid = msgid;
		EVENT((int) routine[STARTEV]);
		t_fork_urgent(routine[INDIRECT], (char *) msg, msg);
		if (nwanted == 0)
			/* Must wait to know if it got through or not! */
			++nwanted;
		ndests = collect_replies(msgid, alist, nwanted, collect, answ, alen, died);
#           if (AD_DEBUG)
		if (ndests == IE_AGAIN)
			print("task %x iterated broadcast must run again...\n", ctp);
#           endif
		cache_refresh(msg, ndests);
		EVENT((int) routine[ENDEV]);
	}
	return (ndests);
}

#define SEND_MSG(proc)       (n = SENDMSG(msg, alist, n, proc, died, nwanted, collect, answ, alen))

#define GENERIC(name, routine, fv)                                      \
name(aexpr, msg, nwanted, collect, answ, alen)                          \
  address *aexpr;                                                       \
  message *msg;                                                         \
  int (*collect)();                                                     \
  char *answ;                                                           \
  {                                                                     \
        register n = 0, firsttime = 0, is_a_copy = 0, f = fv;           \
        address sender, alist[ADDR_LEN];                                \
        char died[MAX_PROCS];                                           \
        sender = *msg_getsender(msg);                                   \
        alist[0] = NULLADDRESS;                                         \
        do                                                              \
        {                                                               \
            if(firsttime++ && (f&AD_GBCAST))                            \
                EVENT (S_GBABORTS);                                     \
            if(n == IE_AGAIN)                                           \
            {                                                           \
                register message *cmsg = msg_copy(msg);                 \
                if(is_a_copy++)                                         \
                    msg_delete(msg);                                    \
                msg = cmsg;                                             \
                if(f&AD_GBCAST)                                         \
                    _msg_deleteall(msg, SYSFLD_VERIFY);                 \
                _msg_deleteall(msg, SYSFLD_VIEWID);                     \
            }                                                           \
            n = aexpr_expand(sender, aexpr, alist, msg, f);             \
            f |= AD_ITER;                                               \
        }                                                               \
        while(SEND_MSG(routine) == IE_AGAIN);                           \
        if(is_a_copy)                                                   \
            msg_delete(msg);                                            \
        pg_wait(sender, alist, died);                                   \
        return(n);                                                      \
  }

#define GENERIC_GROW(name, routine, fv)                                 \
name(aexpr, msg, nwanted, collect, answ, alen)                          \
  address *aexpr;                                                       \
  message *msg;                                                         \
  int (*collect)();                                                     \
  char *answ;                                                           \
  {                                                                     \
        register n = 0, firsttime = 0, is_a_copy = 0, f = fv;           \
        address sender, alist[ADDR_LEN];                                \
        char died[MAX_PROCS];                                           \
        sender = *msg_getsender(msg);                                   \
        alist[0] = NULLADDRESS;                                         \
        do                                                              \
        {                                                               \
            if(firsttime++ && (f&AD_GBCAST))                            \
                EVENT (S_GBABORTS);                                     \
            if(n == IE_AGAIN)                                           \
            {                                                           \
                register message *cmsg = msg_copy(msg);                 \
                if(is_a_copy++)                                         \
                    msg_delete(msg);                                    \
                msg = cmsg;                                             \
                if(f&AD_GBCAST)                                         \
                    _msg_deleteall(msg, SYSFLD_VERIFY);                 \
                _msg_deleteall(msg, SYSFLD_VIEWID);                     \
            }                                                           \
            n = aexpr_expand(sender, aexpr, alist, msg, f|AD_GBGROW);   \
            f |= AD_ITER;                                               \
        }                                                               \
        while(SEND_MSG(routine) == IE_AGAIN);                           \
        if(is_a_copy)                                                   \
            msg_delete(msg);                                            \
        pg_wait(sender, alist, died);                                   \
        return(n);                                                      \
  }

#define GENERIC_V(name, routine)                                        \
name(v, pid, ent, msg, nwanted, collect, answ, alen)                    \
  sview *v;                                                             \
  message *msg;                                                         \
  int (*collect)();                                                     \
  char *answ;                                                           \
  {                                                                     \
        register n = 0, is_a_copy = 0;                                  \
        address alist[ADDR_LEN];                                        \
        static char *died;                                              \
                                                                        \
        do                                                              \
        {                                                               \
            n = v_expand(v, pid, ent, alist);                           \
            if(n == IE_AGAIN)                                           \
            {                                                           \
                register message *cmsg = msg_copy(msg);                 \
                if(is_a_copy++)                                         \
                    msg_delete(msg);                                    \
                msg = cmsg;                                             \
            }                                                           \
        }                                                               \
        while(SEND_MSG(routine) == IE_AGAIN);                           \
        if(is_a_copy)                                                   \
            msg_delete(msg);                                            \
        return(n);                                                      \
  }

/*****************************************************************************/
GENERIC(GBCAST, using_gbcast, AD_GBCAST)
    GENERIC(ABCAST, using_abcast, 0)
    GENERIC(CBCAST, using_cbcast, 0)
    GENERIC(FBCAST, using_fbcast, 0)
    GENERIC(BCAST, using_bcast, 0)
    GENERIC_V(GBCAST_V, using_gbcast)
    GENERIC_V(ABCAST_V, using_abcast)
    GENERIC_V(CBCAST_V, using_cbcast)
    GENERIC_V(BCAST_V, using_bcast)
    GENERIC_GROW(GBCAST_GROW, using_gbcast, AD_GBCAST)
/*****************************************************************************/
/* Special for Tommy */
    BCAST_SL(slist, pid, ent, msg, nwanted, collect, answ, alen)
	site_id *slist;
	message *msg;
	char *answ;
	int (*collect) ();
{
	address alist[ADDR_LEN];
	register cnt = 0;
	register address *ap;
	register site_id *sp;

	ap = alist;
	for (sp = slist; *sp; sp++)
		*ap++ = ADDRESS(SITE_NO(*sp), SITE_INCARN(*sp), pid, ent);
	*ap = NULLADDRESS;
	cnt = ap - alist;
	cnt = SENDMSG(msg, alist, cnt, using_bcast, (char *) 0, nwanted, collect, answ, alen);
	return (cnt);
}

#ifndef AD_DEBUG
#define add_ent(ap, bp, e)                                      \
  {                                                             \
        if(ap == &alist[ADDR_LEN-1])                            \
        {                                                       \
            ap = &alist[dl_compact(alist, ap-alist)];           \
            if(ap == &alist[ADDR_LEN-1])                        \
                goto toolong;                                   \
        }                                                       \
        *ap = *bp;                                              \
        ap->addr_entry = e;                                          \
        ++ap;                                                   \
  }
#else
#define add_ent(ap, bp, e)                                      \
  {                                                             \
        print("add_ent: ");                                     \
        paddr(bp);                                              \
        print("\n");                                            \
        if(ap == &alist[ADDR_LEN-1])                            \
        {                                                       \
            ap = &alist[dl_compact(alist, ap-alist)];           \
            if(ap == &alist[ADDR_LEN-1])                        \
                goto toolong;                                   \
        }                                                       \
        *ap = *bp;                                              \
        ap->addr_entry = e;                                          \
        ++ap;                                                   \
  }
#endif

aexpr_expand(sender, aexpr, alist, msg, flag)
	address sender, *aexpr, *alist;
	message *msg;
{
	register address *ep, *ap = alist;
	register sys_groupview *pg;
	register qnode *cl_root;
	int agcount, used_cache, used_view, n, msg_id;

	if (aexpr->addr_site == 0) {
		/* Special case -- a null list */
		alist[0] = NULLADDRESS;
		return (0);
	}
	/* Otherwise, always get the lock, but unlock it if we didn't use the view */
	begin {
		msg_id = GENMSGID;
		msg_replacefield(msg, SYSFLD_PROTID, (char *) &msg_id, FTYPE_LONG, sizeof(int));
	}
      again:
	cl_root = 0;
	agcount = used_cache = used_view = 0;
#       if (AD_DEBUG)
	print("aexpr_expand: task %x lock before expand, sender ", ctp);
	paddr(&sender);
	print(", dests ");
	paddrs(aexpr);
	print("\n");
#       endif
	shr_glock(msg_id, sender.addr_process);
	sender.addr_entry = 0;
	for (ep = aexpr; !aptr_isnull(ep); ep++) {
		if (used_cache)
			goto restrict;
		if (addr_isgid(ep)) {
			if (cl_root == 0 && (cl_root = pg_find(pg_root, &sender)) == 0)
				++used_cache;
			else if (pg = view_get(cl_root->qu_queue, *ep)) {
				register address *mp;

				++used_view;
				for (mp = pg->pg_alist; !aptr_isnull(mp); mp++)
					add_ent(ap, mp, ep->addr_entry);
				if (pg_changesview(msg))
					for (++mp; !aptr_isnull(mp); mp++)
						add_ent(ap, mp, ep->addr_entry);
				if (flag & AD_GBGROW) {
					if (agcount++)
						goto toolong;
					/* Piggyback current sys_groupview view */
					msg_replacefield(msg, SYSFLD_PGVIEW, (char *) pg,
							 FTYPE_PGROUP, pglength(pg));
				}
				if (flag & AD_GBCAST) {
					verify vi;

					vi.vi_gid = *ep;
					vi.vi_viewid = pg->pg_viewid;
					bclr(&vi.vi_sites);
					for (mp = pg->pg_alist; !aptr_isnull(mp); mp++)
						bis(&vi.vi_sites, mp->addr_site);
					++mp;
					while (!aptr_isnull(mp)) {
						bis(&vi.vi_sites, mp->addr_site);
						mp++;
					}
					msg_insertfield(msg, SYSFLD_VERIFY, (char *) &vi,
							FTYPE_VERIFY, sizeof(vi));
				}
			} else
				++used_cache;
		} else if (addr_ispid(ep) && SITE_IS_UP(ep->addr_site, ep->addr_incarn))
			/* Unrestricted address */
			add_ent(ap, ep, ep->addr_entry);
	}
	if (used_cache) {
		register address *mp;

		if ((flag & AD_GBGROW) || ap != alist)
			goto restrict;
#           if (AD_DEBUG)
		print("aexpr_expand: unlock (cache_get)\n");
#           endif
		shr_gunlock(msg_id, sender.addr_process);
		msg_deletefield(msg, SYSFLD_VREAD, 1);
		if ((pg = cache_get(sender, *aexpr, msg)) == 0)
			return (IE_UNKNOWN);
		if ((pg->pg_flag & PG_CACHED) == 0) {
#               if (AD_DEBUG)
			print("aexpr_expand: must retry.\n");
#               endif
			goto again;
		}
		for (mp = pg->pg_alist; !aptr_isnull(mp); mp++)
			add_ent(ap, mp, aexpr->addr_entry);
	}
	*ap = NULLADDRESS;
	n = dl_compact(alist, ap - alist);

	if (used_cache) {
		if (pg_changesview(msg)) {
			--pg->pg_ccount;
			goto restrict;
		}
		return (n | ITERATED);
	}
	if (flag & AD_GBCAST) {
#           if (AD_DEBUG)
		print("aexpr_expand: unlock (is a gbcast)\n");
#           endif
		shr_gunlock(msg_id, sender.addr_process);
		msg_deletefield(msg, SYSFLD_VREAD, 1);
	} else if (used_view == 0) {
#           if (AD_DEBUG)
		print("aexpr_expand: unlock (didn't use view)\n");
#           endif
		shr_gunlock(msg_id, sender.addr_process);
		msg_deletefield(msg, SYSFLD_VREAD, 1);
	} else if (msg_getfield(msg, SYSFLD_VREAD, 1, 0) == 0)
		msg_insertfield(msg, SYSFLD_VREAD, (char *) 0, FTYPE_CHAR, 0);
#       if (AD_DEBUG)
	print("aexpr_expand: returns %d dests = ", n);
	paddrs(alist);
	print("\n");
#       endif
	return (n);

      restrict:
	/* Error cases: always unlock the process */
#       if (AD_DEBUG)
	print("aexpr_expand: unlock (IE_RESTRICTED)\n");
#       endif
	shr_gunlock(msg_id, sender.addr_process);
	return (IE_RESTRICTED);

      toolong:
#       if (AD_DEBUG)
	print("aexpr_expand: unlock (IE_TOOLONG)\n");
#       endif
	shr_gunlock(msg_id, sender.addr_process);
	return (IE_TOOLONG);
}

pg_isgrow(mp)
	register message *mp;
{
	return (msg_getfield(mp, SYSFLD_PGVIEW, 1, (int *) 0) != 0);
}

pg_changesview(mp)
	register message *mp;
{
	return (msg_getfield(mp, SYSFLD_VCHANGE, 1, (int *) 0) != 0);
}

pg_readsview(mp)
	register message *mp;
{
	return (msg_getfield(mp, SYSFLD_VREAD, 1, (int *) 0) != 0);
}

v_expand(v, pid, ent, alist)
	sview *v;
	address *alist;
{
	register address *ap = alist;
	register site_id *sp;
	register n;

	for (sp = v->sv_slist; *sp; sp++) {
		if (ap == &alist[ADDR_LEN])
			return (IE_TOOLONG);
		*ap++ = ADDRESS(SITE_NO(*sp), SITE_INCARN(*sp), pid, ent);
	}
	*ap = NULLADDRESS;
	n = dl_compact(alist, ap - alist);
	return (n);
}

/*****************************************************************************/
/*            Compaction routine, other useful stuff                         */
/*****************************************************************************/

/* This uses an address comparison rule that checks entry but not portno */
#define   _Docmp(a,la,b,lb)   ((la[0]!=lb[0])?(la[0]-lb[0]): (a->addr_entry>=0&&b->addr_entry>=0)? 0: (a->addr_entry-b->addr_entry))
#define   Addr_cmp(a,b)       _Docmp((a),((long*)a),(b),((long*)b))
cmp_addr(a0, a1)
	register address *a0, *a1;
{
	return (Addr_cmp(a0, a1));
}

dl_compact(alist, dl)
	register address *alist;
{
	register address *ap, *aap;

	if (dl <= 1)
		return (dl);
	qsort((char *) alist, dl, sizeof(address), cmp_addr);
	aap = alist;
	for (ap = &alist[1]; !aptr_isnull(ap); ap++)
		if (addr_cmp(ap, aap))
			*++aap = *ap;
	*++aap = NULLADDRESS;
	return (aap - alist);
}

address
ADDRESS(s, i, p, e)
	site_id s;
{
	address d;

	d.addr_site = s;
	d.addr_incarn = i;
	d.addr_process = p;
	d.addr_entry = e;
	d.addr_portno = 0;
	return (d);
}

_addr_cmp(a, b)
	address *a, *b;
{
	if (a->addr_site != b->addr_site)
		return (a->addr_site - b->addr_site);
	if (addr_isgid(a)) {
		if (a->addr_groupid != b->addr_groupid)
			return (a->addr_groupid - b->addr_groupid);
	} else if (a->addr_process != b->addr_process)
		return (a->addr_process - b->addr_process);
	if (a->addr_portno > 0 && b->addr_portno > 0 && a->addr_portno != b->addr_portno)
		return (a->addr_portno - b->addr_portno);
	if (a->addr_incarn != b->addr_incarn)
		return (a->addr_incarn - b->addr_incarn);
	if (a->addr_entry == 0 || b->addr_entry == 0)
		return (0);
	return (a->addr_entry - b->addr_entry);
}

cmp_sid(s1, s2)
	register site_id *s1, *s2;
{
	return (*s1 - *s2);
}

_msg_deleteall(mp, fld)
	message *mp;
{
	msg_deleteall(mp, fld);
	if (msg_getfield(mp, fld, 1, (int *) 0))
		panic("msg_deleteall fuckup!");
}
