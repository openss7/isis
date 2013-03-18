/*  $RCSfile: cl_bypass.c,v $ $Revision: 2.109 $ $Date: 90/09/14 14:18:30 $  */
/*
 *	Originally coded by Ken Birman
 *      This module implements the client-client bypass mechanism
 *      Used if ISIS was compiled with -DBYPASS
 *      Performance factor of 5-10 improvement
 *
 *      ISIS release V2.0, May 1990
 *      Export restrictions apply
 *
 *      The contents of this file are subject to a joint, non-exclusive
 *      copyright by Ken Birman and the ISIS Project.  Permission is granted for
 *      use of this material in unmodified form in commercial or research
 *      settings.  Creation of derivative forms of this software may be
 *      subject to restriction; obtain written permission from the ISIS Project
 *      in the event of questions or for special situations.
 */

#include "isis.h"

#define CL_ABNEEDSORDER      -1
#define BYP_DEBUG	     0		/* A lot faster if disabled */
#define BYP_VERBOSE	     0		/* A lot quieter if disabled */
#define WTIMEOUT	     30000      /* Limit time on bypass_waitq */

int     isis_nblock;

#ifdef	BYPASS
int isis_bypass_enabled = 1;
#else 	BYPASS
int isis_bypass_enabled = 0;
#endif	BYPASS

#ifdef	BYPASS
static  qnode *bc_list;
static  qnode *term_queue;
static  qnode *bypass_deleted;
qnode   *bypass_queue;
message *protos_blockmsg;		/* First message to "despool" */

adesc	bcn_ad	=	{ sizeof(bc_node), 0, 32};

#define	bc_alloc()	(bc_node*)mallocate(&bcn_ad)

void
bc_free(bc)
  register bc_node *bc;
  {
#if	(BYP_DEBUG)
	if(bc->bc_queue)
	    qu_free(bc->bc_queue);
#endif  (BYP_DEBUG)
	mdeallocate((char*)bc, &bcn_ad);
  }


static	ginfo *active_gip;          /* Set if there is a unique active group */
static  active_count;               /* Count of active groups */
static  condition want_active;      /* Queue for waiting in make_active */
static  condition *want_bypass;     /* Waiting for PBUFDIRTY to clear */
static  qnode *order_msg;	    /* Messages used by abcast algorithm */
static  flushing;                   /* Doing a flush */
static  condition wait_flush;       /* Waiting for flush to complete */
static  condition wait_doflush;     /* Waiting to start flush */
static  void ab_flushorder(), ab_setorder();
static  void bypass_probe();
void    by_dump(), bypass_aborder(), bypass_sendterminate(), waitq_free();


#define A_SNDBCAST    1
#define A_SNDMSG      2
#define A_RCVBCAST    3

void bypass_init()
  {
        isis_entry(GENERIC_BYFLSH, bypass_flush, "bypass_flush");
        isis_entry(GENERIC_BYINACTIVE, bypass_inactive, "bypass_inactive");
	isis_entry(GENERIC_BYPROBE, bypass_probe, "bypass_probe");
	isis_entry(GENERIC_BYORDER, bypass_aborder, "bypass_aborder");
	isis_task(ab_flushorder, "bypass:ab_flushorder");
	isis_task(bypass_sendterminate, "bypass:send_terminate");
	isis_task((vfunc *) by_flushfirst, "bypass:by_flushfirst");
	isis_task((vfunc *) bypass_checkview, "bypass_checkview");
	isis_task((vfunc*)bypass_unblock, "bypass_unblock");
	isis_task((vfunc*)waitq_free, "bypass_waitq_free");
	bypass_queue = qu_null();
	order_msg = qu_null();
	term_queue = qu_null();
	bypass_deleted = qu_null();
	bypass_waitq = qu_null();
        bc_list = qu_null();
	isis_transport("isis", (ifunc*)net_send, (ifunc*)0, (ifunc*)0);
  }

static	NT;
static	char	transport_names[NTRANSPORT][16];

transport_lookup(pname)
  register char *pname;
  {
	register pn;
	for(pn = 0; pn < NT; pn++)
	    if(strcmp(transport_names[pn], pname) == 0)
		return(pn);
	return(-1);
  }

isis_transport(pname, send_func, view_func, dead_func)
  char *pname;
  ifunc *send_func, *view_func, *dead_func;
  {
	int pn;
	if(transport_lookup(pname) != -1)
	    panic("isis_transport: protocol <%s> redefined!", pname);
	bcopy(pname, transport_names[pn = NT++], strlen(pname));
	by_transport[pn] = send_func;
	by_newview[pn] = view_func;
	by_physdead[pn] = dead_func;
  }

/* ISIS protos sometimes piggybacks bypass flags on other messages */
void
pbuf_dirty()
  {
        want_bypass = 0;
        isis_state |= ISIS_PBUFDIRTY;
  }

void
start_flush(gip, why)
  char *why;
  register ginfo *gip;
  {
	while(gip->gi_flag&GI_FLUSHING)
	    t_wait_l(&wait_doflush, why);
	if(gip->gi_flag == 0)
	    panic("group deleted while doing start-flush on it!");
	gip->gi_flag |= GI_FLUSHING;
  }

/* When the message queue is drained */
void
done_flush()
  {
	register ginfo *gip;
	void bypass_sendterminate();
	register qnode *qp;
	register term_flag = 0;
	for(gip = isis_groups; gip; gip = gip->gi_next)
	{
	    if((gip->gi_flag&GI_FLUSHING) == 0)
		continue;
	    gip->gi_flag &= ~GI_FLUSHING;
	    if(gip->gi_myseqn == 0)
		continue;
	    ++term_flag;
            for(qp = term_queue->qu_next; qp != term_queue; qp = qp->qu_next)
                if(qp->qu_gip == gip)
		{
                    qp->qu_name = gip->gi_myseqn;
                    break;
		}
	    if(qp == term_queue)
                qp = qu_add_gip(term_queue, gip->gi_myseqn, gip);
	}
	t_sig_all(&wait_doflush, 0);
	if(term_flag)
	    bypass_sendterminate();
  }

void
waitq_free(qp)
  register qnode *qp;
  {
#       if (BYP_VERBOSE)
	    print("Timeout, discard wait-listed ");
	    pmsg(qp->qu_msg);
#       endif
	qu_free(qp);
  }

/* Second chance to check: about to install new view */
message *
bypass_send(protocol, exmode, dests, msg, nwant, flag, pn)
  register address *dests;
  register message *msg;
  {
        message *collect_replies();
        register groupview *bv;
        register ginfo *gip;
        register address *ap, *dp;
        register entry, myrank;
	register bc_node *bc = 0;
        int msgid, bypass_sent();
	vfunc *routine = NULLROUTINE;
        int sngl_dest = 0;
        address *true_dests = 0;
        extern ISIS_MSGID;
	void bc_countdown(), ms_countdown();

        if(!aptr_isnull(&dests[1]))
	    goto pbchk;
        if((flag&ISISBC_NONVSYNC) == 0)
	{
	    if((isis_state&ISIS_PBUFDIRTY) && protocol == CL_CBCAST)
	    {
                if(want_bypass)
	            t_wait_l(want_bypass, "bcast: waiting for pbuf-dirty to clear");
	        else
	        {
                    condition *wait_queue;
		    want_bypass = wait_queue = (condition*)malloc(sizeof(condition));
		    *wait_queue = 0;
                    flush();
		    if(want_bypass == wait_queue)
		    {
                        isis_state &= ~ISIS_PBUFDIRTY;
		        want_bypass = 0;
		    }
                    t_sig_all(wait_queue, 0);
		    free(wait_queue);
	        }
	    }
	}
#        if (BYP_VERBOSE)
         {
            print("[%d]: bypass_send, protocol %d, dests ", my_process_id, protocol);
	    paddrs(dests);
	    print(", flushing %d\n", flushing);
        }
#       endif
  again:
	if(flushing && protocol != CL_MBCAST)
	    t_wait_l(&wait_flush, "bypass: waiting for flush to complete");
        if((addr_isgid(&dests[0]) || addr_isplist(&dests[0])) && (gip = map_gaddr(&dests[0])))
        {
            /* Case of bcast to a group to which I belong */
	    if(protocol != CL_FBCAST || protocol != CL_MBCAST)
	        make_active(gip, A_SNDBCAST, pn);
	    if(flushing)
	    {
		make_inactive(gip, A_SNDBCAST);
		goto again;
	    }
	    if(gip->gi_parent)
                bv = gip->gi_view;
	    else
                bv = gip->gi_bypassview;
            if(bv == 0 || (bv->gv_flag&PG_VALID) == 0)
	    {
		make_inactive(gip, A_SNDBCAST);
                goto pbchk;
	    }
            myrank = 0;
            for(ap = bv->gv_members; !aptr_isnull(ap); ap++)
                if(addr_cmp(ap, &my_address) == 0)
                    break;
                else
                    ++myrank;
            if(myrank == bv->gv_nmemb)
	    {
		make_inactive(gip, A_SNDBCAST);
                goto pbchk;
	    }
	    if(gip->gi_parent)
	    {
                ap = msg_setdests(msg, bv->gv_members);
                true_dests = ap;
		gip = gip->gi_parent;
                bv = gip->gi_bypassview;
		dp = bv->gv_members;
	    }
	    else
                dp = ap = msg_setdests(msg, bv->gv_members);
	    if(protocol == CL_FBCAST || protocol == CL_MBCAST)
	    {
		make_inactive(gip, A_SNDBCAST);
		goto xmit;
	    }
        }
        else if(!addr_ispid(&dests[0]))
            goto pbchk;
        else
	{
            /* Case of bcast to a single process... member of group to which I belong? */
	    if(gip = active_gip)
            {
                /* active_gip is set if there is only one that is active */
                if((bv = gip->gi_bypassview) == 0)
		    goto pbchk;
		if(!pg_ismemb(bv, &dests[0]))
		    gip = 0;
	    }
	    if(gip == (ginfo*)0)
                for(gip = isis_groups; gip; gip = gip->gi_next)
	        {
                    if(gip == active_gip)
                        continue;
                    if((bv = gip->gi_bypassview) == 0)
		        continue;
		    if(pg_ismemb(bv, &dests[0]))
		        break;
	        }
	    if(gip == (ginfo*)0)
		goto pbchk;
	    ++sngl_dest;
            dp = ap = msg_setdests(msg, dests);
	    if(protocol == CL_FBCAST || protocol == CL_MBCAST)
            {
#	        if (BYP_VERBOSE)
	            print("[%d]: ... send with no BYINFO field\n",  my_process_id); 
#	        endif
		goto xmit;
            }
	    protocol = CL_CBCAST;
	    make_active(gip, A_SNDMSG, pn);
	    if(flushing)
	    {
		make_inactive(gip, A_SNDMSG);
		goto again;
	    }
	}
	if(!sngl_dest)
	{
	    register address *xp = dp;
	    bc = bc_alloc();
	    bc->bc_gip = gip;
	    bc->bc_seqn = ++gip->gi_nextseqn;
            bc->bc_viewid = bv->gv_viewid;
	    bc->bc_ndests = bv->gv_nmemb;
	    qu_add_bc(bc_list, bc->bc_seqn, bc, bc_free);
#if	    (BYP_DEBUG)
	    bc->bc_queue = qu_null();
            while(!aptr_isnull(xp))
                pg_add(bc->bc_queue, xp++, NULLARG, NULLROUTINE);
#endif      (BYP_DEBUG)
	    routine = (vfunc*)bc_countdown;
	    if(protocol == CL_ABCAST && myrank)
		protocol = CL_ABNEEDSORDER;
	}
        else
        {
	    bc = bc_alloc();
	    bc->bc_gip = gip;
            bc->bc_viewid = bv->gv_viewid;
#if	    (BYP_DEBUG)
	    bc->bc_queue = (qnode*)0;
#endif      (BYP_DEBUG)
            routine = (vfunc*)ms_countdown;
        }
	begin
	{
            static by_info binfo;
	    register by_info *byi;
            msg_setbyi(msg, &binfo);
            byi = msg_getbyi(msg);
	    byi->by_group = *gip->gi_gaddr;
            byi->by_dest = dests[0];
	    byi->by_protocol = protocol;
            isis_ctp->task_sentto = dests[0];
            byi->by_viewid = bv->gv_viewid;
	    byi->by_myseqn = gip->gi_myseqn;
            bcopy(gip->gi_bseqns, byi->by_bseq, bv->gv_nmemb<<2);
            if(!sngl_dest)
	    {
	       gip->gi_bcseqn = gip->gi_myseqn;
               byi->by_bseq[myrank] = bc->bc_seqn;
	    }
#	    if (BYP_VERBOSE)
            {
	        register *bseqn = byi->by_bseq;
	        print("[%d]: send [%d %d %d %d] view %d.%d, dests ", my_process_id, 
	          bseqn[0], bseqn[1], bseqn[2], bseqn[3], VMM(byi->by_viewid));
		paddrs(dests);
		print("\n");
	    }
#	    endif
	}
  xmit: entry = dests[0].addr_entry;
        while(!aptr_isnull(ap))
            ap++->addr_entry = entry;
	msg_setsender(msg, &isis_ctp->task_addr);
	if(msg_getid(msg) == 0)
	{
            msgid = isis_ctp->task_msgid;
            if(msgid == 0)
                msgid = (ISIS_MSGID += 2)+1;
	    msg_setid(msg, msgid);
	}
        if(isis_ctp->task_act)
            msg_setact(msg, &act_map[isis_ctp->task_act].act_id);
        while((*by_transport[pn])(dests, exmode, msg, dp, routine, bc, NULLARG) == -1)
	    pn = 0;
	if(exmode != 0)
	{
	    register address *ap = dp;
	    /* now we use exmode as a counter */
	    exmode = 0;
	    while(!aptr_isnull(ap))
	    {
		if(addr_ismine(ap))
		    (*routine)(ap, bc, 999);
		ap++;
		++exmode;
	    }
	}
        if(nwant)
            return(collect_replies(msgid, true_dests? true_dests: dp, nwant, exmode));
        return((message*)-1);
  pbchk:
        /* Before I can send a CBCAST to protos, must wait for activity to end */
	if(protocol == CL_CBCAST)
	    do_check_pbuf(nwant);
	return((message*)0);
  }

void
by_flush()
  {
	do_check_pbuf(1);
  }

int
pg_ismemb(gv, addr)
  register groupview *gv;
  address *addr;
  {
	address *ap;
	if(gv == 0 || (gv->gv_flag&PG_VALID) == 0)
	    return(0);
	for(ap = gv->gv_members; !aptr_isnull(ap); ap++)
	    if(addr_cmp(ap, addr) == 0)
		return(1);
	return(0);
  }

/* transport informs us that <bc> "made it" to <dest> */
void
bc_countdown(dest, bc, xx)
  address *dest;
  register bc_node *bc;
  {
        register ginfo *gip = bc->bc_gip;
	register qnode *qp, *xp;
#if	(BYP_DEBUG)
        if((qp = pg_find(bc->bc_queue, dest)) == 0)
	    panic("bc_inactive");
	qp->qu_pname.addr_incarn = -1;
#endif  (BYP_DEBUG)
	if(--bc->bc_ndests > 0)
	    return;
	while(qp = qu_head(bc_list))
	{
            register bc_node *bc;
            bc = qp->qu_bcn;
	    if(bc->bc_ndests)
	        return;
            if((gip->gi_flag&GI_INUSE) && gip->gi_bypassview &&
		bc->bc_viewid == gip->gi_bypassview->gv_viewid)
	    {
	        ++gip->gi_myseqn;
	        make_inactive(bc->bc_gip, A_SNDBCAST);
	    }
#if	    (BYP_DEBUG)
	    while(xp = qu_head(bc->bc_queue))
		qu_free(xp);
#endif      (BYP_DEBUG)
	    qu_free(qp);
	}
  }

bc_dump()
  {
        register qnode *qp, *dp;
        print("Dump of bc_nodes:\n");
        if(bc_list == 0)
            return;
        for(qp = bc_list->qu_next; qp != bc_list; qp = qp->qu_next)
        {
            register bc_node *bc = qp->qu_bcn;
            print("bc %x: gip %x ndests %d, seqn %d, ", bc, bc->bc_gip, bc->bc_ndests, bc->bc_seqn);
            print("viewid %d.%d, flag %d, remdests=[", VMM(bc->bc_viewid), bc->bc_flag);
#if	    (BYP_DEBUG)
            for(dp = bc->bc_queue->qu_next; dp && dp != bc->bc_queue; dp = dp->qu_next)
                paddr(&dp->qu_pname);
#endif      (BYP_DEBUG)
            print("]\n");
        }
  }

/* Version for point-to-point messages */
void
ms_countdown(dest, bc)
  address *dest;
  register bc_node *bc;
  {
	register ginfo *gip = bc->bc_gip;
	if(bc->bc_viewid == gip->gi_bypassview->gv_viewid)
	    make_inactive(gip, A_SNDMSG);
	bc_free(bc);
  }

void 
bypass_recv(byi, mp, pn)
  register by_info *byi;
  register message *mp;
  int pn;
  {
        register *bseqn, *gp, n;
        register ginfo *gip;
        register groupview *gv;
        int hisrank = -1, myrank = -1, sngl_dest = 0, gotone, wantflush = 0;
	address *sender, *gaddr;
	qnode *aq = 0;
#	if (BYP_VERBOSE)
	  bseqn = byi->by_bseq;
	  print("[%d]: got [%d %d %d %d] in ", my_process_id, 
	    bseqn[0], bseqn[1], bseqn[2], bseqn[3]);
	  paddr(&byi->by_group);
	  print(" from %d view %d.%d\n", msg_getsender(mp)->addr_process, VMM(byi->by_viewid));
          pmsg(mp);
#	endif
	sender = msg_getsender(mp);
	if(addr_isnull(sender))
	{
#	    if (BYP_VERBOSE)
		print("... null sender\n");
#	    endif
	    return;
	}
        sender->addr_entry = 0;
        begin
        {
	    register address *ap;
	    if((gip = map_gaddr(&byi->by_group)) == (ginfo*)0 ||
	       (gv = gip->gi_bypassview) == 0 ||
               (gv->gv_flag&PG_VALID) == 0 || VMAJOR(gv->gv_viewid) < VMAJOR(byi->by_viewid))
	    {
                register qnode *qp;
	        msg_increfcount(mp);
	        qp = pg_add_mp(bypass_waitq, &byi->by_dest, (VOID*)mp, MSG_DELETE);
                qp->qu_pn = pn;
		/* Wait a while for group to show up, then discard the node */
	        isis_timeout(WTIMEOUT, (vfunc*) waitq_free, (void*)qp, NULLARG);
#		if (BYP_VERBOSE)
		    print("... added to bywaitq\n");
#		endif
	        return;
	    }
	    if(byi->by_viewid != gip->gi_bypassview->gv_viewid)
	    {
#		if (BYP_VERBOSE)
		    print("... dup (expected viewid %d.%d)\n", VMM(gip->gi_bypassview->gv_viewid));
#		endif
		return;
	    }
            n = 0;
	    for(ap = gv->gv_members; !aptr_isnull(ap); ap++, n++)
	    {
		if(addr_cmp(ap, sender) == 0)
		    hisrank = n;
		if(addr_ismine(ap))
		    myrank = n;
	    }
	}
	if(byi->by_dest.addr_entry == GENERIC_BYINACTIVE)
	{
#	    if (BYP_VERBOSE)
	        print("... pass through to bypass_inactive\n");
#	    endif
            bypass_inactive(mp);
	    return;
	}
	bseqn = byi->by_bseq;
        byi->by_dest.addr_entry = 0;
	if(bseqn[hisrank] < 0)
	    bseqn[hisrank] = -bseqn[hisrank];
	if(addr_isgid(&byi->by_dest) && (gip = map_gaddr(&byi->by_dest)))
	{
            if(hisrank != myrank && !flushing)
            {
                register qnode *qp, *mqp;
		qp = pg_find(gip->gi_byprocs, sender);
		if(qp == 0)
		{
		    qp = pg_add_qu(gip->gi_byprocs, sender, qu_null());
		    qp->qu_gseqn = 0;
		}
		for(mqp = qp->qu_queue->qu_next; mqp != qp->qu_queue; mqp = mqp->qu_next)
		    if(mqp->qu_mseqn == bseqn[hisrank])
		    {
#			if (BYP_VERBOSE)
			    print("... dup 1\n");
#			endif
		        return; /* dup */
                    }
		    else if(mqp->qu_mseqn > bseqn[hisrank])
			break;
		/* This copy is for retransmission after a failure, if necessary */
		/* Tricky: keeps the queue sorted */
                msg_increfcount(mp);
                mqp = qu_add_mp(mqp, bseqn[hisrank], (VOID*)mp, MSG_DELETE);
		mqp->qu_flag = 0;
	        if(byi->by_myseqn != qp->qu_gseqn)
                    bypass_gcollect(qp, gip, sender, byi->by_myseqn);
            }
	}
	else if(addr_ismine(&byi->by_dest))
	    ++sngl_dest;
	else
	{
	    print("bypass_recv: unknown addr");
	    paddr(&byi->by_dest);
	    pmsg(mp);
	    panic("bypass_recv: unknown addr");
	}
        gp = gip->gi_bseqns;
	/*
	 * Catch dups and also let messages through in right order
	 * Various cases here... below, the dup issue need not be
	 * considered and this simplifies the loop.
	 */
        for(n = 0; n < gv->gv_nmemb; n++, bseqn++, gp++)
	{
	    /*
	     *  Can ignore seqn.  from" self. Bypass never sends dups to
	     *  self and there is no issue of "auto causality" here
	     */
            if(n == myrank)
		continue;
	    /*
	     *  From sender: could see out of order or dups
	     */
	    if(n == hisrank)
	    {
	        /*
	         *  Dups never occur in sngl_dest case and here
	         *  seqn isn't incremented before transmission
		 *  else dups detectable because seqn repeats.
	         */
		if(!sngl_dest && *bseqn <= *gp)
		{
#		    if (BYP_VERBOSE)
		        print("... dup 2\n");
#		    endif
		    return; /* dup */
		}
		else if(*bseqn > *gp+1)
		{
	            /* Received out of order from this sender */
	            msg_increfcount(mp);
	            qu_add_mp(gip->gi_bypassq, hisrank, mp, MSG_DELETE);
#		    if (BYP_VERBOSE)
			print("... delay 1 (%d > %d)\n", *bseqn, *gp+1);
#		    endif
	            return;
		}
	    }
	    else if(*bseqn > *gp)
	    {
                /* This message needs to wait for something */
	        msg_increfcount(mp);
	        qu_add_mp(gip->gi_bypassq, hisrank, mp, MSG_DELETE);
#	        if (BYP_VERBOSE)
		    print("... delay 2 (%d > %d)\n", *bseqn, *gp);
#	        endif
	        return;
	    }
	}
        if(sngl_dest)
        {
            isis_gotmsg(mp, BYP_DONTCHECK, 0);
#	    if (BYP_VERBOSE)
		print("... deliver\n");
#	    endif
            return;
        }
        by_makeactive(gip, mp, pn);
	gip->gi_bseqns[hisrank] = byi->by_bseq[hisrank];
	if(byi->by_protocol != CL_ABNEEDSORDER)
	{
#           if (BYP_VERBOSE)
		print("... deliver\n");
#	    endif
	    isis_gotmsg(mp, BYP_DONTCHECK, 0);
	}
	else
	{   
	    msg_increfcount(mp);
	    pg_add_mp(gip->gi_aborder, gip->gi_gaddr, mp, MSG_DELETE);         
#           if (BYP_VERBOSE)
		print("... aborder queue 2\n");
#           endif
	    if(myrank == 0 && !flushing)
	    {
	        ab_setorder(gip->gi_gaddr, mp, gip->gi_bypassview->gv_viewid);
	        ++wantflush;
	    }
	}
        do
        {
	    register qnode *qp, *nqp;
	    gotone = 0;
	    for(qp = gip->gi_bypassq->qu_next; qp != gip->gi_bypassq; qp = nqp)
	    {
	        nqp = qp->qu_next;
	        hisrank = qp->qu_name;
	        mp = qp->qu_msg;
	        byi = (by_info*)msg_getbyi(mp);
	        bseqn = byi->by_bseq;
	        gp = gip->gi_bseqns;
#               if (BYP_VERBOSE)
	            print("[%d]: rechecking [%d %d %d %d] in ", my_process_id, 
		        bseqn[0], bseqn[1], bseqn[2], bseqn[3]);
		    paddr(&byi->by_group);
	            print(" from %d view %d.%d\n", msg_getsender(mp)->addr_process, VMM(byi->by_viewid));
#               endif
	        for(n = 0; n < gv->gv_nmemb; n++)
		    if(n != hisrank)
		    {
		        if(*bseqn++ > *gp++ && n != myrank)
			{
#                           if (BYP_VERBOSE)
				print("... still delayed (%d > %d for rank %d -- mine %d)\n",
				    *--bseqn, *--gp, n, myrank);
#                           endif
		            break;
			}
		    }
		    else
			/* Must be next in line from this sender */
			if(*bseqn++ > *gp++ +1)
			{
#                           if (BYP_VERBOSE)
				print("... still delayed (%d > %d for sender\n",
				    *--bseqn, 1 + *--gp);
#                           endif
			    break;
			}
	        if(n == gv->gv_nmemb)
	        {
		    sngl_dest = addr_ispid(&byi->by_dest);
		    if((gip->gi_bseqns[hisrank] < byi->by_bseq[hisrank]) ||
		       (sngl_dest && gip->gi_bseqns[hisrank] == byi->by_bseq[hisrank]))
		    {
		        ++gotone;
			qu_remove(qp);
			gip->gi_bseqns[hisrank] = byi->by_bseq[hisrank];
#           		if (BYP_VERBOSE)
			    bseqn = byi->by_bseq;
			    print("[%d]: undelay [%d %d %d %d] from %d view %d.%d\n", my_process_id, 
			    bseqn[0], bseqn[1], bseqn[2], bseqn[3], msg_getsender(mp)->addr_process, VMM(byi->by_viewid));
#           		endif
                        if(!sngl_dest)
                            by_makeactive(gip, mp, pn);
			if(byi->by_protocol != CL_ABNEEDSORDER)
			{
#			    if (BYP_VERBOSE)
				print("... deliver\n");
#			    endif
			    isis_gotmsg(mp, BYP_DONTCHECK, 0);
			}
			else
			{   
			    msg_increfcount(mp);
			    pg_add_mp(gip->gi_aborder, gip->gi_gaddr, mp, MSG_DELETE);         
#			    if (BYP_VERBOSE)
				print("... onto aborder queue 3\n");
#			    endif
			    if(myrank == 0 && !flushing) 
			    {
			        if(aq == (qnode*)0)
				    aq = qu_null();
			        msg_increfcount(mp);
			        pg_add_mp(aq, gip->gi_gaddr, mp, MSG_DELETE);         
			    }
			}
		    }
		    else
		    {
#      		        if (BYP_VERBOSE)
			    print("... dup 3\n");
#      		        endif
		    }
		    qu_free(qp);
	        }
	    }
        }
        while(gotone);
	if(aq)
	{
	    register qnode *qp;
	    while(qp = qu_head(aq))
	    {
		register ginfo *gip;
		gip = map_gaddr(&qp->qu_pname);
	        ab_setorder(gip->gi_gaddr, qp->qu_msg, gip->gi_bypassview->gv_viewid);
		++wantflush;
		qu_free(qp);
	    }
	    qu_free(aq);
	}
	if(wantflush && (gip->gi_flag&GI_ABFORKED) == 0)
	{
	    gip->gi_flag |= GI_ABFORKED;
	    t_fork_urgent(ab_flushorder, (VOID*)gip);
	}
  }

/* Executed by new abcast ordering process for this group */
static void ab_flushorder(gip)
  register ginfo *gip;
  {
	register qnode *om;
	while(om = pg_find(order_msg, gip->gi_gaddr))
	{
	    qu_remove(om);
	    cbcast_l("Bm!", gip->gi_gaddr, GENERIC_BYORDER, om->qu_msg, 0);	
	    qu_free(om);
	}
	gip->gi_flag &= ~GI_ABFORKED;
  }

static void ab_setorder(gaddr, mp, vid)
  register address *gaddr;
  register message *mp;
  {
	register qnode *om = pg_find(order_msg, gaddr);
	if(om == (qnode*)0)
	    om = pg_add_mp(order_msg, gaddr, msg_newmsg(), MSG_DELETE);
	msg_put(om->qu_msg, "%A[1],%d,%d", msg_getsender(mp), msg_getid(mp), vid);
  }

/* Got abcast order */
void bypass_aborder(mp)
  register message *mp;
  {
	register qnode *qp;
	register address *who; address *Who;
	register ginfo *gip;
	register uid; int Uid, Vid;
	if(!msg_getbyi(mp) || (gip = map_gaddr(&msg_getbyi(mp)->by_group)) == (ginfo*)0)
	    return;
#if     (BYP_VERBOSE)
	    print("[%d]:: ", my_process_id); pmsg(mp);
#endif  (BYP_VERBOSE)
	while(msg_get(mp, "%-A[1],%d,%d,%d", &Who, &Uid, &Vid) == 3)
	{
	    register qnode *nqp;
	    who = Who; uid = Uid;
	    for(qp = gip->gi_aborder->qu_next; qp != gip->gi_aborder; qp = nqp)
	    {
	        register message *msg = qp->qu_msg;
		nqp = qp->qu_next;
	        if(msg_getid(msg) == uid && addr_isequal(msg_getsender(msg), who))
	        {
		    qu_remove(qp);
		    isis_gotmsg(msg, BYP_DONTCHECK, 0);
		    qu_free(qp);
		    goto next;
	        }
	    }
#if         (BYP_VERBOSE)
		/* Only during flush due to sort-order deliveries */
	        print("[%d]- From ", my_process_id); paddr(who);
	        print(", abcast %d ", uid);
	        paddr(gip->gi_gaddr);
	        print(", vid %d.%d *** NOT FOUND ***\n", VMM(Vid));
	        continue;
	    next:
		/* This is the normal case: found and delivered the abcast */
	        print("[%d]+ From ", my_process_id); paddr(who);
	        print(", abcast %d ", uid);
	        paddr(gip->gi_gaddr);
	        print(", vid %d.%d *** FOUND ***\n", VMM(Vid));
	        continue;
#else       (BYP_VERBOSE)
	  next: continue;
#endif      (BYP_VERBOSE)
	}
  }

/* Sender informs me: ``ok to delete up through ....'' */
bypass_gcollect(qp, gip, sender, seqn)
  register ginfo *gip;
  address *sender;
  register seqn;
  register qnode *qp;
  {
        register qnode *np, *nnp;
        qp->qu_gseqn = seqn;
#if     (BYP_VERBOSE)
	    print("bypass_gcollect: sender %d seqn %d\n", sender->addr_process, seqn);
#endif  (BYP_VERBOSE)
        for(np = qp->qu_queue->qu_next; np != qp->qu_queue && np->qu_mseqn <= seqn; np = nnp)             
        {
	    nnp = np->qu_next;
            qu_remove(np);
	    if(np->qu_flag)
	        make_inactive(gip, A_RCVBCAST);
	    qu_free(np);
        }
  }

by_makeactive(gip, mp, pn)
  register ginfo *gip;
  register message *mp;
  {
	register address *sender;
        register qnode *qp, *mqp;
	sender = msg_getsender(mp);
	if(addr_ismine(sender))
            return;
	if(qp = pg_find(gip->gi_byprocs, sender))
	    for(mqp = qp->qu_queue->qu_next; mqp != qp->qu_queue; mqp = mqp->qu_next)
	        if(mqp->qu_msg == mp)
	        {
	            ++mqp->qu_flag; /* ``Made active'' */
		    make_active(gip, A_RCVBCAST, pn);
                    break;
	        }
  }

static  want_do_check;
condition want_makeactive;

/* Active group is equivalent to non-empty pbuf */
do_check_pbuf(nwant)
  {
	if(active_count == 0)
	    return;
        ++want_do_check;
	if(nwant == 0)
	    isis_state |= ISIS_DRAINPBUF;
	do
            t_wait_l(&want_active, "do_check_pbuf:want_active");
	while(active_count);
        if(isis_state&ISIS_DRAINPBUF)
	    isis_state &= ~ISIS_DRAINPBUF;
        if(--want_do_check == 0)
	    t_sig_all(&want_makeactive, 0);
  }

make_active(gip, how, pn)
  register ginfo *gip;
  register how;
  {	
        register ginfo *agip = gip;
        /* For fairness.... */
	if(isis_ctp != isis_scheduler)
	{
            if(want_do_check)
                t_wait_l(&want_makeactive, "make_active:want_makeactive");
            if(t_waiting(&want_active))
                t_wait_l(&want_active, "make_active:want_active");
	}
	if(gip->gi_flag == 0)
	    panic("group deleted while doing bypass-send to it!");
	/*
	 * OPTIMISTIC is slightly risky.  Ignores termination rule... risks
	 * causality violations.  Provided only until pg_client runs in
         * BYPASS mode.  Use with caution...
	 */
	if(isis_state&ISIS_OPTIMISTIC)
	{
	    register was_active = gip->gi_activebsnd+gip->gi_activemsg+gip->gi_activebrcv;
            switch(how)
	    {
              case A_SNDBCAST:
	        ++gip->gi_activebsnd;
		break;
              case A_SNDMSG:
	        ++gip->gi_activemsg;
		break;
              case A_RCVBCAST:
	        ++gip->gi_activebrcv;
		break;
	     }
             gip->gi_transport = pn;
             if(was_active)
                 return;
             if(++active_count == 1)
                 active_gip = gip;
	     else
                 active_gip = 0;
	     return;
	}
        forever
	{
	  if(gip->gi_flag == 0)
	    panic("group deleted while doing bypass-send to it!");
	  switch(how)
	  {
            case A_SNDBCAST:
	      if(active_count == 0)
	      {
	          ++active_count;
	          ++gip->gi_activebsnd;
                  gip->gi_transport = pn;
	          active_gip = gip;
	          return;
	      }
	      if(active_count == 1 && gip == active_gip && gip->gi_activemsg == 0)
	      {
	          ++gip->gi_activebsnd;
	          return;
	      }
	      t_wait_l(&want_active, "bypass: make_active (sndbcast)");
	      continue;
            case A_SNDMSG:
	      if(active_count == 0)
	      {
	          ++active_count;
	          ++gip->gi_activemsg;
                  gip->gi_transport = pn;
	          active_gip = gip;
	          return;
	      }
	      if(active_count == 1 && gip == active_gip)
	      {
	          ++gip->gi_activemsg;
                  gip->gi_transport = pn;
	          return;
	      }
	      t_wait_l(&want_active, "bypass: make_active (sndmsg)");
	      continue;
  
            case A_RCVBCAST:
	      if(active_count == 0)
	      {
	          ++active_count;
	          ++gip->gi_activebrcv;
                  gip->gi_transport = pn;
	          active_gip = gip;
	          return;
	      }
	      if(active_count == 1 && gip == active_gip)
	      {
	          ++gip->gi_activebrcv;
                  gip->gi_transport = pn;
	          return;
	      }
	      active_gip = 0;
	      if(gip->gi_activebsnd+gip->gi_activemsg+gip->gi_activebrcv == 0)
	          ++active_count;
	      ++gip->gi_activebrcv;
	      gip->gi_transport = pn;
	      return;
          }
        }
  }

void
bypass_inactivate(gip)
  register ginfo *gip;
  {
        if(--active_count == 0)
	    active_gip = 0;
        else if(active_count == 1)
        {
	    for(gip = isis_groups; gip; gip = gip->gi_next)
	        if(gip->gi_activebsnd+gip->gi_activemsg+gip->gi_activebrcv)
	        {
		    active_gip = gip;
		    break;
	        }
        }
        t_sig_all(&want_active, 0);
  }

static term_scheduled;

make_inactive(gip, how)
  register ginfo *gip;
  register how;
  {
	if((gip->gi_flag&GI_INUSE) == 0)
	    return;
        switch(how)
        {
          case A_SNDBCAST:
	    if(--gip->gi_activebsnd < 0)
		panic("inactive: sndbcast");
	    break;
          case A_SNDMSG:
	    if(--gip->gi_activemsg < 0)
		panic("inactive: sndmsg");
	    break;
          case A_RCVBCAST:
	    if(--gip->gi_activebrcv < 0)
		panic("inactive: rcvbcast");
	    break;
        }
        if(gip->gi_activebsnd+gip->gi_activemsg+gip->gi_activebrcv == 0)
	    bypass_inactivate(gip);
	if(how == A_SNDBCAST && gip->gi_activebsnd == 0 && (gip->gi_flag&GI_FLUSHING) == 0)
        {
	    void bypass_sendterminate();
            register qnode *qp;
            for(qp = term_queue->qu_next; qp != term_queue; qp = qp->qu_next)
                if(qp->qu_gip == gip)
		{
		    qp->qu_name = gip->gi_myseqn;
                    return;
		}
            qp = qu_add_gip(term_queue, gip->gi_myseqn, gip);
	    if(term_scheduled++ == 0)
	        (void)isis_ondrain(bypass_sendterminate, 0);
        }

  }

void
bypass_sendterminate()
  {
        register qnode *qp;
	force_lazy(TRUE);
        while(qp = qu_head(term_queue))
        {
            address dests[PG_ALEN];
	    register term_seqn, n;
            register message *mp;
            register address *ap, *dp;
            register groupview *gv;
            register ginfo *gip;
            static by_info binfo;
            register by_info *byi;
            gip = qp->qu_gip;
	    term_seqn = qp->qu_name;
            qu_free(qp);
            gv = gip->gi_bypassview;
	    if((gip->gi_flag&GI_INUSE) == 0 || gv == 0 || term_seqn <= gip->gi_bcseqn)
		continue;
            mp = msg_gen("%d,%A[1]", gip->gi_myseqn, gip->gi_gaddr);
            ap = dests;
            dp = gv->gv_members;
            while(!aptr_isnull(dp))
            {
                *ap = *dp++;
                ap->addr_entry = GENERIC_BYINACTIVE;
                ++ap;
            }
            *ap = NULLADDRESS;
            msg_setdests(mp, dests);
            msg_setbyi(mp, &binfo);
            byi = msg_getbyi(mp);
            byi->by_group = *gip->gi_gaddr;
            byi->by_dest = byi->by_group;
            byi->by_dest.addr_entry = GENERIC_BYINACTIVE;
            byi->by_viewid = gv->gv_viewid;
            byi->by_myseqn = gip->gi_myseqn;
	    n = gip->gi_transport;
  	    while((*by_transport[n])(gip->gi_gaddr, TRUE, mp, dests, NULLROUTINE, NULLARG, NULLARG) == -1 && n)
		n = 0;
            msg_delete(mp);
        }
	force_lazy(FALSE);
	term_scheduled = 0;
  }

void
bypass_inactive(mp)
  register message *mp;
  {
        address gaddr;
	register address *sender = msg_getsender(mp);
        register qnode *qp;
        register ginfo *gip;
        int seqn;
        msg_get(mp, "%d,%a", &seqn, &gaddr);
        gip = map_gaddr(&gaddr);
#       if (BYP_VERBOSE)
	    print("bypass_inactive: group "); paddr(&gaddr);
	    print(" sender "); paddr(sender);
	    print(" terminate through %d\n", seqn);
#       endif
        if(gip == (ginfo*)0)
            panic("bypass inactive: unknown gaddr");
	if((qp = pg_find(gip->gi_byprocs, sender)) && qp->qu_gseqn < seqn)
	    bypass_gcollect(qp, gip, sender, seqn);
  }

void
bypass_del_pgroup(gip)
  register ginfo *gip;
  {
	register qnode *qp;
	register groupview *bv = gip->gi_protosview;
	if(bv == 0)
	    return;
	bv->gv_nmemb = 0;
	bv->gv_departed = NULLADDRESS;
	intercl_newview(0, bv);
	if(gip->gi_activebsnd+gip->gi_activemsg+gip->gi_activebrcv)
	{
	    gip->gi_activebsnd = gip->gi_activemsg = gip->gi_activebrcv = 0;
	    bypass_inactivate(gip);
	}
	while(qp = qu_head(gip->gi_byprocs))
	    qu_free(qp);
	while(qp = qu_head(gip->gi_bypassq))
	{
#	    if (BYP_VERBOSE)
		print("*** bypass_del_pgroup! "); pmsg(qp->qu_msg);
#	    endif
	    qu_free(qp);
	}
	while(qp = qu_head(gip->gi_aborder))
	    qu_free(qp);
  }

/* First chance to check: when message received from protos */
void
bypass_precheck(mp, entry)
  register message *mp;
  {
        register sys_groupview *npg;
        register ginfo *gip;
	register groupview *bv;
        groupview *isis_gv_alloc();
	int first_view = 0;

	if(msg_getpbflag(mp))
	    pbuf_dirty();
	if(entry != GENERIC_NEW_VIEW)
	    return;
	if((npg = (sys_groupview*)msg_getfield(mp, CL_NEWVIEW, 1, (int*)0)) == (sys_groupview*)0)
	    return;
	gip = add_group(npg->pg_name, &npg->pg_gid);
	bv = gip->gi_protosview;
	if(bv == 0)
	{
	    ++first_view;
	    gip->gi_protosview = bv = isis_gv_alloc();
	}
	(void)isis_pg_copy(npg, bv);
	intercl_newview(first_view, bv);
	if(!addr_isnull(&bv->gv_departed))
	{
	    register qnode *qp;
	    register message *failed = (message*)0;
	    for(qp = isis_tasks->qu_next; qp != isis_tasks; qp = qp->qu_next)
	    {
		register task *tp = qp->qu_task;
		if(tp->task_queue == &tp->task_mwant && tp->task_sentto.addr_process != PROTOCOLS)
		{
		    if(failed)
			msg_increfcount(failed);
		    else
		    {
			failed = msg_newmsg();
			msg_setsender(failed, &bv->gv_departed);
			msg_insertfield(failed, FLD_ISNULLREP, 0, 0, 0);
		    }
		    t_sig_urgent(&tp->task_mwant, (void*)failed);
		}
	    }
	}
  }

void
bypass_checkview(mp)
  message *mp;
  {
	register qnode *qp;
	register address *ap;
        register groupview *bv;
	register departed, my_rank;
        register sys_groupview *npg;
	groupview *isis_gv_alloc();
        register ginfo *gip;

        npg = (sys_groupview*)msg_getfield(mp, CL_NEWVIEW, 1, NULLIARG);
        gip = add_group(npg->pg_name, &npg->pg_gid);
	start_flush(gip, "bypass_checkview:start_flush");
        bv = gip->gi_bypassnew;
	if(bv == 0)
	    gip->gi_bypassnew = bv = isis_gv_alloc();
        else if(bv->gv_viewid >= VIEWID(npg->pg_viewid, 0))
	    goto done;
        (void)isis_pg_copy(npg, bv);
	if(bv->gv_nmemb == 0)
	    goto done;
	/* Compute new rank */
	ap = bv->gv_members;
	while(!aptr_isnull(ap) && !addr_ismine(ap))
	    ++ap;
	if(!aptr_isnull(ap))
	    my_rank = ap-bv->gv_members;
	else
	    goto done;

	/* Always block before flushgroup */
	isis_state |= ISIS_FBLOCK;
	++isis_nblock;

	/* Send flush messages, wait for others to flush */
        bypass_flushgroup(gip, bv->gv_viewid, bv->gv_members, mp);

	if(gip->gi_bypassview == 0)
	    gip->gi_bypassview = isis_gv_alloc();
        isis_pg_copy(npg, gip->gi_bypassview);
	gip->gi_bseqns[bv->gv_nmemb] = 0;
	begin
	{
	    register i;
	    for(i = 0; i != NTRANSPORT; i++)
	        if(by_newview[i])
		    (*by_newview[i])(gip);
	}
  done:
	(void)isis_ondrain(done_flush, 0);
  }

/* Got a gbcast, forces flush in any group(s) this overlaps */
void
by_flushfirst(mp)
  register message *mp;
  {
	register ginfo *gip;
	register address *ap, *bp;
	register address *pp;
        ++isis_nblock;
	isis_state |= ISIS_FBLOCK;
	for(gip = isis_groups; gip; gip = gip->gi_next)
	{
	    register groupview *gv;
	    start_flush(gip, "got_gbcast:start_flush");
	    gv = gip->gi_bypassnew;
	    pp = msg_getdests(mp);
	    while(!aptr_isnull(pp))
	    {
		if(pg_ismemb(gv, pp))
		{
		    ++isis_nblock;
		    bypass_flushgroup(gip, gv->gv_viewid+1, msg_getdests(mp), mp);
		    ++gip->gi_bypassview->gv_viewid;
		    break;
		}
	        ++pp;
	    }
	}
	if(--isis_nblock == 0)
	{
	    isis_state &= ~ISIS_FBLOCK;
	    protos_despool();
	}
  done:
	(void)isis_ondrain(done_flush, 0);
  }

/* Always called with FBLOCK set */
bypass_flushgroup(gip, viewid, participants, blockmsg)
  register ginfo *gip;
  register address *participants;
  message *blockmsg;
  {
        register n = 0, myseqn = -1;
        register message *mp;
	register address *ap, *bp;
	int waited = 0;
	address *senders = participants;
	register groupview *bv = gip->gi_bypassnew;
	/* Prevents confusion when intercl_flushwant is called */
	++flushing;
	force_lazy(TRUE);
	gip->gi_flushwant = 9999;
	gip->gi_flushcount = 0; 
	gip->gi_flushviewid = viewid; 
        gip->gi_blockmsg = blockmsg;
	bv->gv_viewid = viewid;
        for(ap = senders; !aptr_isnull(ap); ap++, n++)
	{
	    if(senders != bv->gv_members)
	    {
	        for(bp = bv->gv_members; !aptr_isnull(bp); bp++)
		    if(addr_isequal(ap, bp))
			break;
	        if(aptr_isnull(bp)) 
		    continue;
	    }
            ++gip->gi_flushwant;
	    if(addr_ismine(ap))
	        myseqn = gip->gi_bseqns[n];
	    else if(intercl_wantflush(ap, gip->gi_gaddr, viewid) == -1)
	        /* Zombie! Dead men tell no lies */
	        ++gip->gi_flushcount;
	}
	gip->gi_flushwant -= 9999;
	/* First push through unterminated messages, if any */
	begin
	{
	    register qnode *qp;
	    for(qp = gip->gi_byprocs->qu_next; qp != gip->gi_byprocs; qp = qp->qu_next)
	    {
		register qnode *np;
		while(np = qu_head(qp->qu_queue))
		{
		    register message *msg;
		    msg = np->qu_msg;
                    qu_remove(np);
		    if(bv->gv_nmemb > 1)
		    {
			register address *dlist, *ap;
			address dests[PG_ALEN];
#			if (BYP_VERBOSE)
			register by_info *byi;
			register *bseqn;
			byi = msg_getbyi(msg);
			bseqn = byi->by_bseq;
			print("[%d]: send copy of [%d %d %d %d] from %d view %d.%d\n", my_process_id, 
			    bseqn[0], bseqn[1], bseqn[2], bseqn[3], msg_getsender(msg)->addr_process,
			    VMM(byi->by_viewid));
#			endif
			/*
			 * Don't send dups to self or back to sender... 
			 * Inefficient to send to self and sender assumes
			 * we won't send back to it (in bypass_recv)
			 */
			ap = msg_getdests(msg);
			dlist = dests;
			while(!aptr_isnull(ap))
			{
			    if(!addr_isequal(ap, msg_getsender(msg)) && !addr_ismine(ap))
				*dlist++ = *ap;
			    ap++;
			}
			if(dlist != dests)
			{
			    *dlist = NULLADDRESS;
			    (*by_transport[gip->gi_transport])(gip->gi_gaddr, TRUE,
			        msg, dests, NULLROUTINE, NULLARG, NULLARG);
			}
		    }
		    if(np->qu_flag)
			make_inactive(gip, A_RCVBCAST);
		    qu_free(np);
		}    
	    }    
	}
        if(myseqn != -1)
        {
	    mp = msg_gen("%A[1],%A[1],%d,%d", gip->gi_gaddr,
		&my_address, viewid, myseqn);
	    ap = msg_setdests(mp, participants);
	    if(participants != bv->gv_members)
	    {
		while(!aptr_isnull(ap))
		{
	            for(bp = bv->gv_members; !aptr_isnull(bp); bp++)
		        if(addr_isequal(ap, bp))
			    break;
		    if(!aptr_isnull(bp))
			*bp++ = *ap++;
		    else
			++ap;
		}
		ap = msg_getdests(mp);
	    }
	    ap = msg_getdests(mp);
#           if (BYP_VERBOSE)
               print("Sending FLUSH messages for view %d.%d\n", VMM(viewid));
#           endif
	    while(!aptr_isnull(ap))
	        (ap++)->addr_entry = GENERIC_BYFLSH;
	    (*by_transport[0])(gip->gi_gaddr, 1, mp, msg_getdests(mp),
		NULLROUTINE, NULLARG, NULLARG);
	    isis_gotmsg(mp, BYP_DONTCHECK, 0);
	    msg_delete(mp);
        }
	/* Flush also takes care of termination */
	begin
	{
	    register qnode *qp;
            for(qp = term_queue->qu_next; qp != term_queue; qp = qp->qu_next)
                if(qp->qu_gip == gip)
		{
		    qu_free(qp);
                    break;
		}
	}
	force_lazy(FALSE);
	/* Now wait for flush to finish */
	if(gip->gi_flushcount < gip->gi_flushwant)
	{
	    if((int)t_wait_l(&gip->gi_bypasswait, "bypass_checkview:do flush") == -1)		panic("left group during flush protocol");
	}
	else if(gip->gi_flushcount == gip->gi_flushwant)
	    gip->gi_flushwant = -1;
	/*
	 * This is the key point in the code.  Flush is finished.  Reset
         * everything to the initial state 
	 */
	begin
	{
	    register qnode *qp;
	    gip->gi_bcseqn = gip->gi_myseqn = gip->gi_nextseqn = 0;
	    bzero(gip->gi_bseqns, sizeof(gip->gi_bseqns));
            if(gip->gi_activebsnd+gip->gi_activemsg+gip->gi_activebrcv)
	    {
	        gip->gi_activebsnd = gip->gi_activemsg = gip->gi_activebrcv = 0;
                for(qp = term_queue->qu_next; qp != term_queue; qp = qp->qu_next)
                    if(qp->qu_gip == gip)
		    {
			qu_free(qp);
			break;
		    }
		bypass_inactivate(gip);
	    }
	    while(qp = qu_head(gip->gi_byprocs))
	        qu_free(qp);
	    while(qp = qu_head(gip->gi_bypassq))
	    {
#		if (BYP_VERBOSE)
		    register message *mp;
		    register by_info *byi;
		    register *bseqn;
		    mp = qp->qu_msg;
		    byi = msg_getbyi(mp);
		    bseqn= byi->by_bseq;
		    print("[%d]: discard [%d %d %d %d] from %d view %d.%d\n", my_process_id, 
		        bseqn[0], bseqn[1], bseqn[2], bseqn[3], msg_getsender(mp)->addr_process,
		        VMM(byi->by_viewid));
#		endif
		qu_free(qp);
	    }
	}
	/* Now can deliver any delayed abcast messages in sorted order */
	if(gip->gi_bypassview)
	{
	    register qnode *qp;
	    for(ap = gip->gi_bypassview->gv_members; !aptr_isnull(ap); ap++)
	    {
		register qnode *nqp;
	        for(qp = gip->gi_aborder->qu_next; qp != gip->gi_aborder; qp = nqp)
	        {
	            register message *msg = qp->qu_msg;
		    nqp = qp->qu_next;
	            if(addr_isequal(msg_getsender(msg), ap))
	            {
		        qu_remove(qp);
		        isis_gotmsg(msg, BYP_DONTCHECK, 0);
		        qu_free(qp);
	            }
	        }
	    }
	}
	if(qu_head(gip->gi_aborder))
	    panic("aborder queue not empty after flushgroup");
	if(--isis_nblock == 0)
	{
	    isis_state &= ~ISIS_FBLOCK;
	    protos_blockmsg = gip->gi_blockmsg;
	    protos_despool();
	}
	intercl_unblock(gip->gi_bypassnew, gip->gi_gaddr);
	if(--flushing == 0 && t_waiting(&wait_flush))
	    t_sig_all(&wait_flush, 0);
  }

void
bypass_unblock(gip)
  register ginfo *gip;
  {
	if(t_waiting(&gip->gi_bypasswait))
	    t_sig_all(&gip->gi_bypasswait, 0);
  }

static void
bypass_probe(mp)
  register message *mp;
  {
	reply(mp, "");
  }

/* Reception of flush messages */
void
bypass_flush(mp)
  register message *mp;
  {
	address gaddr, addr;
	int vid = -1, hisseqn = -1;
	register ginfo *gip;
        msg_get(mp, "%a,%a,%d,%d", &gaddr, &addr, &vid, &hisseqn);
        gip = map_gaddr(&gaddr);
        if(gip == (ginfo*)0)
	    panic("bypass flush: unknown gaddr");
#       if (BYP_VERBOSE)
           if(vid != -1)
               print("Got flush %d.%d from ", VMM(vid));
           else
               print("Got flush *.* (%d.%d) from ", VMM(gip->gi_flushviewid));
           paddr(&addr);
           print(", will be %d of %d wanted\n", gip->gi_flushcount+1, gip->gi_flushwant);
#       endif
	if(vid == -1 || vid == gip->gi_flushviewid)
	{
	    register address *ap;
	    if(gip->gi_bypassview == 0)
	    {
	        register n = 0;
	        for(ap = gip->gi_bypassnew->gv_members; !aptr_isnull(ap); ap++, n++)
		    if(addr_cmp(ap, &addr) == 0)
		        break;
	        if(aptr_isnull(ap))
		    panic("bypass_flush");
	    }
	    if(++gip->gi_flushcount == gip->gi_flushwant)
	    {
	        gip->gi_flushwant = -1;
                if(isis_ctp == isis_scheduler)
	            t_fork((vfunc*)bypass_unblock, (VOID*)gip);
	        else 
	            bypass_unblock(gip);
	    }
	}
	else
	{
             print("ISIS client ");
             paddr(&my_address);
             print(" got unexpected flush message vid %d.%d bypassvid %d.%d\n",VMM(vid), VMM(gip->gi_flushviewid));
	}
  }

message *
collect_replies(msgid, dests, nwant, exmode)
  address *dests;
  register nwant;
  {
        register message *rmp = msg_newmsg();
	register nsent = alist_len(dests), nreplies;
	register address *ap, *who, *fwd;
	register n;
	int *errno = (int*)0;
	char done[ADDR_LEN], *dp;
	bitvec sent_to;

        isis_ctp->task_msgid = msgid;
	isis_nsent = nsent;
	if(nwant == MAJORITY)
	    nwant = nsent/2+1;
	if(nwant > nsent)
	   nwant = nsent;
	bclr(&sent_to);
	nreplies = 0;
	isis_ctp->task_nwant = nwant;
	isis_ctp->task_nsent = nsent;
	isis_ctp->task_nreplies = isis_ctp->task_nullreps = 0;
	isis_ctp->task_dests = dests;
	isis_ctp->task_done = done;
	dp = &done[nsent];
	*dp = 0;
	while(dp != done)
	    *--dp = 'W';
	if(exmode)
	{
	    n = 0;
	    for(ap = dests; !aptr_isnull(ap); ap++, n++)
		if(addr_ismine(ap))
		{
		    bis(&sent_to, n);
		    done[n] = 'N';
		    ++isis_ctp->task_nullreps;
		    --nsent;
		}
	}
	/* nsent counts the number who could still send a reply */
        while(nreplies < nwant && nsent > 0)
        {
            register message *mp;
            mp = (message*)t_wait_l(&isis_ctp->task_mwant, "collect-replies");
	    if(errno = (int*)msg_getfield(mp, FLD_ISABORTREP, 1, (int*)0))
	    {
	        msg_delete(mp);
		break;
	    }
	    who = msg_getsender(mp);
	    n = -1;
	    for(ap = dests; !aptr_isnull(ap); ap++)
	        if(addr_isequal(ap, who))
		{
		    n = ap-dests;
		    if(bit(&sent_to, n) == 0)
		    {
			bis(&sent_to, n);
			--nsent;
			ap = 0;
		    }
		    else
			n = -1;
		    break;
		}
	    if(ap || msg_getfield(mp, FLD_ISNULLREP, 1, (int*)0))
	    {
	        if(n != -1)
		{
		    done[n] = 'N';
		    ++isis_ctp->task_nullreps;
		}
		msg_delete(mp);
		continue;
	    }
	    else if(fwd = msg_getforwarder(mp))
	    {
	        /* Forwarded message */
		register sview *sv = site_getview();
		if(sv->sv_incarn[fwd[FWI_NEWDEST].addr_site] == fwd[FWI_NEWDEST].addr_incarn)
	        {
		    dests[n] = fwd[FWI_NEWDEST];
		    done[n] = '*';
		    break;
	        }
	        else
	        {
		    done[n] = 'D';
		    ++isis_ctp->task_nullreps;
		}
	    }
	    ++nreplies;
	    ++isis_ctp->task_nreplies;
	    done[n] = 'R';
	    msg_addmsg(rmp, FLD_ANSW, mp);
	    msg_delete(mp);
        }
	isis_ctp->task_dests = 0;
        isis_ctp->task_msgid = 0;
	if(errno)
	{
	    msg_delete(rmp);
	    isis_errno = *errno;
	    isis_nreplies = 0;
	    return((message*)-1);
	}
	isis_nreplies = nreplies;
        return(rmp);
  }

/* May delay for a while before returning */
isis_querydead(who)
  register address *who;
  {
        register message *mp, *rmsg;
        address dest;
	register got_answ = 1;
        ISIS_ENTER();
        mp = msg_newmsg();
        dest = *who;
	dest.addr_entry = GENERIC_BYPROBE;
        rmsg = protos_rpc(&dest, mp, 0);
        msg_delete(mp);
	if(rmsg)
	{
	    if(msg_getfield(rmsg, FLD_ISNULLREP, 1, (int*)0))
		got_answ = 0;
            msg_delete(rmsg);
	}
	else
	    got_answ = 0;
        ISIS_EXIT();
	return(got_answ);
  }

void
by_dump()
  {
        register ginfo *gip;
	register qnode *qp, *np;
	for(gip = isis_groups; gip; gip = gip->gi_next)
            if(qu_head(gip->gi_byprocs))
  	        break;
	print("\nBypass procs: %d active groups\n", active_count);
	if(gip)
	{
	    for(gip = isis_groups; gip; gip = gip->gi_next)
            {
                print("  Group "); paddr(gip->gi_gaddr);
                if(gip->gi_activebsnd+gip->gi_activemsg+gip->gi_activebrcv)
                    print("<< active: bsnd %d msg %d brcv %d >>", gip->gi_activebsnd, gip->gi_activemsg, gip->gi_activebrcv);
		print("\n");
                if(qu_head(gip->gi_bypassq))
		    print("Bypass delay queue:\n");

                for(qp = gip->gi_bypassq->qu_next; qp != gip->gi_bypassq; qp = qp->qu_next)
		{
		    register message *mp = qp->qu_msg;
		    register by_info *byi;
	  	    register *b;
		    print("  From "); paddr(&gip->gi_bypassview->gv_members[qp->qu_name]);
		    byi = msg_getbyi(mp);
		    b = byi->by_bseq;
		    print(", VT(msg)=[%d %d %d %d...]", b[0], b[1], b[2], b[3]);
		    b = gip->gi_bseqns;
		    print(", VT(g)=[%d %d %d %d...]\n", b[0], b[1], b[2], b[3]);
		}
                for(qp = gip->gi_byprocs->qu_next; qp != gip->gi_byprocs; qp = qp->qu_next)
	        {
                    print("Sender ");
                    paddr(&qp->qu_pname);
                    print("... garbage collected through seqn %d.\n", qp->qu_gseqn);
		    for(np = qp->qu_queue->qu_next; np != qp->qu_queue; np = np->qu_next)
		        print("  Retained copy of msg seqn=%d, flag=%d, mp=%x view %d.%d\n", np->qu_mseqn, np->qu_flag, np->qu_msg, VMM(msg_getbyi(np->qu_msg)->by_viewid));
	        }
	        print("Abcast ordering queue:\n");
	        for(qp = gip->gi_aborder->qu_next; qp != gip->gi_aborder; qp = qp->qu_next)
	        {
	            register message *msg = qp->qu_msg;
	            print("  "); pmsg(msg);
	        }
            }
	}
	bc_dump();
	if(qu_head(term_queue))
	{
	    print("Terminate queue\n");
	    for(qp = term_queue->qu_next; qp != term_queue; qp = qp->qu_next)
	    {
                print("Group ");
	        paddr(qp->qu_gip->gi_gaddr);
	        print(" terminate seqn %d\n", qp->qu_name);
	    }
	}
  }

#else
void bypass_init()
  {
        isis_entry(GENERIC_BYFLSH, bypass_flush, "bypass_flush");
  }

message	*
bypass_send(protocol, exmode, dests, msg, nwant, flag, pn)
  int protocol;
  int exmode;
  address *dests;
  message *msg;
  int nwant;
  int flag;
  int pn;
  { return(0); }

void
bypass_precheck(mp, entry)
  message *mp;
  int entry;
  { }

void
bypass_checkview(mp)
  message *mp;
  {
	isis_gotmsg(mp, BYP_DONTCHECK, 0);
  }

void
bypass_inactive(mp)
  message *mp;
  {}

void
bypass_unblock(gip)
  ginfo *gip;
  {}

void
bypass_flush(mp)
  message *mp;
  {
	panic("FATAL ERROR: mixed BYPASS and NON BYPASS group members!");
  }


void
bypass_recv(byi, mp, pn)
  by_info *byi;
  message *mp;
  int pn;
  { }

void
by_dump()
  { }

void
pbuf_dirty()
  { }

void
by_flush()
  { }

void
by_flushfirst(mp)
  message *mp;
  {
	isis_gotmsg(mp, BYP_DONTCHECK, 0);
  }

void
bypass_del_pgroup(gip)
  register ginfo *gip;
  { }

transport_lookup(pname)
  char *pname;
  {  }

bypass_sendterminate()
  {  }

void 
bypass_aborder(mp)
  message *mp;
  {}

void
bypass_inactivate(gip)
  ginfo *gip;
  { }

void
waitq_free(qp)
  qnode *qp;
  { }

#endif
