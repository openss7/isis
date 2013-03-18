/*  $RCSfile: cl_join.c,v $ $Revision: 2.108 $ $Date: 90/09/14 13:19:18 $  */
/*
 *	Originally coded by Ken Birman
 *      State transfer toolkit utility 
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


#define   ISIS_SYS
# include "isis.h"
# include "spooler.h"

saddr   my_addr;                /* Set in cl_isis.c */

typedef struct
{
        address gaddr;
        int     domain;
        int     locator;
} x_where;

#define FTYPE_WHERE     FTYPE_LONG

static  void xf_where();
static  void pg_join_req(), pg_client_req(), xfer_send();
static  void pg_join_action(), pg_client_action();
static  address *pg_create();
static  address join_gaddr;
static  xfer_sock; 
static  xfer_in_progress;
static  condition xfer_done;
condition       wants_to_run_after_join;

void
join_init()
  {
        isis_entry(GENERIC_JOIN_REQ, (vfunc *) pg_join_req,
                   "cl_join:pg_join_req");
        isis_entry(GENERIC_CLIENT_REQ, (vfunc *) pg_client_req,
                   "cl_join:pg_client_req");
        isis_entry(GENERIC_XFER_STATE, (vfunc *) xfer_rcv_small,
                   "cl_join:xfer_rcv_small");
        isis_entry(GENERIC_XFER_WHERE, (vfunc *) xf_where, "cl_join:xf_where");
        isis_task((vfunc *) xfer_send, "cl_join:xfer_send");
        isis_task((vfunc *) pg_join_action, "cl_join:pg_join_action");
        isis_task((vfunc *) pg_client_action, "cl_join:pg_client_action");
  }

/* Join the group arranging for a state transfer too */
address* pg_dojoin();

address*
pg_join(va_alist)
  va_dcl
  {
        register address* rval;
        va_list ap;
        va_start(ap);
        rval = pg_dojoin(&ap);
        va_end(ap);
        return(rval);
  }

#ifndef MACH
static char *imsg = "**** subroutine/function parameter not declaraed external";
#define  is_ifunc(i) { if((isis_state&ISIS_XBYREF) && ((char**)i) && *((char**)i) == 0) panic(imsg); }
#else
#define  is_ifunc(i)
#endif MACH

static	coldstart = L_WARM;
static	ignoreold = L_ERROR;

log_coldstart(how)
  {
	coldstart = how? L_COLD: L_WARM;
  }

log_ignoreold(how)
  {
	ignoreold = how? L_COLD: L_ERROR;
  }

address*
pg_dojoin(ap)
  va_list *ap;
  {
        char *gname;
        register groupview *gv;
        vfunc *pg_init_routine = NULLROUTINE, *pg_monitor_routine = NULLROUTINE;
        int old_state, old_act, code, recover = 0, replayspool = 0;
        char *pg_monitor_arg;
        int retry, dont_create = 0, incarn = 0, niter = 0, logged = 0, diffusion = 0;
        int cl_wid = 0, nmbuf = 0, nmdsk = 0, lflen = 0, lentry, flush_when;
	ifunc *log_routine;
	unsigned timer;
        char *credentials = "", *logfname;
        message *mp;
        address *gaddr, *adp;
        register ginfo *gip;

	ISIS_ENTER();
        gname = VA_REF(*ap, char*);
        while(isis_joining)
            t_wait(&wants_to_run_after_join);
        isis_joining = gname;
        gip = add_gname(gname);
        isis_errno = 0;
	code = VA_ARG(*ap, int);
        if(code) do
        {
            switch(code)
            {
              case PG_INIT:
                pg_init_routine = VA_REF(*ap, vfunc*);
                is_ifunc(pg_init_routine);
                break;
              case PG_MONITOR:
                if(pg_monitor_routine != NULLROUTINE)
                {
                    isis_errno = IE_BADARG;
                err:group_unmap(gip);
		    isis_joining = 0;
                    t_sig_all(&wants_to_run_after_join, 0);
                    ISIS_RETURN(&NULLADDRESS);
                }
                pg_monitor_routine = VA_REF(*ap, vfunc*);
                is_ifunc(pg_monitor_routine);
	        pg_monitor_arg = VA_ARG(*ap, char *);
                break;
              case PG_JOIN_AUTHEN:
                pg_join_verifier(gname, VA_REF(*ap, ifunc*));
                break;
              case PG_CLIENT_AUTHEN:
                pg_client_verifier(gname, VA_REF(*ap, ifunc*));
                break;
              case PG_XFER:
                begin
                {
                    int dom;
                    vfunc *snd_proc, *rcv_proc;
                    dom = VA_ARG(*ap, int);
                    snd_proc = VA_REF(*ap, vfunc*);
                    rcv_proc = VA_REF(*ap, vfunc*);
                    is_ifunc(snd_proc); is_ifunc(rcv_proc);
                    allow_xfers_xd(gname, XD_USER+dom, snd_proc, rcv_proc);
                }
                break;
              case PG_CREDENTIALS:
                credentials = VA_REF(*ap, char*);
                break;
              case _PG_LOGGED:
                ++logged;
	        logfname = VA_REF(*ap, char*);
	        lentry = VA_ARG(*ap, int);
	        flush_when = VA_ARG(*ap, int);
	        log_routine = VA_REF(*ap, ifunc*);
	        is_ifunc(log_routine);
                break;
	      case PG_LOGPARAMS:
		nmbuf = VA_ARG(*ap, int);	/* Number buffered bef flush */
		nmdsk = VA_ARG(*ap, int);	/* Log len (msgs) before checkpt */
		lflen = VA_ARG(*ap, int);	/* File len (bytes) "" */
		timer = VA_ARG(*ap, unsigned);	/* Flush timer */
		break;
              case PG_DONTCREATE:
                ++dont_create;
                break;
              case PG_BIGXFER:	/* No longer special cased */
                break;
              case PG_INCARN:
                incarn = VA_ARG(*ap, int);
                break;
	      case PG_REPLAYSPOOL:
                ++replayspool;
                break;
	      case PG_DIFFUSION:
		++diffusion;
                break;
              default:
                isis_errno = IE_BADARG;
                goto err;
            }
            code = VA_ARG(*ap, int);
        }
        while(code);
	if(logged)
	{
	    if(log_init(logfname, gname, lentry, flush_when, log_routine, coldstart, ignoreold) == NULL)
		goto err;
	    if(nmbuf)
		log_cntthresh(gname, nmbuf);
	    if(nmdsk)
		log_reqsthresh(gname, nmdsk);
	    if(lflen)
		log_lenthresh(gname, lflen);
	    if(timer)
		log_timer(gname, timer);
	}
        old_state = isis_state;
        isis_state &= ~ISIS_XBYREF;
        if(logged)
        {
            switch(log_action(gname, &incarn))
            {
                case L_INIT:
                    /* Just do normal stuff */
                    break;
    
                case L_JOIN:
                    /* Must join */
                    --recover;
                    break;
    
                case L_RECOVER:
                    /* Legal to create (or join) but recover from log */
                    ++recover;
                    break;
            }
        }
  do_create:
        gaddr = pg_lookup(gname);
        retry = 1;
        while(aptr_isnull(gaddr))
        {
            static address mlist[2];
            if(dont_create || recover < 0)
            {   
                isis_errno = recover? IE_MUSTJOIN: IE_UNKNOWN;
                goto err;
            }
            mlist[0] = my_address;
            gaddr = pg_create(gname, incarn, mlist, (address*)0);
            if(!aptr_isnull(gaddr))
            {
		register qnode *qp;
	        if((qp = pg_find(ADDRS, gaddr)) == 0)
	             qp = pg_add(ADDRS, gaddr, NULLARG, NULLROUTINE);
                qp->qu_pname.addr_entry = 0;
                gip->gi_gaddr = &qp->qu_pname;
		if(diffusion)
		    gip->gi_flag |= GI_DIFFUSION;
                if(logged)
                    rmgr_start_log(gaddr, logfname);
                if(recover)
                    log_start_recovery(gname);
                if(recover == 0 || log_has_ckpt(gname) == 0)
                {
                    if(pg_init_routine)
                        ISISCALL1(pg_init_routine, gaddr);
                }
                else
                    log_replay_ckpt(gname);
                isis_state = old_state;
                if(pg_monitor_routine)
                    pg_monitor(gaddr, pg_monitor_routine, pg_monitor_arg);
                if(replayspool)
                    spool_replay(gname, 0);
                isis_joining = 0;
                t_sig_all(&wants_to_run_after_join, 0);
                ISIS_RETURN(gaddr);
            }
            if(retry-- == 0)
            {
                log_remove(gname);
                isis_errno = IE_AGAIN;
                goto err;
            }
            sleep(1);
            gaddr = pg_lookup(gname);
        }
        if(gv = pg_getlocalview(gaddr))
            for(adp = gv->gv_members; !aptr_isnull(adp); adp++)
                if(addr_cmp(adp, &my_address) == 0)
                {
		    if(diffusion)
		        gip->gi_flag |= GI_DIFFUSION;
                    isis_joining = 0;
                    t_sig_all(&wants_to_run_after_join, 0);
                    ISIS_RETURN(gaddr);
                }
        xfer_in_progress = 1;
	begin
	{
	    register qnode *qp;
	    if((qp = pg_find(ADDRS, gaddr)) == 0)
		 qp = pg_add(ADDRS, gaddr, NULLARG, NULLROUTINE);
	    qp->qu_pname.addr_entry = 0;
            gip->gi_gaddr = &qp->qu_pname;
	}
        if(act_blocked == 0)
        {
            if(old_act = act_begin())
                act_ev(1, old_act, ACT_MSG);
            cl_wid = cl_watch_for(gaddr, &my_address);
        }
        else
            old_act = -1;
        mp = msg_gen("%A[1],%s", gaddr, credentials);
  loop:
        if(abcast_l("s", gaddr, GENERIC_JOIN_REQ, mp, 1, "%d", &isis_errno) == 0)
        {
            register groupview *gv = pg_getlocalview(gaddr);
            register address *ap;
            /*
             * Join failed... pg_leave() if I got added to the group while this was happening */
            if(gv)
                for(ap = gv->gv_members; !aptr_isnull(ap); ap++)
                    if(addr_cmp(ap, &my_address) == 0)
		    {
			pg_leave(gaddr);
			break;
		    }
	    sleep(10);
            /* Ok to try again */
            if(niter++ == 0)
                goto loop;
            if(niter == 2)
                goto do_create;
	    panic("pg_join: can't create group, can't join group!");
        }
        xfer_in_progress = 0;
        msg_delete(mp);
        if(old_act != -1)
        {
            if(isis_errno == 0)
                act_restart();
            act_end(old_act);
            if(old_act)
                act_ev(-1, old_act, ACT_MSG);
        }
        isis_state = old_state;
        if(isis_errno == 0 && pg_monitor_routine)
            pg_monitor(gaddr, pg_monitor_routine, pg_monitor_arg);
        if(isis_errno)
        {
            if(cl_wid)
                cl_watch_cancel(gaddr, cl_wid);
            group_unmap(gip);
            isis_joining = 0;
            t_sig_all(&wants_to_run_after_join, 0);
            ISIS_RETURN(&NULLADDRESS);
        }
        if(logged && !aptr_isnull(gaddr))
        {
            log_checkpoint(gname);
            rmgr_start_log(gaddr, logfname);
        }
        if(gip = map_gname(gname))
	{
	    if(diffusion)
		gip->gi_flag |= GI_DIFFUSION;
            isis_joining = 0;
            t_sig_all(&wants_to_run_after_join, 0);
            ISIS_RETURN(gaddr);
	}
	/* Only happens if a group is deleted ``while'' the join is occuring */
	isis_errno = IE_TOTFAIL;
	isis_joining = 0;
	t_sig_all(&wants_to_run_after_join, 0);
	ISIS_RETURN(&NULLADDRESS);
  }

static  x_where XWHERE;

/* Tell remote side where transfer has gotten to */
static void
xf_where(mp)
  message *mp;
  {
        reply_l("f", mp, "%d,%d", XWHERE.domain, XWHERE.locator);
  }

/* Unpack a received message, calling the receive routine as needed */
int
xfer_rcv_unpack(gip, mp)
  register ginfo *gip;
  register message *mp;
  {
        register count = 0;
        message *msg;
        while(msg_get(mp, "%d,%d,%m", &XWHERE.domain, &XWHERE.locator, &msg) == 3)
        {
            if(gip->gi_xfer_rcv[XWHERE.domain] == NULLROUTINE)
            {
                print("Group address is "); paddr(gip->gi_gaddr);
                print(", name %s, domain %d\n", gip->gi_gname, XWHERE.domain);
                panic("rcv routine undefined!");
            }
            if(gip->gi_xbyref&(1<<XWHERE.domain))
                ISISCALL2(gip->gi_xfer_rcv[XWHERE.domain], &XWHERE.locator, &msg);
            else
                ISISCALL2(gip->gi_xfer_rcv[XWHERE.domain], XWHERE.locator, msg);
            msg_delete(msg);
            ++count;
        }
        return(count);
  }

/* Receive a block of state */
/* ** NOTE: This code is roughly duplicated in cl_lmgr.c:log_replay_ckpt.
            Any changes should also be made there. */
void
xfer_rcv_small(mp)
  register message *mp;
  {
        address gaddr;
        register ginfo *gip;
        msg_get(mp, "%a", &gaddr);
        gip = map_gaddr(&gaddr);
        if(gip == (ginfo*)0)
	{
	    paddr(&gaddr);
            panic(": xfer_rcv_small can't map gaddr");
	}
        (void)xfer_rcv_unpack(gip, mp);
  }

static    blocked;
static    condition want_join;

void
pg_join_inhibit(flag)
  int flag;
  {
        static join_inhibited;
        if(flag)
        {
            ++join_inhibited;
            isis_state |= ISIS_XJOINS;
        }
        else if(--join_inhibited == 0)
        {
            t_sig(&want_join, 0);
            isis_state &= ~ISIS_XJOINS;
        }
        else if(join_inhibited < 0)
            panic("inhibit_join(0) called more often than inhibit_join(1)");
  }


/* Called from cl_watch in cl_isis */
void
join_block()
  {
        act_block();
        ++blocked;
  }

/*
 * Coordinator choser (picks oldest curent member)
 */
address *
cc_pick_oldest(gaddr, senderp, players, arg)
  register address *players;
  address *senderp, *gaddr;
  VOID *arg; /* PFFS */
  {
        register groupview *gv = pg_getlocalview(gaddr);
        register address *ap, *bp;
        address sender;
        register alen;

        if(gv == 0)
            return(&NULLADDRESS);
        sender = *senderp;
        sender.addr_entry = 0;
	for(ap = gv->gv_members; !aptr_isnull(ap); ap++)
	    for(bp = players; !aptr_isnull(bp); bp++)
		if(addr_isequal(ap, bp))
		    return(ap);
        return(&NULLADDRESS);
  }

/*
 * Receive an xfer request.
 */
static void
pg_join_req(mp)
  register message *mp;
  {
        address gaddr, who;
        int pg_join_done(), cl_wid;
        static wants_to_join;

        ++wants_to_join;
        isis_state |= ISIS_WJOIN;
        who = *msg_getsender(mp);
        msg_get(mp, "%a", &gaddr);
        cl_wid = cl_watch_for(&gaddr, &who);
        if(isis_state&ISIS_XJOINS)
            (void)t_wait_l(&want_join, "pg_join waiting for its turn");
	pg_join_inhibit(TRUE);
        coord_cohort_l(mp, &gaddr, (vfunc *) pg_join_action, NULLROUTINE, NULLARG, cc_pick_oldest);
        cl_watch_cancel(&gaddr, cl_wid);
	pg_join_inhibit(FALSE);
        if(--wants_to_join == 0)
            isis_state &= ~ISIS_WJOIN;
        if(blocked && isis_ctp->task_act == act_blocked)
        {
            act_restart();
            --blocked;
        }
  }



typedef struct
{
        address x_who;
        address x_gaddr;
        int     x_how;
        int     x_rval;
} xinfo;

/* This is the coordinator routine */
static void
pg_join_action(mp, gaddr, how)
  register message *mp;
  address *gaddr;
  int how;
  {
        address who;
        int wid;
        register ginfo *gip = map_gaddr(gaddr);
        char *credentials;
        xinfo xi;
        
        if(gip == (ginfo*)0 || (act_blocked? gip->gi_mutexview: gip->gi_view)->gv_nmemb == 0)
        {
	    cc_refuse();
            return;
	}
        /* Verify join and do add_member operation; these are idempotent */
        isis_ctp->task_routine = (vfunc*)pg_join_action;
        who = *msg_getsender(mp);
        msg_get(mp, "%-s", &credentials);
        if(gip->gi_join_verifier == 0 ||
           (xi.x_rval = _ISISCALL1(gip->gi_join_verifier, credentials)) == 0)
        {
            xi.x_rval = 0;
            xi.x_who = who;
            xi.x_how = how;
            xi.x_gaddr = *gaddr;
            /* When join takes place, trigger the xfer_send routine */
            xfer_in_progress = 1;
            if((wid = pg_watch(gaddr, &who, W_JOIN, (vfunc *) xfer_send,
                               (void*)&xi)) == 0)
                /* Already a member */
                xfer_send(gaddr, &who, W_JOIN, &xi);
            else
            {
                register rval;
                join_gaddr = *gaddr;
                if(xfer_in_progress && (rval = pg_addmemb(gaddr, &who)))
                {
                    if(xi.x_rval == 0)
			xi.x_rval = rval;
                    pg_watch_cancel(wid);
                    goto done;
                }
		if(xfer_in_progress)
                    t_wait_l(&xfer_done, "isis system: coordinator waiting for xfer_send to finish");
            }
        }
        else if(xi.x_rval == -1)
            xi.x_rval = IE_NOTALLOWED;
        
  done:
        if(xfer_sock)
            close(xfer_sock);
	if(xi.x_rval != IE_REFUSED)
            reply(mp, "%d", xi.x_rval);
	else
            cc_refuse();
  }

#define FLUSH_XFER      4096    /* Transmit when it reaches this size */

static x_where where;
static xinfo *cur_xi;
static message *xfer_mp;
static making_checkpoint;

void
xfer_to_checkpoint(gaddr)
  address *gaddr;
  {
        register ginfo *gip = map_gaddr(gaddr);
        static xinfo xi;
	ISIS_ENTER();
        cur_xi = &xi;
        xi.x_rval = 0;
        where.gaddr = *gaddr;
        where.domain = 0;
        where.locator = -1;
        making_checkpoint = 1;
        do
        {
            if(gip->gi_xfer_gen[where.domain] == NULLROUTINE)
                continue;
            if(gip->gi_xbyref&(1<<where.domain))
                ISISCALL2(gip->gi_xfer_gen[where.domain], &where.locator, &gaddr);
            else
                ISISCALL2(gip->gi_xfer_gen[where.domain], where.locator, gaddr);
            where.locator = -1;
        }
        while(++where.domain != XD_MAX);
        if(xfer_mp)
            xfer_flush();
        making_checkpoint = 0;
	ISIS_EXIT();
  }

static xfermp_count;

static void
xfer_send(gaddr, who, event, xi)
  address *gaddr, *who;
  int event;
  xinfo *xi;
  {
        register ginfo *gip = map_gaddr(&xi->x_gaddr);
        cur_xi = xi;
        where.gaddr = xi->x_gaddr;
        if(event == W_FAIL)
	{
            xi->x_rval = IE_TOTFAIL;
	    goto done;
	}
	else if(gip == 0 || (isis_state&ISIS_LEAVING))
	{
            xi->x_rval = IE_REFUSED;
	    goto done;
	}
	
        if(xi->x_how == TAKEOVER)
        {
            /* Find out how far the transfer had gotten */
            if(fbcast(&xi->x_who, GENERIC_XFER_WHERE, "", 1, "%d,%d", &where.domain, &where.locator) != 1)
                xi->x_rval = IE_BROKEN;
        }
        else
        {
            where.domain = 0;
            where.locator = -1;
        }
	xfermp_count = 0;
        if(xi->x_rval == 0)
        {
            do
            {
                if(gip->gi_xfer_gen[where.domain] == NULLROUTINE)
                    continue;
                if(gip->gi_xbyref&(1<<where.domain))
                    ISISCALL2(gip->gi_xfer_gen[where.domain],
                          &where.locator, &xi->x_gaddr);
                else
                    ISISCALL2(gip->gi_xfer_gen[where.domain],
                          where.locator, &xi->x_gaddr);
                where.locator = -1;
            }
            while(xi->x_rval == 0 && ++where.domain != XD_MAX);
            if(xfer_mp)
                xfer_flush();
        }
  done:
	/* Wait for ack that state was received */
	if(xfermp_count > 0)
            (void)fbcast(&xi->x_who, GENERIC_XFER_WHERE, "", 1, "%d,%d", &where.domain, &where.locator);
        xfer_in_progress = 0;
        t_sig(&xfer_done, 0);
  }

void
xfer_refuse()
  {
        register xinfo *xi = cur_xi;
	xi->x_rval = IE_REFUSED;
  }

void
xfer_out(va_alist)
  va_dcl
  {
	int locator;
        va_list ap;
        message *mp;
        va_start(ap);
        locator = VA_ARG(ap, int);
        mp = msg_newmsg();
	msg_doputf(mp, SYSFLD_SCAN, &ap);
	do_xfer_out(locator, mp);
        va_end(ap);
        msg_delete(mp);
  }

void
do_xfer_out(locator, mp)
  int locator;
  message *mp;
  {
	ISIS_ENTER();
        if(cur_xi->x_rval == IE_BROKEN)
            return;
        where.locator = locator;
        if(xfer_mp == 0)
            xfer_mp = msg_gen("%A[1]", &where.gaddr);
        msg_put(xfer_mp, "%d,%d,%m", where.domain, where.locator, mp);
        if(msg_getlen(xfer_mp) >= FLUSH_XFER)
            xfer_flush();
	ISIS_EXIT();
  }

int
xfer_flush()
  {
        register xinfo *xi = cur_xi;
	ISIS_ENTER();
        if(xfer_mp == 0)
            ISIS_RETURN(0);
        if(xi->x_rval == 0)
        {
            if(making_checkpoint)
                logging_out(xfer_mp);
            else 
	    {
		++xfermp_count;
                fbcast_l("sR", &xi->x_who, GENERIC_XFER_STATE, xfer_mp, 0);
	    }
        } 
        msg_delete(xfer_mp);
        xfer_mp = 0;
	ISIS_RETURN(0);
  }

void
allow_xfers_xd(gname, xd, send_routine, rcv_routine)
  char *gname;
  register xd;
  vfunc *send_routine, *rcv_routine;
  {
        register ginfo *gip;
        if(xd > XD_MAX || xd < 0)
            panic("allow_xfers_xd: xd %d", xd);
        if(gname != 0)
        {
            gip = add_gname(gname);
            gip->gi_xfer_gen[xd] = send_routine;
            gip->gi_xfer_rcv[xd] = rcv_routine;
            if(isis_state&ISIS_XBYREF)
                 gip->gi_xbyref |= 1<<xd;
        }
        else
        {
            def_xfer_gen[xd] = send_routine;
            def_xfer_rcv[xd] = rcv_routine;
            for(gip = isis_groups; gip; gip = gip->gi_next)
            {
                gip->gi_xfer_gen[xd] = send_routine;
                gip->gi_xfer_rcv[xd] = rcv_routine;
            }
        }
  }

void
pg_join_verifier(gname, routine)
  char *gname;
  ifunc *routine;
  {
	is_ifunc(routine);
        add_gname(gname)->gi_join_verifier = routine;
  }

void
pg_client_verifier(gname, routine)
  char *gname;
  ifunc *routine;
  {
	is_ifunc(routine);
        add_gname(gname)->gi_client_verifier = routine;
  }

/* Makes an exact duplicate of specified group */
address*
pg_dup(gaddr, sgname)
  address *gaddr;
  char *sgname;
  {
        register groupview *gv;
        ISIS_ENTER();
        gv = pg_getlocalview(gaddr);
        if(gv == 0)
            ISIS_RETURN(&NULLADDRESS);
	return( pg_subgroup(gaddr, sgname, 0, gv->gv_members, gv->gv_clients) );
  }

address*
pg_subgroup(gaddr, sgname, incarn, mlist, clist)
  address *gaddr;
  char *sgname;
  int incarn;
  address *mlist, *clist;
  {
        register groupview *gv;
        register address *mp, *mmp, *ap;
        address alist[MAX_PROCS];
        ISIS_ENTER();
        gv = pg_getlocalview(gaddr);
        if(gv == 0)
            ISIS_RETURN(&NULLADDRESS);
	mmp = alist;
        for(mp = mlist; !aptr_isnull(mp); mp++)
        {
	    /* Trim non-members from list */
            for(ap = gv->gv_members; !aptr_isnull(ap); ap++)
                if(addr_cmp(mp, ap) == 0)
                    break;
            if(!aptr_isnull(ap))
                *mmp++ = *mp;
        }
	*mmp = NULLADDRESS;
	if(mmp != alist)
            ISIS_RETURN(pg_create(sgname, incarn, alist, clist));
	ISIS_RETURN(&NULLADDRESS);
  }

static address*
pg_create(gname, incarn, mlist, clist)
  char *gname;
  int incarn;
  address *mlist, *clist;
  {
        message *msg = msg_newmsg();
        address gaddr, alist[MAX_PROCS];
	register qnode *qp;
        register address *ap, *mp;

        if(strlen(gname) > PG_GLEN-1)
            gname[PG_GLEN-1] = 0;
        ap = alist;
        for(mp = mlist; mp && !aptr_isnull(mp); ++mp)
            *ap++ = *mp;
        *ap++ = NULLADDRESS;
        for(mp = clist; mp && !aptr_isnull(mp); ++mp)
            *ap++ = *mp;
        *ap++ = NULLADDRESS;
        msg_addfield(msg, CL_PNAME, (char*)alist, FTYPE_ADDRESS, sizeof(address)*(ap-alist));
        if(gname)
            msg_addfield(msg, CL_GNAME, gname, FTYPE_CHAR, strlen(gname)+1);
        msg_addfield(msg, CL_INCARN, (char*)&incarn, FTYPE_LONG, sizeof(incarn));
        if(isis(CL_CREATE, msg, (char*)&gaddr, sizeof(gaddr)) != sizeof(gaddr))
            panic("pg_create: no GID returned");
        msg_delete(msg);
 	if((qp = pg_find(ADDRS, &gaddr)) == 0)
            qp = pg_add(ADDRS, &gaddr, NULLARG, NULLROUTINE);
        return(&qp->qu_pname);
  }

int
pg_addmemb(gaddr, pname)
  address *gaddr, *pname;
  {
        register message *msg = msg_newmsg();
        static address addrs[3], name[2];
        char answ[MAX_PROCS];
        int rval;

        name[0] = *pname;
        msg_addfield(msg, CL_GID, (char*)gaddr, FTYPE_ADDRESS, sizeof(address));
        msg_addfield(msg, CL_PNAME, (char*)name, FTYPE_ADDRESS, sizeof(name));
        addrs[0] = *gaddr;
        addrs[0].addr_entry = GENERIC_ADDMEMB;
        addrs[1] = *pname;
        addrs[1].addr_entry = GENERIC_ADDMEMB;
        msg_insertfield(msg, SYSFLD_VCHANGE, NULLARG, FTYPE_CHAR, 0);
        rval = gbcast_grow("lsg", addrs, msg, ALL, "%b", answ);
        msg_delete(msg);
        if(rval == 0)
            return(IE_TOTFAIL);
        return(0);
  }

int
pg_client(gaddr, credentials)
  address *gaddr;
  char *credentials;
  {
        int rval, err;
        ISIS_ENTER();
        if((rval = bcast(gaddr, GENERIC_CLIENT_REQ, "%A[1]/%s", gaddr, credentials, 1, "%d", &err)) == 1)
        {
            if((isis_errno = err) == 0)
                ISIS_RETURN(0);
            ISIS_RETURN(-1);
        }
        else if(rval == 0)
            isis_errno = IE_TOTFAIL;
        ISIS_RETURN(-1);
  }

static void
pg_client_req(mp)
  register message *mp;
  {
        address gaddr;
        if(msg_get(mp, "%a", &gaddr) != 1)
            panic("pg_client_req");
        coord_cohort(mp, &gaddr, (vfunc *) pg_client_action,
                     NULLROUTINE, NULLARG);
  }

static void
pg_client_action(mp, gaddr, how, arg)
  register message *mp;
  address *gaddr;
  int how;
  VOID *arg;
  {
        char *credentials;
        register ginfo *gip;
        if(msg_get(mp, "%-s", &credentials) != 1)
            panic("pg_client_action");
        if((gip = map_gaddr(gaddr)) == (ginfo*)0)
            reply(mp, "%d", IE_UNKNOWN);
        else if(gip->gi_client_verifier &&
                _ISISCALL1(gip->gi_client_verifier, credentials) == -1)
            reply(mp, "%d", IE_NOTALLOWED);
        else if(pg_addclient(gaddr, msg_getsender(mp)) == 0)
            reply(mp, "%d", 0);
        else
            reply(mp, "%d", isis_errno);
  }
