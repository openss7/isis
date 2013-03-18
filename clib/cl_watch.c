/*  $RCSfile: cl_watch.c,v $ $Revision: 2.48 $ $Date: 90/08/03 13:09:16 $  */
/*
 *	Originally coded by Ken Birman
 *      ISIS distributed systems toolkit: watch for process joins and failure
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

struct  wnode
  {
        vfunc   *w_proc;
        address w_gaddr;
        address w_paddr;
        VOID    *w_arg;
        int     w_act;
        int     w_xbyref;
        int     w_id;
        int     w_jid;
        int     w_mode;
  };

static adesc   wadesc ={ sizeof(wnode),  0,  8, };

#define w_alloc()       (wnode*)mallocate(&wadesc)

void
w_free(wp)
  register wnode *wp;
  {
        act_ev(-1, wp->w_act, ACT_WATCH);
        mdeallocate((char*)wp, &wadesc);
  }

static WID = 1<<30, PWID = 2<<30;
static void w_new_sl(), pwatch_call(), watch_call(), pg_detect_pfailure();
static void cl_proc_failed(), pg_failed_first(), do_pmonitor(), pg_watching();
static qnode *detect_failure, *watching;

#define	POST_WATCH	0
#define	TRIGGER		1

void
w_init()
{
        register act = isis_ctp->task_act;
        isis_task((vfunc *) pwatch_call, "pg_watch:pwatch_call");
        isis_task((vfunc *) pg_failed_first, "pg_watch:pg_failed_first");
        isis_task((vfunc *) watch_call, "pg_watch:watch_call");
        isis_task((vfunc *) w_new_sl, "pg_watch:w_new_sl");
        isis_task((vfunc *) do_pmonitor, "pg_watch:call_pmonitor");
        isis_task((vfunc *) pg_detect_pfailure, "pg_watch:pg_detect_pfailure");
        isis_entry(GENERIC_PROC_FAILED, (vfunc *) cl_proc_failed, "pg_watch:cl_proc_failed");
        isis_entry(GENERIC_WATCHING, (vfunc *) pg_watching, "pg_watch:pg_watching");
        isis_ctp->task_act = 0;
        sv_monitor(w_new_sl, NULLARG);
        isis_ctp->task_act = act;
	detect_failure = qu_null();
  }

int
pg_watch(gaddrp, whop, event, routine, arg)
  address *whop, *gaddrp;
  int event;
  vfunc *routine;
  VOID *arg;
  {
	address who, gaddr;
        register wnode *wp;
        register qnode *qp;
        register address *vp;
        groupview *gv;
        int w_event();

	ISIS_ENTER();
	who = *whop; gaddr = *gaddrp;
        who.addr_entry = gaddr.addr_entry = 0;
        if((gv = pg_getlocalview(&gaddr)) == 0)
            return(-1);
        if(!aptr_isnull(&who))
        {
            for(vp = gv->gv_members; !aptr_isnull(vp); vp++)
                if(addr_cmp(vp, &who) == 0)
                    break;
            if(event == W_LEAVE && aptr_isnull(vp))
                ISIS_RETURN(0);
            else if(event == W_JOIN && !aptr_isnull(vp))
                ISIS_RETURN(0);
        }
        if((qp = pg_find(isis_wlist, &gaddr)) == 0)
            qp = pg_add_qu(isis_wlist, &gaddr, qu_null());
        wp = w_alloc();
        wp->w_gaddr = gaddr;
        wp->w_paddr = who;
        wp->w_proc = routine;
        wp->w_arg = arg;
        wp->w_id = ++WID;
        wp->w_act = isis_ctp->task_act;
        wp->w_xbyref = isis_state&ISIS_XBYREF;
        act_ev(1, wp->w_act, ACT_WATCH);
        wp->w_mode = event;
        (void)pg_add_wp(qp->qu_queue, &who, wp, (vfunc *) w_free);
        if(event == W_JOIN)
        {
            wp->w_jid = proc_watch(&who, (vfunc *) pg_failed_first, (char*)wp);
        }
        else
            wp->w_jid = 0;
        ISIS_RETURN(wp->w_id);
  }

static 
void pg_detect_pfailure(gaddrp, event, wp)
  address *gaddrp; /* May be the group address if called via pg_watching
                      of a member process address if called via proc_watch. */
  int event;
  register wnode *wp;
  {
        address gaddr;
	register count = 0;
	register groupview *gv;
	if(gv = pg_getlocalview(gaddrp))
	    return;
	gaddr = wp->w_gaddr;
 again: if(++count == 6 || (gv = pg_getview(gaddrp)) == 0)
        {
	    pg_detected_failure(&gaddr);
        }
        else if(!addr_isequal(gaddrp, gv->gv_members))
        {
            wp->w_jid = proc_watch(gv->gv_members,
                                   (vfunc *) pg_detect_pfailure, wp);
        }
	else
	{
	    sleep(10);
	    goto again;
	}
    }

int
pg_detect_failure(gaddrp, routine, arg)
  address *gaddrp;
  vfunc *routine;
  VOID *arg;
  {
	register groupview *gv;
	register am_client = 0;
	register wnode *wp;
	ISIS_ENTER();
	if(gv = pg_getlocalview(gaddrp))
	{
	    register address *ap;
	    for(ap = gv->gv_members; !aptr_isnull(ap); ap++)
		if(addr_ismine(ap))
		    return(-1);
	    ++am_client;
	}
	else if((gv = pg_getview(gaddrp)) == 0)
	    return(-1);
        wp = w_alloc();
        wp->w_id = ++WID;
        wp->w_gaddr = *gaddrp;
        wp->w_gaddr.addr_entry = 0;
        wp->w_proc = routine;
        wp->w_arg = arg;
        wp->w_act = isis_ctp->task_act;
        wp->w_xbyref = isis_state&ISIS_XBYREF;
	if(am_client == 0)
	{
	    wp->w_jid = proc_watch(gv->gv_members, (vfunc *) pg_detect_pfailure, wp);
	    fbcast(gv->gv_members, GENERIC_WATCHING, "%d,%A[1],%d", POST_WATCH, gaddrp, wp->w_id, 0);
	}
	else
	    wp->w_jid = 0;
        act_ev(1, wp->w_act, ACT_WATCH);
        (void)pg_add_wp(detect_failure, &wp->w_gaddr, wp, (vfunc *) w_free);
	ISIS_RETURN(wp->w_id);
  }

static void
pg_watching(mp)
  register message *mp;
  {
	register ginfo *gip;
	int req, wid;
	address gaddr;
	msg_get(mp, "%d,%a,%d", &req, &gaddr, &wid);
	gip = map_gaddr(&gaddr);
	if(req == POST_WATCH)
	{
	    if(gip == 0 || pg_rank(&gaddr, &my_address) < 0)
	    {
	        fbcast(msg_getsender(mp), GENERIC_WATCHING, "%d,%A[1],%d", TRIGGER, gaddr, wid, 0);
		return;
	    }
            pg_add(gip->gi_watching, msg_getsender(mp), (VOID *) wid, NULLROUTINE);
	}
	else /* TRIGGER */
	{
	    register qnode *qp;
	    for(qp = detect_failure->qu_next; qp != detect_failure; qp = qp->qu_next)
		if(qp->qu_wnode->w_id == wid)
		{
		    pg_detect_pfailure(&gaddr, W_FAIL, qp->qu_wnode);
		    return;
		}
	}
  }

void
pg_detected_failure(gaddrp)
  address *gaddrp;
  {
	register qnode *qp;
	while(qp = pg_find(detect_failure, gaddrp))
	{
            int jid = qp->qu_wnode-> w_jid;
	    qu_remove(qp);
	    t_fork((vfunc *) pwatch_call, qp);

            if (jid != 0 && jid != -1)
            {
                proc_watch_cancel(jid);
            }
	}
  }

static void
pg_failed_first(paddr, event, wp)
  address *paddr;
  int event;
  register wnode *wp;
  {
        isis_ctp->task_routine = wp->w_proc;
        isis_ctp->task_arg0 = wp->w_arg;
        isis_ctp->task_act = wp->w_act;
        wp->w_jid = 0;
        if((vfunc*)wp->w_proc == (vfunc*)pg_failed_first)
            panic("pg_failed_first recursion bug");
        if(wp->w_xbyref)
        {
            int wf = W_FAIL;
            ISISCALL4(wp->w_proc, &wp->w_gaddr, &wp->w_paddr, &wf, &wp->w_arg);
        }
        else
            ISISCALL4(wp->w_proc, &wp->w_gaddr, &wp->w_paddr, W_FAIL, wp->w_arg);
        pg_watch_cancel(wp->w_id);
  }

static  address g_paddr;

static void
do_pmonitor()
  {
        address paddrs[2];
        /* This blocks */
        paddrs[0] = g_paddr;
        paddrs[1] = NULLADDRESS;
        if(cl_pmonitor(&g_paddr) != -1)
            return;
        do_cl_proc_failed(paddrs);
  }

int
proc_watch(paddrp, routine, arg)
  address *paddrp;
  vfunc *routine;
  VOID *arg;
  {
        register wnode *wp;
        int w_event();

	ISIS_ENTER();
        /* Monitor a process that isn't a member of any group */
        g_paddr = *paddrp;
        g_paddr.addr_entry = 0;
        t_fork_urgent((vfunc *) do_pmonitor, 0);
        wp = w_alloc();
        wp->w_proc = routine;
        wp->w_arg = arg;
        wp->w_id = ++PWID;
        wp->w_act = isis_ctp->task_act;
        wp->w_xbyref = isis_state&ISIS_XBYREF;
        act_ev(1, wp->w_act, ACT_WATCH);
        wp->w_mode = W_FAIL;
        (void)pg_add_wp(isis_pwlist, &g_paddr, wp, (vfunc *) w_free);
        ISIS_RETURN(wp->w_id);
  }

static void
pwatch_call(qp)
  register qnode *qp;
  {
        register wnode *wp = qp->qu_wnode;
        isis_ctp->task_routine = wp->w_proc;
        isis_ctp->task_arg0 = wp->w_arg;
        isis_ctp->task_act = wp->w_act;
        if(wp->w_xbyref)
        {
            int wf = W_FAIL;
            ISISCALL3(wp->w_proc, &qp->qu_pname, &wf, &wp->w_arg);
        }
        else
            ISISCALL3(wp->w_proc, &qp->qu_pname, W_FAIL, wp->w_arg);
        qu_free(qp);
  }

/* If a site crashes, check to see if we were monitoring any processes there */
void static
w_new_sl(sv)
  register sview *sv;
  {
        register qnode *qp, *nqp;
        for(qp = isis_pwlist->qu_next; qp != isis_pwlist; qp = nqp)
        {
            nqp = qp->qu_next;
            if(act_blocked && qp->qu_wnode->w_act != act_blocked)
                continue;
            if(sv->sv_incarn[qp->qu_pname.addr_site] != qp->qu_pname.addr_incarn)
            {   
                qu_remove(qp);
                t_fork((vfunc *) pwatch_call, qp);
            }
        }
  }


int
pg_watch_cancel(wid)
  int wid;
  {
        register qnode *gp, *qp;
	ISIS_ENTER();
        for(gp = isis_wlist->qu_next; gp != isis_wlist; gp = gp->qu_next)
            for(qp = gp->qu_queue->qu_next; qp != gp->qu_queue; qp = qp->qu_next)
                if(qp->qu_wnode->w_id == wid)
                {
                    register jid = qp->qu_wnode->w_jid;
                    qu_free(qp);
                    if(jid && jid != -1)
                        proc_watch_cancel(jid);
                    if(qu_head(gp->qu_queue) == 0)
                        qu_free(gp);
                    ISIS_RETURN(0);
                }
        for(qp = detect_failure->qu_next; qp != detect_failure; qp = qp->qu_next)
	    if(qp->qu_wnode->w_id == wid)
	    {
	        register jid = qp->qu_wnode->w_jid;
	        if(jid && jid != -1)
		    proc_watch_cancel(jid);
	        qu_free(qp);
	        ISIS_RETURN(0);
	    }
        ISIS_RETURN(-1);
  }

int
proc_watch_cancel(wid)
  int wid;
  {
        register qnode *qp;
	ISIS_ENTER();
        for(qp = isis_pwlist->qu_next; qp != isis_pwlist; qp = qp->qu_next)
            if(qp->qu_wnode->w_id == wid)
            {
                qu_free(qp);
                ISIS_RETURN(0);
            }
        ISIS_RETURN(-1);
  }

static void
watch_call(qp)
  register qnode *qp;
  {
        register wnode *wp = qp->qu_wnode;
        register jid;
        isis_ctp->task_routine = wp->w_proc;
        isis_ctp->task_arg0 = wp->w_arg;
        isis_ctp->task_act = wp->w_act;
        jid = wp->w_jid;
        if(wp->w_xbyref)
            ISISCALL4(wp->w_proc,
                  &wp->w_gaddr, &qp->qu_pname, &wp->w_mode, &wp->w_arg);
        else
            ISISCALL4(wp->w_proc,
                  &wp->w_gaddr, &qp->qu_pname, wp->w_mode, wp->w_arg);
        qu_free(qp);
        if(jid)
            proc_watch_cancel(jid);
  }

void
w_new_view(gv)
  register groupview *gv;
  {
        register qnode *wq, *qp, *nqp, *wroot;
	if(gv->gv_nmemb == 0)
	{
	    register ginfo *gip;
	    if(gip = map_gaddr(&gv->gv_gaddr))
	        while(qp = qu_head(gip->gi_watching))
	        {
	            address *gaddrp;
	            int req, wid;
	            qu_remove(qp);
	            fbcast(&qp->qu_pname, GENERIC_WATCHING, "%d,%A[1],%d", TRIGGER, &gv->gv_gaddr, qp->qu_data, 0);
	            qu_free(qp);
	        }
        }
        if((wroot = pg_find(isis_wlist, &gv->gv_gaddr)) != 0)
        {
            wq = wroot->qu_queue;
            for(qp = wq->qu_next; qp != wq; qp = nqp)
            {
                register address *ap;
                register wnode *wp = qp->qu_wnode;
                
                nqp = qp->qu_next;
                
                if(act_blocked && qp->qu_wnode->w_act != act_blocked)
                    continue;
                if(gv->gv_nmemb == 0)
                    goto always;
                if(wp->w_mode == W_LEAVE)
                {
                    if(!aptr_isnull(&qp->qu_pname))
                    {
                        for(ap = gv->gv_members; !aptr_isnull(ap); ap++)
                            if(addr_cmp(ap, &qp->qu_pname) == 0)
                                break;
                        if(!aptr_isnull(ap))
                            continue;
                    }
                    else
                    {
                        if(aptr_isnull(&gv->gv_departed))
                            continue;
                        qp->qu_pname = gv->gv_departed;
                    }
                }
                else
                {
                    if(!aptr_isnull(&qp->qu_pname))
                    {
                        for(ap = gv->gv_members; !aptr_isnull(ap); ap++)
                            if(addr_cmp(ap, &qp->qu_pname) == 0)
                                break;
                        if(aptr_isnull(ap))
                            continue;
                    }
                    else
                    {
                        if(aptr_isnull(&gv->gv_joined))
                            continue;
                        qp->qu_pname = gv->gv_joined;
                    }
                }
              always:
                if(gv->gv_nmemb == 0 && wp->w_mode == W_JOIN)
                    wp->w_mode = W_FAIL;
                qu_remove(qp);
                t_fork((vfunc *) watch_call, qp);
            }
            if(wq->qu_next == wq)
                qu_free(wroot);
        }
	if(gv->gv_nmemb == 0)
	    pg_detected_failure(&gv->gv_gaddr);
    }

int
cl_pmonitor(addr)
  address *addr;
  {
        register message *mp;
        int outcome;
        mp = msg_genmsg(CL_PNAME, (char*)addr, FTYPE_ADDRESS, sizeof(address), 0);
        isis(CL_PMONITOR, mp, &outcome, sizeof(outcome));
        msg_delete(mp);
        return(outcome);
  }

static void
cl_proc_failed(mp)
  register message *mp;
  {
        do_cl_proc_failed((address*)msg_getfield(mp, CL_PNAME, 1, NULLIARG));
  }

void
do_cl_proc_failed(ap)
  register address *ap;
  {
        register qnode *qp, *nqp;
        extern condition wants_to_run_after_join;
        while(isis_joining)
            (void)t_wait_l(&wants_to_run_after_join, "isis system: proc_failed waiting for join to finish");
        for(qp = isis_pwlist->qu_next; qp != isis_pwlist; qp = nqp)
        {
            nqp = qp->qu_next;
            if(act_blocked && qp->qu_wnode->w_act != act_blocked)
                continue;
            if(addr_cmp(&qp->qu_pname, ap) == 0)
            {
                qu_remove(qp);
                t_fork(pwatch_call, qp);
            }
        }
  }

void
proc_monitor_dump()
  {
        register qnode *gp, *qp;
        for(gp = isis_wlist->qu_next; gp != isis_wlist; gp = gp->qu_next)
            for(qp = gp->qu_queue->qu_next; qp != gp->qu_queue; qp = qp->qu_next)
            {
                register wnode *wp = qp->qu_wnode;
                print("  [act %d wid %x]: Watch addr ", wp->w_act, wp->w_id);
                paddr(&qp->qu_pname);
                print(", ");
                paddr(&gp->qu_pname);
                print(". Call ");
                cl_rname(wp->w_proc);
                print("(%x)\n", wp->w_arg);
            }
        for(qp = isis_pwlist->qu_next; qp != isis_pwlist; qp = qp->qu_next)
        {
            register wnode *wp = qp->qu_wnode;
            print("  [act %d wid %x]: Watch process ", wp->w_act, wp->w_id);
            paddr(&qp->qu_pname);
            print(". Call ");
            cl_rname(wp->w_proc);
            print("(%x)\n", wp->w_arg);
        }
        for(qp = detect_failure->qu_next; qp != detect_failure; qp = qp->qu_next)
        {
	    register wnode *wp = qp->qu_wnode;
	    print("  [act %d wid %x]: Watch group ", wp->w_act, wp->w_id);
	    paddr(&qp->qu_pname);
	    print(". On total failure call ");
	    cl_rname(wp->w_proc);
	    print("(%x)\n", wp->w_arg);
	}
  }
