/*  $RCSfile: cl_sview.c,v $ $Revision: 2.31 $ $Date: 90/07/31 11:13:30 $  */
/*
 *	Originally coded by Ken Birman
 *      Monitor site view changes
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

static	sv_known;
static  condition wants_view;
condition isis_want_incarn;
sview   isis_sv;                        /* Site-views */
sview   isis_svmutex;                   /* Site-views */

sview *
site_getview()
  {
	register sview *sv = act_blocked? &isis_svmutex: &isis_sv;
	if(sv_known == 0)
	{
            site_id *sid;
            for(sid = sv->sv_slist; *sid; sid++)
                if (SITE_NO(*sid) == my_site_no)
                {
                    if (SITE_INCARN(*sid) == RECOVERY_INCARN)
                    {
                        t_wait_l(&wants_view,
                                 "site-getview: waiting for first view");
                    }
                    break;
                }
	    sv_known = 1;
	}
        return(sv);
  }

void
sv_init()
  {
        static firsttime;
        if(firsttime++ == 0)
        {
            isis_entry(GENERIC_NEW_SVIEW, sv_new_sview, "sv_watch:sv_new_sview");
            isis_swlist = qu_null();
        }
  }

struct svmon
{
        site_id sv_sid;
        int     sv_event;
        int     sv_uid;
        int     sv_act;
        int     sv_xbyref;
        int     sv_viewid;
        vfunc   *sv_proc;
        VOID    *sv_arg;
};

static adesc    sv_adesc ={ sizeof(svmon), 0, 8, };

#define sv_alloc()        ((svmon*)mallocate(&sv_adesc))

void
sv_free(svm)
  svmon *svm;
  {
        act_ev(-1, svm->sv_act, ACT_WATCH);
        mdeallocate((char*)svm, &sv_adesc);
  }

static  SVID;

int
sv_monitor(routine, arg)
  vfunc *routine;
  VOID *arg;
  {
        return(sv_watch(0, W_MONITOR, routine, arg));
  }

int
sv_monitor_cancel(svid)
  int svid;
  {
        return(sv_watch_cancel(svid));
  }

int
sv_watch(sid, event, routine, arg)
  site_id sid;
  int event;
  vfunc *routine;
  VOID *arg;
  {
        register svmon *svm;
	ISIS_ENTER();
	svm = sv_alloc();
        svm->sv_uid = ++SVID;
        svm->sv_viewid = 0;
        svm->sv_act = isis_ctp->task_act;
        svm->sv_xbyref = isis_state&ISIS_XBYREF;
        act_ev(1, svm->sv_act, ACT_WATCH);
        svm->sv_sid = sid;
        svm->sv_event = event;
        svm->sv_proc = routine;
        svm->sv_arg = arg;
        (void)qu_add_svm(isis_swlist, SVID, svm, (vfunc *) sv_free);
        ISIS_RETURN(SVID);
  }

int
sv_watch_cancel(svid)
  int svid;
  {
        register qnode *qp;
	ISIS_ENTER();
        if(qp = qu_find(isis_swlist, svid))
        {
            qu_free(qp);
            ISIS_RETURN(0);
        }
        ISIS_RETURN(-1);
  }

void
sv_dovcall(svm)
  register svmon *svm;
  {
        register sview *sv = act_blocked? &isis_svmutex: &isis_sv;
        isis_ctp->task_routine = svm->sv_proc;
        isis_ctp->task_arg0 = svm->sv_arg;
        isis_ctp->task_act = svm->sv_act;
        if(svm->sv_xbyref)
        {
            sview *isv = sv;
            ISISCALL2(svm->sv_proc, isv, &svm->sv_arg);
        }
        else
            ISISCALL2(svm->sv_proc, sv, svm->sv_arg);
        sv_free(svm);
  }

void
sv_doecall(svm)
  register svmon *svm;
  {
        isis_ctp->task_routine = svm->sv_proc;
        isis_ctp->task_arg0 = svm->sv_arg;
        if(svm->sv_xbyref)
             ISISCALL3(svm->sv_proc, &svm->sv_sid, &svm->sv_event, &svm->sv_arg);
        else
             ISISCALL3(svm->sv_proc, svm->sv_sid, svm->sv_event, svm->sv_arg);
        sv_free(svm);
  }

void
sv_new_sview(mp)
  register message *mp;
  {
        register sview *sp = (sview*)msg_getfield(mp, CL_VIEW, 1, NULLIARG);
        register qnode *qp, *nqp;

        bcopy(sp, act_blocked? &isis_svmutex: &isis_sv, sizeof(sview));
        for(qp = isis_swlist->qu_next; qp != isis_swlist; qp = nqp)
        {
            register svmon *svm = qp->qu_svm, *scopy;
            nqp = qp->qu_next;
            if(act_blocked && svm->sv_act != act_blocked)
                continue;
            if(svm->sv_viewid == sp->sv_viewid || ((svm->sv_viewid&0xFF) == (sp->sv_viewid&0xFF) && svm->sv_viewid > sp->sv_viewid))
                break;
            svm->sv_viewid = sp->sv_viewid;
            switch(svm->sv_event)
            {
              case W_FAIL:
                   if(svm->sv_sid != NULLSID)
                   {
                       if(bit(&sp->sv_failed, SITE_NO(svm->sv_sid)))
                            continue;
                   }
                   else
                   {
                        register s;
                        if(btst(&sp->sv_failed) == 0)
                            continue;
                        for(s = 0; s < MAX_SITES; s++)
                            if(bit(&sp->sv_failed, s))
                            {
                                svm->sv_sid = MAKE_SITE_ID(s, 0);
                                break;
                            }
                   }
                   goto trigger;
              case W_RECOVER:
                   if(svm->sv_sid != NULLSID)
                   {
                       if(bit(&sp->sv_recovered, SITE_NO(svm->sv_sid)))
                            continue;
                   }
                   else
                   {
                        register s;
                        if(btst(&sp->sv_recovered) == 0)
                            continue;
                        for(s = 0; s < MAX_SITES; s++)
                            if(bit(&sp->sv_recovered, s))
                            {
                                svm->sv_sid = MAKE_SITE_ID(s, sp->sv_incarn[s]);
                                break;
                            }
                   }
              trigger:
                    scopy = sv_alloc();
                    *scopy = *svm;
                    act_ev(1, svm->sv_act, ACT_WATCH);
                    t_fork((vfunc *) sv_doecall, scopy);
                    if(svm->sv_event != W_MONITOR)
                        qu_free(qp);
                    break;
              case W_MONITOR:
                    scopy = sv_alloc();
                    *scopy = *svm;
                    act_ev(1, svm->sv_act, ACT_WATCH);
                    t_fork((vfunc *) sv_dovcall, scopy);
                    break;
            }
        }

	if(sv_known == 0)
	{
	    register site_id *sid;
            for(sid = sp->sv_slist; *sid; sid++)
	        if(SITE_NO(*sid) == my_site_no && SITE_INCARN(*sid) != RECOVERY_INCARN)
		    break;
	    if(*sid == 0)
		panic("client_new_sview: MY SITE NOT LISTED!");
            if(act_blocked)
                bcopy(sp, &isis_sv, sizeof(sview));
	    ++sv_known;
	    t_sig_all(&wants_view, 0);
	    t_sig_all(&isis_want_incarn, 0);
	}
	for(qp = isis_tasks->qu_next; qp != isis_tasks; qp = qp->qu_next)
	{
	    register task *tp = qp->qu_task;
            if(tp->task_queue == &tp->task_mwant)
	    {
		register site_id *sid;
		register siteno, inc;
		if((siteno = tp->task_sentto.addr_site) == my_site_no)
		    continue;
		inc = tp->task_sentto.addr_incarn;
                for(sid = sp->sv_slist; *sid; sid++)
	            if(SITE_NO(*sid) == siteno && (SITE_INCARN(*sid) == inc || inc == RECOVERY_INCARN))
		        break;
		if(*sid == 0)
		    t_sig(&tp->task_mwant, (VOID*)0);
	    }

	}
  }

void
site_monitor_dump()
  {
        register qnode *qp;
        for(qp = isis_swlist->qu_next; qp != isis_swlist; qp = qp->qu_next)
        {
            register svmon *svm = qp->qu_svm;
            print("  [act %d swid %x]: ", svm->sv_act, svm->sv_uid);
            switch(svm->sv_event)
            {
              case W_MONITOR:  print("monitoring site-views, "); break;
              case W_FAIL:  print("watching site %d/%d, ", SITE_NO(svm->sv_sid), SITE_INCARN(svm->sv_sid)); break;
              case W_RECOVER:  print("waiting for site %d to recover, ", SITE_NO(svm->sv_sid)); break;
              default: print(" *** contains a sv_watch event code %d, site %d/%d *** ", svm->sv_event, SITE_NO(svm->sv_sid), SITE_INCARN(svm->sv_sid)); break;
            }
            print("Call ");
            cl_rname(svm->sv_proc);
            print("(%x)\n", svm->sv_arg);
        }
  }
