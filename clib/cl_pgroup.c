/*  $RCSfile: cl_pgroup.c,v $ $Revision: 2.103 $ $Date: 90/09/11 15:37:08 $  */
/*
 *	Originally coded by Ken Birman
 *      Client entry routines in ISIS: unpack args and dispatch request
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
condition isis_wantleave;		/* Wants to leave */

static void
refresh_gaddr_cache(gaddrp, event, name)
  register address *gaddrp;
  int event;
  register char *name;
  {
	register qnode *qp;
	for(qp = GNAMES->qu_next; qp != GNAMES; qp = qp->qu_next)
	    if(strcmp(qp->qu_string, name) == 0)
	    {
	        free(qp->qu_string);
	        qu_free(qp);
		return;
	    }
  }

address*
pg_lookup(name)
  char *name;
  {
        register message *msg;
        address result;
        address *gaddr, *pg_local_lookup();
	register qnode *qp;
        register len;

	ISIS_ENTER();
        if((len = strlen(name)) > PG_GLEN-1)
            name[len = PG_GLEN-1] = 0;
        gaddr = pg_local_lookup(name);
        if(!aptr_isnull(gaddr))
            ISIS_RETURN(gaddr);
	for(qp = GNAMES->qu_next; qp != GNAMES; qp = qp->qu_next)
	    if(strcmp(qp->qu_string, name) == 0)
	        ISIS_RETURN((address*)qp->qu_data);
        msg = msg_newmsg();
        msg_addfield(msg, CL_GNAME, name, FTYPE_CHAR, len+1);
        if(isis(CL_LOOKUP, msg, (char*)&result, sizeof(address)) != sizeof(address))
            panic("pg_lookup: no result returned");
        msg_delete(msg);
	if(addr_isnull(&result))
	    return(&NULLADDRESS);
	if((qp = pg_find(ADDRS, &result)) == 0)
	{
	    register char *copy;
	    register len = strlen(name)+1;
	    static init;
	    qp = pg_add(ADDRS, &result, NULLARG, NULLROUTINE);
	    copy = (char*)malloc((len+3) & ~0x3);
	    bcopy(name, copy, len);
	    qu_add(GNAMES, (int)copy, (char*)&qp->qu_pname, NULLROUTINE);
	    if(isis_joining == 0)
	    {
	        if(init++ == 0)
	            isis_task(refresh_gaddr_cache, "pg_lookup:refresh_gaddr_cache");
	        pg_detect_failure(&result, refresh_gaddr_cache, copy);
	    }
	}
        ISIS_RETURN(&qp->qu_pname);
  }

gl_desc *
pg_list(gname)
  char *gname;
  {
        gl_desc *glp = 0;
        register message *mp = msg_genmsg(CL_GNAME, gname, FTYPE_CHAR, strlen(gname)+1, 0);
        isis(CL_PGLIST, mp, (char*)&glp, AMALLOC);
        msg_delete(mp);
        return(glp);
  }

int
pg_delete(gaddr)
  address *gaddr;
  {
        message *msg = msg_newmsg();
        address all_members[2];
        char answ[MAX_PROCS];
        int rval;

	ISIS_ENTER();
        if(pg_rank(gaddr, &my_address) == -1)
            ISIS_RETURN(-1);
        all_members[0] = ADDRESS(-1, -1, -1, -1);
        all_members[1] = NULLADDRESS;
        msg_addfield(msg, CL_GID, (char*)gaddr, FTYPE_ADDRESS, sizeof(address));
        msg_addfield(msg, CL_PNAME, (char*)all_members, FTYPE_ADDRESS, sizeof(all_members));
        msg_insertfield(msg, SYSFLD_VCHANGE, NULLARG, FTYPE_CHAR, 0);
        rval = gbcast_l("sg", gaddr, GENERIC_DELETE, msg, ALL, "%c", answ);
        msg_delete(msg);
        if(rval == 0)
        {
            isis_errno = IE_TOTFAIL;
            ISIS_RETURN(-1);
        }
        ISIS_RETURN(0);
  }

static qnode *leaving_list;
static vfunc *old_filter;

static void
leaving_check(mp)
  register message *mp;
  {
	register address *ap, *bp;
	if(qu_head(leaving_list))
	{
	    register address *ep;
	    if(!addr_isnull(ap = &mp->msg_hdr->hdr_dest))
		ep = ap+1;
	    else if(ap = msg_getalist(mp))
		ep = 0;
	    if(ap)
	    {
	        for(; ap != ep && !aptr_isnull(ap); ++ap)
		{
		    register groupview *gv;
		    if(addr_ismine(ap))
		        goto ok;
		    if(!addr_isgid(ap))
		        continue;
		    if(pg_find(leaving_list, ap))
		        continue;
		    if((gv = pg_getlocalview(ap)) == (groupview*)0)
		        continue;
		    for(bp = gv->gv_members; !aptr_isnull(bp); bp++)
		        if(addr_ismine(bp))
			    goto ok;
	        }
                nullreply(mp);
	        return;
	    }
	}
   ok:
	ISISCALL1(old_filter, mp);
  }


int
pg_leave(gaddr)
  address *gaddr;
  {
        message *msg = msg_newmsg();
        static address name[2];
        char answ[MAX_PROCS];
        int rval;
	static leaving;
	register qnode *qp;

	ISIS_ENTER();
	while(isis_state&ISIS_COORD)
	    (void)t_wait_l(&isis_wantleave, "pg_leave: waiting for coordinator to finish");
	if(leaving++ == 0)
	{
	    isis_state |= ISIS_LEAVING;
	    if(leaving_list == (qnode*)0)
	    {
		leaving_list = qu_null();
	        old_filter = isis_setfilter((vfunc *) leaving_check);
	    }
	}
	qp = pg_add(leaving_list, gaddr, NULLARG, NULLROUTINE);
        name[0] = my_address;
        msg_addfield(msg, CL_GID, (char*)gaddr, FTYPE_ADDRESS, sizeof(address));
        msg_addfield(msg, CL_PNAME, (char*)name, FTYPE_ADDRESS, sizeof(name));
        msg_insertfield(msg, SYSFLD_VCHANGE, NULLARG, FTYPE_CHAR, 0);
        rval = gbcast_l("sg", gaddr, GENERIC_DELETE, msg, ALL, "%c", answ);
        msg_delete(msg);
	qu_free(qp);
	if(--leaving == 0)
	    isis_state &= ~ISIS_LEAVING;
        if(rval == 0)
        {
            isis_errno = IE_TOTFAIL;
            ISIS_RETURN(-1);
        }
        ISIS_RETURN(0);
  }

int
pg_addclient(gaddr, pname)
  address *gaddr, *pname;
  {
        register message *msg = msg_newmsg();
        static address addrs[3], name[2];
        char answ[MAX_PROCS];
        int rval;

	ISIS_ENTER();
        name[0] = *pname;
        msg_addfield(msg, CL_GID, (char*)gaddr, FTYPE_ADDRESS, sizeof(address));
        msg_addfield(msg, CL_PNAME, (char*)name, FTYPE_ADDRESS, sizeof(name));
        addrs[0] = *gaddr;
        addrs[0].addr_entry = GENERIC_ADDCLIENT;
        addrs[1] = *pname;
        addrs[1].addr_entry = GENERIC_ADDCLIENT;
        msg_insertfield(msg, SYSFLD_VCHANGE, NULLARG, FTYPE_CHAR, 0);
        rval = gbcast_grow("lsg", addrs, msg, ALL, "%c", answ);
        msg_delete(msg);
        if(rval == 0)
        {
            isis_errno = IE_TOTFAIL;
            ISIS_RETURN(-1);
        }
        ISIS_RETURN(0);
  }

int
pg_delclient(gaddr, pname)
  address *gaddr, *pname;
  {
        message *msg = msg_newmsg();
        static address name[2];
        char answ[MAX_PROCS];
        int rval;

	ISIS_ENTER();
        name[0] = *pname;
        msg_addfield(msg, CL_GID, (char*)gaddr, FTYPE_ADDRESS, sizeof(address));
        msg_addfield(msg, CL_PNAME, (char*)name, FTYPE_ADDRESS, sizeof(name));
        msg_insertfield(msg, SYSFLD_VCHANGE, NULLARG, FTYPE_CHAR, 0);
        rval = gbcast_l("sg", gaddr, GENERIC_DELETE, msg, ALL, "%c", answ);
        msg_delete(msg);
        if(rval == 0)
        {
            isis_errno = IE_TOTFAIL;
            ISIS_RETURN(-1);
        }
        ISIS_RETURN(0);
  }


int
pg_signal(gaddr, signo)
  address *gaddr;
  int signo;
  {
        message *msg = msg_newmsg();
        char answ[MAX_PROCS];
        int rval;

	ISIS_ENTER();
        msg_addfield(msg, CL_SIGNO, (char*)&signo, FTYPE_LONG, sizeof(int));
        msg_addfield(msg, CL_GID, (char*)gaddr, FTYPE_ADDRESS, sizeof(address));
        rval = gbcast_l("s", gaddr, GENERIC_SIGNAL, msg, ALL, "%c", answ);
        msg_delete(msg);
        if(rval == 0)
        {
            isis_errno = IE_TOTFAIL;
            ISIS_RETURN(-1);
        }
        ISIS_RETURN(0);
  }

struct  pwatch
  {
        int         pw_uid;
        int         pw_act;
        vfunc       *pw_proc;
        int         pw_viewid;
        int         pw_xbyref;
        char        *pw_arg;
        groupview   *pw_gv;
  };

static adesc    pw_adesc ={ sizeof(pwatch), 0, 8, };

#define pw_alloc()        ((pwatch*)mallocate(&pw_adesc))

void
pw_free(pw)
  pwatch *pw;
  {
        act_ev(-1, pw->pw_act, ACT_WATCH);
        mdeallocate((char*)pw, &pw_adesc);
  }

static  PWID;

groupview *pg_getlocalview();

int
pg_monitor(gaddr, routine, arg)
  address *gaddr;
  vfunc *routine;
  VOID *arg;
  {
        register pwatch *pw = pw_alloc();
        register groupview *gv;
        register act;
	ISIS_ENTER();
	act = isis_ctp->task_act;
        if(isis_state&ISIS_STARTUP)
            act = 0;
        pw->pw_uid = ++PWID;
        pw->pw_viewid = 0;
        pw->pw_act = act;
        pw->pw_xbyref = isis_state&ISIS_XBYREF;
        act_ev(1, pw->pw_act, ACT_WATCH);
        pw->pw_proc = routine;
        pw->pw_arg = arg;
        (void)pg_add_pw(isis_pgmon, gaddr, pw, (vfunc *) pw_free);
        if(isis_joining)
        {
            register pwatch *npw;
            register ginfo *gip = map_gaddr(gaddr);
            gv = 0;
            if(gip && gip->gi_view && (gip->gi_view->gv_flag&PG_FIRST))
                gv = gip->gi_view;
            if(gv == 0 && (gv = pg_getlocalview(gaddr)) == 0)
		/* In BYPASS mode this can happen */
                ISIS_RETURN(PWID);
            pw->pw_viewid = gv->gv_viewid;
	    npw = pw_alloc();
            npw->pw_proc = routine;
            npw->pw_arg = arg;
            npw->pw_gv = gv;
            ++gv->gv_refcount;
            npw->pw_act = act;
            npw->pw_xbyref = isis_state&ISIS_XBYREF;
            act_ev(1, npw->pw_act, ACT_WATCH);
            BEGINFROMC;
                t_fork_urgent((vfunc *) pg_pwatch_invoke, npw);
            ENDFROMC;
        }
	else
	    pw->pw_gv = (groupview*)0;
        ISIS_RETURN(PWID);
  }

int
pg_monitor_act(gaddr, routine, arg)
  address *gaddr;
  vfunc *routine;
  VOID *arg;
  {
        register pwatch *pw;
        register groupview *gv;
	pw = pw_alloc();
        pw->pw_uid = ++PWID;
        pw->pw_act = isis_ctp->task_act;
	pw->pw_xbyref = isis_state&ISIS_XBYREF;
        act_ev(1, pw->pw_act, ACT_WATCH);
        pw->pw_proc = routine;
        pw->pw_arg = arg;
        (void)pg_add_pw(isis_pgmon, gaddr, pw, (vfunc *) pw_free);
        if(isis_joining && (gv = pg_getlocalview(gaddr)))
        {
            register pwatch *npw = pw_alloc();
            pw->pw_viewid = gv->gv_viewid;
            npw->pw_proc = routine;
            npw->pw_arg = arg;
            npw->pw_gv = gv;
            ++gv->gv_refcount;
            npw->pw_act = isis_ctp->task_act;
	    npw->pw_xbyref = isis_state&ISIS_XBYREF;
            act_ev(1, npw->pw_act, ACT_WATCH);
            BEGINFROMC;
                t_fork_urgent((vfunc *) pg_pwatch_invoke, npw);
            ENDFROMC;
        }
	else
	    pw->pw_gv = (groupview*)0;
        return(PWID);
  }

void
pg_unmonitor(gaddr)
  address *gaddr;
  {
        register qnode *pw;
	ISIS_ENTER();
        while(pw = pg_find(isis_pgmon, gaddr))
            qu_free(pw);
	ISIS_EXIT();
  }

int
pg_monitor_cancel(pwid)
  int pwid;
  {
        register qnode *qp;
	ISIS_ENTER();
        for(qp = isis_pgmon->qu_next; qp != isis_pgmon; qp = qp->qu_next)
        {
            register pwatch *pw = qp->qu_pwatch;
            if(pw->pw_uid == pwid)
            {
                qu_free(qp);
                ISIS_RETURN(0);
            }
        }
        isis_errno = IE_BADARG;
        ISIS_RETURN(-1);
  }

void
pg_pwatch_invoke(pw)
  register pwatch *pw;
  {
        isis_ctp->task_routine = pw->pw_proc;
        isis_ctp->task_arg0 = pw->pw_arg;
        isis_ctp->task_act = pw->pw_act;
        if(pw->pw_xbyref)
            ISISCALL2(pw->pw_proc, &pw->pw_gv, &pw->pw_arg);
        else
            ISISCALL2(pw->pw_proc, pw->pw_gv, pw->pw_arg);
	if(pw->pw_gv)
            isis_gv_free(pw->pw_gv);
        pw_free(pw);
  }

void
pg_new_view(gv)
  register groupview *gv;
  {
        register qnode *qp, *nqp;
        for(qp = isis_pgmon->qu_next; qp != isis_pgmon; qp = nqp)
        {
            nqp = qp->qu_next;
            if(act_blocked && qp->qu_pwatch->pw_act != act_blocked)
                continue;
            if(addr_cmp(&qp->qu_pname, &gv->gv_gaddr) == 0 && qp->qu_pwatch->pw_viewid < gv->gv_viewid)
            {
                register pwatch *pw = pw_alloc();
                qp->qu_pwatch->pw_viewid = gv->gv_viewid;
                *pw = *qp->qu_pwatch;
                pw->pw_gv = gv;
                ++gv->gv_refcount;
                act_ev(1, pw->pw_act, ACT_WATCH);
                t_fork((vfunc *) pg_pwatch_invoke, pw);
            }
        }
	if(act_blocked == 0 && gv->gv_nmemb == 0)
	    for(qp = GNAMES->qu_next; qp != GNAMES; qp = qp->qu_next)
	        if(strcmp(qp->qu_string, gv->gv_name) == 0)
	        {
	            free(qp->qu_string);
	            qu_free(qp);
		    break;
	        }
        w_new_view(gv);
  }

void
pg_monitor_dump()
  {
        register qnode *qp;
        for(qp = isis_pgmon->qu_next; qp != isis_pgmon; qp = qp->qu_next)
        {
            register pwatch *pw = qp->qu_pwatch;
            print("  [act %d wid %x]: Monitor gaddr ", pw->pw_act, pw->pw_uid);
            paddr(&qp->qu_pname);
            print(". Call ");
            cl_rname((vfunc *) pw->pw_proc); 
            print("(%x)\n", pw->pw_proc, pw->pw_arg);
        }   
  }
