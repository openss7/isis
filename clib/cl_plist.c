/*  $RCSfile: cl_plist.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:17 $  */
/*
 *	Coded by Ken Birman
 *      Process group member subsets
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

#ifdef FUN_TYPES
   static  void pl_monitor(groupview *gv, ginfo *pip);
#else  FUN_TYPES
   static  void pl_monitor();
#endif FUN_TYPES

static	PLCOUNT;

static void
pl_monitor(gv, pip)
  register groupview *gv;
  register ginfo *pip;
  {
	register address *addr;
	if(!aptr_isnull(addr))
	{
	    register address *ap, *bp;
	    for(ap = bp = pip->gi_view->gv_members; !aptr_isnull(ap); ap++)
	        if(!addr_isequal(ap, addr))
		    if(ap != bp++)
		        bp[-1] = *ap;
	    *bp = NULLADDRESS;
	}
	pip->gi_view->gv_viewid = gv->gv_viewid;
  }

address *
pl_create(gaddr, addrs)
  address *gaddr, *addrs;
  {
	static first_time;
	address *list_p;
	register address *ap, *bp;
	groupview *isis_gv_alloc();
	register groupview *pv, *gv;
	
	register ginfo *gip, *pip;
	if((gip = map_gaddr(gaddr)) == 0)
        {
	    isis_errno = IE_UNKNOWN;
	    return(&NULLADDRESS);
	}
	gv = act_blocked? gip->gi_mutexview: gip->gi_view;
	if(gv == 0)
	{
	    isis_errno = IE_UNKNOWN;
	    return(&NULLADDRESS);
	}
	else
	{
	    address pladdr;
	    register address *gp;
	    register qnode *qp;
	    for(ap = bp = addrs; !aptr_isnull(ap); ap++)
	    {
		for(gp = gip->gi_view->gv_members; !aptr_isnull(gp); gp++)
		    if(addr_isequal(ap, gp))
			break;
		if(!aptr_isnull(gp) && ap != bp++)
		    bp[-1] = *ap;
	    }
	    if(bp == addrs)
	    {
		isis_errno = IE_EMPTY;
		return(&NULLADDRESS);
 	    }
	    *bp = NULLADDRESS;
	    pladdr = my_address;
	    pladdr.addr_portno = ISPLIST;
	    pladdr.addr_incarn = ++PLCOUNT;
	    if((qp = pg_find(ADDRS, &pladdr)) == (qnode*)0)
	        qp = pg_add(ADDRS, &pladdr, NULLARG, NULLROUTINE);
	    list_p = &qp->qu_pname;
	}
	pip = add_group("*", list_p);
	pip->gi_parent = gip;
	strcpy(pip->gi_gname+1, gip->gi_gname);
	pv = isis_gv_alloc();
	++pv->gv_refcount;
	pip->gi_view = pip->gi_mutexview = pv;
	ap = addrs;
	bp = pv->gv_members;
	while(!aptr_isnull(ap))
	    *bp++ = *ap++;
	*bp = NULLADDRESS;
	pv->gv_departed = NULLADDRESS;
	pv->gv_joined = NULLADDRESS;
	pv->gv_viewid = gip->gi_view->gv_viewid;
	pv->gv_flag = gip->gi_view->gv_flag;
	if(first_time == 0)
	{
	    ++first_time;
	    isis_task((vfunc *) pl_monitor, "plist:pl_monitor");
	}
	pip->gi_mid = pg_monitor(gaddr, (vfunc *) pl_monitor, (VOID*)pip);
	return(list_p);
  }

address *
pl_makegroup(list_p, gname)
  register address *list_p;
  register char *gname;
  {
	register ginfo *pip = map_gaddr(list_p);
	address *pg_subgroup();
	return(pg_subgroup(pip->gi_parent->gi_gaddr, gname, 0, pip->gi_view->gv_members, (address*)0));
  }

void
pl_add(list_p, addr_p)
  register address *list_p, *addr_p;
  {
	register ginfo *pip = map_gaddr(list_p);
	register address *ap;
	register groupview *gv;
	if(pip == (ginfo*)0)
	    return;
	/* Valid member of parent group? */
	gv = act_blocked? pip->gi_parent->gi_mutexview: pip->gi_parent->gi_view;
	for(ap = gv->gv_members; !aptr_isnull(ap); ap++)
	    if(addr_isequal(ap, addr_p))
		break;
	if(addr_isnull(ap))
	    return;
	/* Already on list? */
	for(ap = pip->gi_view->gv_members; !aptr_isnull(ap); ap++)
	    if(addr_isequal(ap, addr_p))
		return;
	*ap++ = *addr_p;
	*ap = NULLADDRESS;
  }

void
pl_remove(list_p, addr)
  register address *list_p, *addr;
  {
	register ginfo *pip = map_gaddr(list_p);
	register address *ap, *bp;
	if(pip == (ginfo*)0)
	    return;
	for(ap = bp = pip->gi_view->gv_members; !aptr_isnull(ap); ap++)
	    if(addr_isequal(ap, addr))
		continue;
	    else if(ap != bp++)
		bp[-1] = *ap;
	*bp = NULLADDRESS;
  }

void
pl_delete(list_p)
  register address *list_p;
  {
	register ginfo *pip = map_gaddr(list_p);
	register qnode *qp;
	if(pip == (ginfo*)0)
	    return;
	if(qp = pg_find(ADDRS, pip->gi_gaddr))
	    qu_free(qp);
	group_unmap(pip);
  }
