/*  $RCSfile: cl_groups.c,v $ $Revision: 2.62 $ $Date: 90/08/08 11:06:46 $  */
/*
 *	Originally coded by Ken Birman
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
 */
#include "isis.h"

static adesc gi_ad = { sizeof(ginfo), 0, 2 };

static ginfo *
gi_alloc()
{
	register ginfo *gip = ((ginfo *) mallocate(&gi_ad));

	gip->gi_gaddr = &NULLADDRESS;
	*gip->gi_gname = 0;
	gip->gi_join_verifier = (ifunc *) 0;
	gip->gi_client_verifier = (ifunc *) 0;
	gip->gi_lnode = 0;
	bclr(&gip->gi_lentries);
	bcopy(def_xfer_rcv, gip->gi_xfer_rcv, sizeof(def_xfer_rcv));
	bcopy(def_xfer_gen, gip->gi_xfer_gen, sizeof(def_xfer_gen));
	gip->gi_xbyref = 0;
	gip->gi_next = isis_groups;
	gip->gi_bypassq = qu_null();
	gip->gi_byprocs = qu_null();
	gip->gi_aborder = qu_null();
	gip->gi_clwatchq = qu_null();
	gip->gi_watching = qu_null();
	gip->gi_clwatchq->qu_viewid = 0;
	gip->gi_protosview = 0;
	gip->gi_bypassview = 0;
	gip->gi_bypassnew = 0;
	gip->gi_mutexview = 0;
	gip->gi_bypasswait = 0;
	gip->gi_parent = 0;
	gip->gi_myseqn = 0;
	gip->gi_bcseqn = 0;
	gip->gi_nextseqn = 0;
	gip->gi_view = 0;
	gip->gi_flag = GI_INUSE;
	gip->gi_flushcount = gip->gi_flushviewid = 0;
	gip->gi_activemsg = gip->gi_activebsnd = gip->gi_activebrcv = 0;
	bzero(gip->gi_bseqns, sizeof(gip->gi_bseqns));
	isis_groups = gip;
	return (gip);
}

void
gi_free(gip)
	register ginfo *gip;
{
	register ginfo *agip = gip;

	if (qu_head(gip->gi_bypassq))
		panic("gi_free: bypassq not empty");
	qu_free(gip->gi_bypassq);
	qu_free(gip->gi_byprocs);
	qu_free(gip->gi_aborder);
	qu_free(gip->gi_clwatchq);
	if (gip->gi_mutexview)
		isis_gv_free(gip->gi_mutexview);
	if (gip->gi_view)
		isis_gv_free(gip->gi_view);
	if (gip->gi_bypassview)
		isis_gv_free(gip->gi_bypassview);
	if (gip->gi_bypassnew)
		isis_gv_free(gip->gi_bypassnew);
	if (gip->gi_protosview)
		isis_gv_free(gip->gi_protosview);
	gip->gi_flag = 0;
	t_sig_all(&gip->gi_bypasswait, (VOID *) - 1);
	if (gip->gi_activebsnd + gip->gi_activemsg + gip->gi_activebrcv) {
		extern void bypass_inactivate();

		gip->gi_activebsnd = gip->gi_activemsg = gip->gi_activebrcv = 0;
		bypass_inactivate(gip);
	}
	mdeallocate((char *) gip, &gi_ad);
	if (isis_groups == gip)
		isis_groups = gip->gi_next;
	else
		for (gip = isis_groups; gip; gip = gip->gi_next)
			if (gip->gi_next == agip) {
				gip->gi_next = agip->gi_next;
				break;
			}
}

ginfo *
map_gname(gname)
	register char *gname;
{
	register ginfo *gip;

	if (*gname == '@') {
		while (*gname && *gname != ':')
			gname++;
		if (*gname)
			gname++;
	}
	for (gip = isis_groups; gip; gip = gip->gi_next)
		if (strcmp(gip->gi_gname, gname) == 0)
			return (gip);
	return ((ginfo *) 0);
}

ginfo *
map_gaddr(gaddrp)
	address *gaddrp;
{
	register ginfo *gip;
	address gaddr;

	gaddr = *gaddrp;
	gaddr.addr_entry = 0;
	for (gip = isis_groups; gip; gip = gip->gi_next)
		if (addr_isequal(&gaddr, gip->gi_gaddr))
			return (gip);
	return ((ginfo *) 0);
}

ginfo *
add_gname(gname)
	register char *gname;
{
	register ginfo *gip;

	if (*gname == '@') {
		while (*gname && *gname != ':')
			gname++;
		if (*gname)
			gname++;
	}
	if ((gip = map_gname(gname)) == (ginfo *) 0) {
		gip = gi_alloc();
		strcpy(gip->gi_gname, gname);
	}
	return (gip);
}

ginfo *
add_group(gname, gaddr)
	char *gname;
	address *gaddr;
{
	register ginfo *gip;
	register qnode *qp;

	for (gip = isis_groups; gip; gip = gip->gi_next)
		if (addr_isequal(gaddr, gip->gi_gaddr))
			break;
		else if (strcmp(gip->gi_gname, gname) == 0)
			break;
	if (gip == (ginfo *) 0)
		gip = gi_alloc();
	if ((qp = pg_find(ADDRS, gaddr)) == 0)
		qp = pg_add(ADDRS, gaddr, NULLARG, NULLROUTINE);
	qp->qu_pname.addr_entry = 0;
	gip->gi_gaddr = &qp->qu_pname;
	strcpy(gip->gi_gname, gname);
	return (gip);
}

/* Called if a join fails or this process leaves the group */
void
group_unmap(gip)
	register ginfo *gip;
{
	register ginfo *gop = 0, *gjp;

	for (gjp = isis_groups; gjp; gjp = gjp->gi_next)
		if (gjp == gip)
			break;
		else
			gop = gjp;
	if (gjp == (ginfo *) 0)
		return;
	if (gop)
		gop->gi_next = gip->gi_next;
	else
		isis_groups = gip->gi_next;
	gi_free(gip);
}

static adesc gv_ad = { sizeof(groupview), 0, 8 };

#define	gv_allocate()		((groupview*)mallocate(&gv_ad))
#define	gv_deallocate(gv)	mdeallocate((char*)gv, &gv_ad)

groupview *
isis_gv_alloc()
{
	register groupview *gv;

	gv = gv_allocate();
	gv->gv_flag = 0;
	gv->gv_refcount = 1;
	return (gv);
}

void
isis_gv_free(gv)
	register groupview *gv;
{
	if (--gv->gv_refcount == 0)
		gv_deallocate(gv);
}
