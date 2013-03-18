/*  $RCSfile: cl_queues.c,v $ $Revision: 2.51 $ $Date: 90/08/06 13:53:36 $  */
/*
 *	Originally coded by Ken Birman
 *      Queue manipulation routines
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

qnode *qu_freelist, *qu_newqp;
adesc qu_adesc = { sizeof(qnode), 0, 16 };

qnode *
qu_alloc(name, data, routine)
	int name;
	VOID *data;
	vfunc *routine;
{
	register qnode *qp;

	qu_alloc3(qp, name, data, 0, 0, routine);
	return (qp);
}

/* Create a new node or a new qnode.  */
qnode *
qu_alloc_pg(name, paddr)
	int name;
	address *paddr;
{
	register qnode *qp;

	qu_alloc3(qp, 0, 0, 0, 0, 0);
	qp->qu_address = *paddr;
	return (qp);
}

/* add a node to qp, returns np */
qnode *
qu_add(qp, name, data, routine)
	int name;
	register qnode *qp;
	VOID *data;
	vfunc *routine;
{
	register qnode *np;

	qu_alloc1(np, name, data, routine);
	qu_append(qp, np);
	return (np);
}

qnode *
qu_add_pg(qp, name, paddr)
	int name;
	register qnode *qp;
	address *paddr;
{
	register qnode *np;

	qu_alloc3(np, 0, 0, 0, 0, 0);
	np->qu_address = *paddr;
	qu_append(qp, np);
	return (np);
}

qnode *
qu_add_cb(qp, proc, arg0, arg1)
	register qnode *qp;
	void (*proc) ();
	VOID *arg0, *arg1;
{
	register qnode *np;

	qu_alloc3(np, (int) proc, arg0, arg1, 0, NULLROUTINE);
	qu_append(qp, np);
	return (np);
}

qnode *
qu_add_sid(qp, name, sid)
	register qnode *qp;
	int name;
	site_id sid;
{
	register qnode *np;

	qu_alloc3(np, name, 0, 0, 0, NULLROUTINE);
	np->qu_sid = sid;
	qu_append(qp, np);
	return (np);
}

/* Resort qnode qp when name of node np has changed */
void
qu_resort(qp, np)
	register qnode *qp, *np;
{
	register qnode *op;

	qu_remove(np);
	op = qp->qu_next;
	while (op != qp) {
		if (op->qu_name > np->qu_name)
			break;
		op = op->qu_next;
	}
	op->qu_last->qu_next = np;
	np->qu_last = op->qu_last;
	op->qu_last = np;
	np->qu_next = op;
}

/* Free a qnode */
void
qu_freeall(qp)
	register qnode *qp;
{
	register qnode *np, *nnp;

	if (qp == 0)
		return;
	nnp = qp->qu_next;
	do {
		np = nnp;
		nnp = np->qu_next;
		qu_free(np);
	}
	while (np != qp);
}

qnode *
qu_find(qp, item)
	register qnode *qp;
	int item;
{
	register qnode *np;

	if (qp == 0)
		return (0);
	np = qp;
	while (np->qu_name != item && (np = np->qu_next) != qp) ;
	if (np->qu_name != item)
		return (NULLQP);
	return (np);
}

/* Create a new node or a new qnode.  */
qnode *
pg_alloc(pname, data, routine)
	register address *pname;
	VOID *data;
	vfunc *routine;
{
	register qnode *qp;

	qu_alloc3(qp, 0, data, 0, 0, routine);
	qp->qu_pname = *pname;
	qp->qu_pname.addr_entry = 0;
	return (qp);
}

/* add a node to qp, returns np */
qnode *
pg_add(qp, pname, data, routine)
	register address *pname;
	register qnode *qp;
	VOID *data;
	vfunc *routine;
{
	register qnode *np;

	qu_alloc3(np, 0, data, 0, 0, routine);
	np->qu_pname = *pname;
	np->qu_pname.addr_entry = 0;
	qu_append(qp, np);
	return (np);
}

qnode *
pg_find(qp, pname)
	register address *pname;
	register qnode *qp;
{
	register qnode *np;

	if ((np = qp) == NULLQP)
		return (NULLQP);
	while (addr_cmp(&np->qu_pname, pname) && (np = np->qu_next) != qp)
		continue;
	if (addr_cmp(&np->qu_pname, pname))
		return (NULLQP);
	if (np == qp && addr_isnull(pname))
		return (NULLQP);
	return (np);
}
