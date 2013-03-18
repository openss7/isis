/*  $RCSfile: pr_queues.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:06 $  */
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
 *
 *
 */

#include "pr.h"

adesc qu_adesc = { sizeof(qnode), 0, 16 };

qnode *qu_freelist, *qu_newQP;

qnode *
qu_alloc(name, data, routine)
	char *data;
	ifunc *routine;
{
	register qnode *qp;

	qu_alloc1(qp, name, data, routine);
	return (qp);
}

/* add a node to qp, returns np */
qnode *
qu_add(qp, name, data, routine)
	register qnode *qp;
	char *data;
	ifunc *routine;
{
	register qnode *np, *op;
	extern qnode *newnode, *oldnode;
	extern ifunc *oldroutine;

	qu_alloc3(np, name, data, 0, 0, routine);
	op = qp->qu_last;
	op->qu_next = np;
	np->qu_next = qp;
	np->qu_last = op;
	qp->qu_last = np;
	return (np);
}

qnode *
qu_add_cb(qp, proc, arg0, arg1)
	register qnode *qp;
	int (*proc) ();
	char *arg0, *arg1;
{
	register qnode *np;

	qu_alloc3(np, (int) proc, arg0, arg1, 0, nullroutine);
	qu_append(qp, np);
	return (np);
}

qnode *
qu_add_sid(qp, name, sid)
	register qnode *qp;
	site_id sid;
{
	register qnode *np;

	qu_alloc3(np, name, 0, 0, 0, nullroutine);
	np->qu_sid = sid;
	qu_append(qp, np);
	return (np);
}

qnode *
qu_add_bits(qp, name)
	register qnode *qp;
{
	register qnode *np;

	qu_alloc1(np, name, 0, nullroutine);
	bclr(&np->qu_bitvec);
	qu_append(qp, np);
	return (np);
}

/* Resort qnode qp when name of node np has changed */
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
{
	register qnode *np;

	if (qp == 0)
		return (0);
	np = qp;
	while (np->qu_name != item && (np = np->qu_next) != qp) ;
	if (np->qu_name != item)
		return ((qnode *) 0);
	return (np);
}

/* Create a new node or a new qnode.  */
qnode *
pg_alloc(pname, data, routine)
	register address *pname;
	char *data;
	ifunc *routine;
{
	register qnode *qp;

	qu_allocpg(qp, (*pname), data, routine);
	return (qp);
}

/* add a node to qp, returns np */
qnode *
pg_add(qp, pname, data, routine)
	register address *pname;
	register qnode *qp;
	char *data;
	ifunc *routine;
{
	register qnode *np;

	qu_allocpg(np, (*pname), data, routine);
	qu_append(qp, np);
	return (np);
}

#define	spin

qnode *
pg_find(qp, pname)
	register address *pname;
	register qnode *qp;
{
	register qnode *np;

	np = qp;
	while (addr_cmp(&np->qu_pname, pname) && (np = np->qu_next) != qp)
		spin;
	if (addr_cmp(&np->qu_pname, pname))
		return ((qnode *) 0);
	return (np);
}
