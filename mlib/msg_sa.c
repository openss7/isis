/*  $RCSfile: msg_sa.c,v $ $Revision: 2.15 $ $Date: 90/08/06 14:13:41 $  */
/*
 *	Originally coded by Ken Birman
 *	Enables stand-alone use of message library 
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
#define   ISIS_SYS
# include "isis.h"

int my_site_no, my_site_incarn, my_process_id, my_port_no, my_genid;
site_id my_site_id;
address NULLADDRESS;
char *isis_dir, *isis_joining;
int isis_errno, isis_nsent, isis_nreplies, act_blocked, isis_forkcnt;
int intercl_socket, isis_state, isis_created, isis_switched, isis_nblock, bypass_lagging;
int ISIS_TIME, isis_enum, act_bits, isis_never, isis_socket;
qnode *bypass_waitq, *ADDRS, *isis_wlist, *isis_swlist, *isis_pwlist, *isis_pgmon;
qnode *qu_freelist;
ginfo *isis_gip, *isis_groups;
event_id *isis_eid;
task *isis_scheduler;
int msg_usagestats, msg_tracemsgs, msg_tracecaller, memfree, nfree, memalloc, nalloc;
long msg_namsgs, msg_nfmsgs;
long msg_nalloc, msg_nfree, msg_memused, msg_memfree;
u_short msg_shortmask;
u_long msg_longmask;

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

/* add a node to qp, returns np */
qnode *
qu_add(qp, name, data, routine)
	register qnode *qp;
	int name;
	VOID *data;
	vfunc *routine;
{
	register qnode *np;

	qu_alloc1(np, name, data, routine);
	qu_append(qp, np);
	return (np);
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

void
panic(fmt, a0, a1, a2, a3, a4, a5)
	char *fmt;
	int a0, a1, a2, a3, a4, a5;
{
	fprintf(stderr, "ISIS stand-alone message library: PANIC (");
	fprintf(stderr, fmt, a0, a1, a2, a3, a4, a5);
	fprintf(stderr, "\n");
	exit(0);
}

void
pmsg(msg)
	message *msg;
{
}

void
print(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
	char *fmt;
	int a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	printf(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
	fflush(stdout);
}

char *
callername(n_levels)
	int n_levels;
{
	return ((char *) 0);
}

void
isis_has_crashed(how)
	int how;
{
}

void
t_scheck()
{
}

unsigned
isis_sleep(n)
	unsigned n;
{
	return sleep(n);
}

void
paddrs(addr)
	address *addr;
{
	if (addr == 0) {
		print("( ??? )");
		return;
	}
	while (addr->addr_site)
		paddr(addr++);
}

void
paddr(addr)
	address *addr;
{
	if (addr == 0) {
		print("(null addr ptr)");
		return;
	}
	print("(");
	switch (addr->addr_portno) {
	case ISAGID:
		print("gaddr=%d/%d.%d[%d])", addr->addr_site, addr->addr_incarn, addr->addr_groupid,
		      addr->addr_entry);
		return;
	case ISACT:
		print("act=");
	default:
		switch (addr->addr_process) {
		case PROTOCOLS:
			print("%d/%d:pr.%d)", addr->addr_site, addr->addr_incarn, addr->addr_entry);
			break;
		case REXEC:
			print("%d/%d:rexec.%d)", addr->addr_site, addr->addr_incarn,
			      addr->addr_entry);
			break;
		default:
			print("%d/%d:%d.%d)", addr->addr_site, addr->addr_incarn,
			      addr->addr_process, addr->addr_entry);
			break;
		}
	}
}
