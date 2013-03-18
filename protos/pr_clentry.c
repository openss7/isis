/*  $RCSfile: pr_clentry.c,v $ $Revision: 2.1 $ $Date: 90/08/14 10:00:35 $  */
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
 *
 *
 */
#include "pr.h"

#ifndef	MACH
char *malloc();
#endif
int collect_msgs();

cl_create(mp)
	register message *mp;
{
	address *mlist, gid, pg_create();
	char *gname;
	int incarn;

	mlist = (address *) msg_getfield(mp, CL_PNAME, 1, (int *) 0);
	gname = msg_getfield(mp, CL_GNAME, 1, (int *) 0);
	incarn = *(int *) msg_getfield(mp, CL_INCARN, 1, (int *) 0);
	gid = pg_create(gname, mlist, incarn);
	reply(mp, (char *) &gid, FTYPE_ADDRESS, sizeof(address));
}

cl_pglookup(mp)
	register message *mp;
{
	char *gname;
	address gid, pg_lookup();

	gname = msg_getfield(mp, CL_GNAME, 1, (int *) 0);

	gid = pg_lookup(gname);
	reply(mp, (char *) &gid, FTYPE_ADDRESS, sizeof(address));
}

cl_pglist(mp)
	register message *mp;
{
	register gl_desc *glp, *gp, *rp, **rrp;
	register char *gname, *scope;
	register len, n, nreps;
	site_id slist[MAX_SITES];
	char *answ[MAX_SITES];
	message *msg;

	gname = msg_getfield(mp, CL_GNAME, 1, 0);
	if (*gname == '@') {
		scope = ++gname;
		while (*gname && *gname != ':')
			++gname;
		if (*gname)
			*gname++ = 0;
	} else
		scope = 0;
	if ((len = strlen(gname)) >= PG_GLEN)
		len = PG_GLEN - 1;
	gname[len++] = 0;
	sl_scope(scope, slist);
	msg = msg_genmsg(GMGR_GNAME, gname, FTYPE_CHAR, len, 0);
	nreps = BCAST_SL(slist, PROTOCOLS, PR_PGLIST, msg, ALL, collect_answ, answ, AMALLOC);
	msg_delete(msg);
	n = nreps;
	rrp = (gl_desc **) answ;
	len = sizeof(gl_desc);
	while (n--) {
		rp = *rrp++;
		while (!aptr_isnull(&rp->gl_addr)) {
			len += sizeof(gl_desc);
			++rp;
		}
	}
	glp = (gl_desc *) malloc(len);
	bzero(glp, len);
	n = nreps;
	rrp = (gl_desc **) answ;
	while (n--) {
		rp = *rrp++;
		while (!aptr_isnull(&rp->gl_addr)) {
			for (gp = glp; !aptr_isnull(&gp->gl_addr); gp++)
				if (addr_isequal(&gp->gl_addr, &rp->gl_addr))
					break;
			if (aptr_isnull(&gp->gl_addr))
				*gp = *rp;
			else
				len -= sizeof(gl_desc);
			++rp;
		}
	}
	reply(mp, (char *) glp, FTYPE_GLDESC, len);
	free(glp);
}

cl_wantflush(mp)
	message *mp;
{
	cb_procflush(msg_getsender(mp)->addr_process);
	reply(mp, "+", FTYPE_CHAR, 1);
}

cl_wantdump(mp)
	register message *mp;
{
	register how = *(int *) msg_getfield(mp, CL_HOW, 1, (int *) 0);
	char *file = msg_getfield(mp, CL_FILE, 1, (int *) 0);
	char answ[MAX_SITES];

	if (how == -1) {
		message *msg = msg_newmsg();

		BCAST_V(&current_view, PROTOCOLS, PR_WANTDUMP, msg, ALL, collect_answ, answ, 1);
		msg_delete(msg);
	} else
		cl_prdump(file);
	reply(mp, "+", FTYPE_CHAR, 1);
}

pr_wantdump(mp)
	register message *mp;
{
	message *msg = msg_newmsg();

	cl_prdump((char *) 0);
	cl_send_all(GENERIC_WANTDUMP, msg);
	msg_delete(msg);
	reply(mp, "+", FTYPE_CHAR, 1);
}

cl_prdump(file)
	char *file;
{
	register save;

	if (file) {
		fflush(stdout);
		save = dup(fileno(stdout));
		close(fileno(stdout));
		open(file, O_WRONLY | O_TRUNC | O_CREAT, 0666);
	}
	pr_dump(DUMP_ALL, "ISIS protocols process status dump");
	if (file) {
		fflush(stdout);
		close(fileno(stdout));
		(void) dup(save);
		close(save);
	}
}

cl_fdrestart(mp)
	message *mp;
{
	int how = *(int *) msg_getfield(mp, CL_RESTARTMODE, 1, (int *) 0);
	site_id coord = *(site_id *) msg_getfield(mp, CL_COORD, 1, (int *) 0);

	fd_restart(how, coord);
	/* Send init the current site view.  It needs this because it starts up in an unsual way */
	reply(mp, &current_view, FTYPE_SVIEW, sizeof(sview));
}

cl_echo(mp)
	message *mp;
{
	int len;
	char *where = msg_getfield(mp, 1, 1, &len);

	if (where)
		reply(mp, where, FTYPE_CHAR, len);
	else
		reply(mp, 0, 0, 0);
}

do_clbcast(mp, broutine)
	register message *mp;
	int (*broutine) ();
{
	address *alist;
	message *msg;
	int nwant, alen;
	bitvec scope;

	if (msg_get(mp, "%-A,%m,%d", &alist, &alen, &msg, &nwant) != 4)
		/* Old-style scope rule */
		bset(&scope);

	msg_insertfield(msg, SYSFLD_SCOPE, (char *) &scope, FTYPE_BITVEC, sizeof(scope));
	if (nwant) {
		/* Collect replies as proxy for the sender */
		register message **rmsgs;
		register nrep;
		extern cl_errno;

		rmsgs = (message **) malloc(nwant * sizeof(message *));
		if ((nrep = (*broutine) (alist, msg, nwant, collect_msgs, rmsgs, 0)) < 0) {
			cl_errno = nrep;
			nrep = 0;
		}
		msg_delete(msg);
		reply_client(mp, rmsgs, nrep);
		while (nrep--)
			msg_delete(rmsgs[nrep]);
		free((char *) rmsgs);
		cl_errno = 0;
	} else {
		extern CBCAST();

		(void) (*broutine) (alist, msg, 0, 0, 0, 0);
		msg_delete(msg);
		if (broutine == CBCAST) {
			register address *ap;

			for (ap = alist; !aptr_isnull(ap); ap++)
				if (addr_isgid(ap)
				    || (addr_ispid(ap) && ap->addr_site != my_site_no))
					return (1);
		}
	}
	return (0);
}

alength(ap)
	register address *ap;
{
	register n = 1;

	while (!aptr_isnull(ap)) {
		++ap;
		++n;
	}
	return (n);
}

/* Generate the actual routines */
#define bcast(clname, brname)                       \
clname(mp)                                          \
  message *mp;                                      \
  {                                                 \
        int brname();                               \
        (void)do_clbcast(mp, brname);               \
  }

bcast(cl_cbcast, CBCAST)
    bcast(cl_fbcast, FBCAST)
    bcast(cl_abcast, ABCAST)
    bcast(cl_gbcast, GBCAST)
    bcast(cl_gbcast_grow, GBCAST_GROW)
