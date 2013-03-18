/*  $RCSfile: pr_init.c,v $ $Revision: 2.21 $ $Date: 90/08/15 09:50:31 $  */
/*
 *	Originally coded by Ken Birman
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

#if   (SUN || AUX)
#    include <sys/resource.h>
#endif

/* Initialize most things, except for net (net_init called during run) */
pr_initialize()
{
	extern qnode *cb_ids;

	qu_freelist = qu_null();
	IOQS = qu_null();
	time_queue = qu_null();
	as_root = qu_null();
	pg_root = qu_null();
	t_init();
	ab_init();
	cb_init();
	gb_init();
	fd_init();
	glocks = qu_null();
	watching = qu_null();
	cb_ids = qu_null();
}

FILE *statfile;

save_stats()
{
	static firsttime;

	pr_dostats();
	if (firsttime == 0 && (isis_stats.is_time & 1)) {
		++firsttime;
		timeout(1000, save_stats, 0, 0);
	} else
		timeout(2000, save_stats, 0, 0);
	fwrite(&isis_stats, sizeof(isis_stats), 1, statfile);
	fwrite(comm_stats.c_stats[1], sizeof(comm_stats.c_stats[1]), NSITES, statfile);
}

pr_stats(mp)
	message *mp;
{
	pr_dostats();
	reply(mp, (char *) &isis_stats, FTYPE_LONG, sizeof(isis_stats));
}

pr_dostats()
{
	static struct timeval tbuf;
	int memuse = memalloc - memfree, msgmem = msg_memused - msg_memfree;
	extern cl_congested;

	isis_stats.is_nlocdelete = as_nlocdelete;
	isis_stats.is_ndelete = as_ndelete;
	if (cl_congested) {
		isis_stats.is_congest = IS_CONGEST;
		if (memuse > MEM_LO)
			isis_stats.is_congest |= IS_MEM;
		if (msgmem > MSGMEM_LO)
			isis_stats.is_congest |= IS_MSG;
		if (ntasks > TASK_LO)
			isis_stats.is_congest |= IS_TASK;
		if (intersite_congested > INTERSITE_LO)
			isis_stats.is_congest |= IS_INTER;
	} else
		isis_stats.is_congest = 0;
	isis_stats.is_memuse = memuse;
	isis_stats.is_msgmem = msgmem;
	isis_stats.is_ntasks = ntasks;
	isis_stats.is_inter = intersite_congested;
	gettimeofday(&tbuf, (struct timezone *) 0);
	isis_stats.is_time = tbuf.tv_sec;
}

int isis_errno, isis_state;
int rcv_reply(), cl_register(), cl_create(), cl_fdrestart(), pglist_rcv();
int cl_cbcast(), cl_cbcast_ex(), cl_abcast(), cl_abcast_ex();
int cl_gbcast(), cl_gbcast_ex(), cl_gbcast_grow(), cl_fbcast(), cl_fbcast_ex();
int cl_getview(), cl_pglookup(), cl_wantdump(), pr_wantdump(), cl_wantflush();
int cl_pglist(), pr_clfailed(), pgwantview_rcv(), pgcreate_chk();
int pglookup_rcv(), st_gparticipant(), gb_recvabort();
int ab_recv1(), ab_recv2(), gb_recv1(), gb_recv2();
int gb_recvconfirm(), cb_recvpkt(), cb_recvflush();
int fragman(), client_failed();
int direct_bcast(), direct_abcast(), direct_cbcast(), direct_fbcast();
int direct_gbcast(), st_flush(), t_fork(), client_crashed();
int t_fork_urgent(), pg_failed(), cl_echo(), cl_pmonitor(), cl_probe(), cl_died();
int cl_hello(), fd_newview(), pr_cbcast(), pr_abcast(), pr_gbcast(), pr_bcast();
int fd_recvnewview(), fd_recvack(), fd_recvnegack(), fd_recvcommit();
int fd_recvrestart(), fd_recvincarn(), fd_recvdeadlist(), pr_is_alive();
int pg_update_pgviews(), ab_takeover(), gb_takeover();
int pr_rescansfile(), pr_doscansfile(), pg_add_groupview();

#ifdef	SUNLWP
_doexit()
{
	_exit(0);
}
#else
_doexit()
{
	exit(0);
}
#endif

struct entry_info {
	int e_name;
	int (*e_proc) ();
	char *e_pname;
};

struct entry_info entry_defs[]
    = {
	CL_REGISTER, cl_register, "cl_register",
	PR_PGLIST, pglist_rcv, "pglist_rcv",
	PR_STATS, pr_stats, "pr_stats",
	CL_PGLIST, cl_pglist, "cl_pglist",
	CL_CREATE, cl_create, "cl_create",
	CL_CBCAST, cl_cbcast, "cl_cbcast",
	CL_FBCAST, cl_fbcast, "cl_fbcast",
	CL_ABCAST, cl_abcast, "cl_abcast",
	CL_GBCAST, cl_gbcast, "cl_gbcast",
	CL_GBCAST_GROW, cl_gbcast_grow, "cl_gbcast_grow",
	CL_GETVIEW, cl_getview, "cl_getview",
	CL_LOOKUP, cl_pglookup, "cl_pglookup",
	CL_FDRESTART, cl_fdrestart, "cl_fdrestart",
	CL_WANTDUMP, cl_wantdump, "cl_wantdump",
	PR_WANTDUMP, pr_wantdump, "pr_wantdump",
	CL_WANTFLUSH, cl_wantflush, "cl_wantflush",
	CL_PMONITOR, cl_pmonitor, "cl_pmonitor",
	CL_PROBE, cl_probe, "cl_probe",
	CL_DIED, cl_died, "cl_died",
	CL_HELLO, cl_hello, "cl_hello",
	CL_ECHO, cl_echo, "cl_echo",
	PR_CLFAILED, pr_clfailed, "pr_clfailed",
	CL_SHUTDOWN, _doexit, "exit",
	PR_RESCANSITES, pr_rescansfile, "pr_rescansfile",
	PR_DOSCANSITES, pr_doscansfile, "pr_doscansfile",
	PR_ADD_GROUPVIEW, pg_add_groupview, "pg_addgroupview",

	/* Normal for isis->isis messages */
	GENERIC_RCV_REPLY, rcv_reply, "rcv_reply",
	PR_PGWANTVIEW, pgwantview_rcv, "pgwantview_rcv",
	PR_CREATE_CHK, pgcreate_chk, "pgcreate_chk",
	PR_STGPARTICIPANT, st_gparticipant, "st_gparticipant",
	PR_ABRECV1, ab_recv1, "ab_recv1",
	PR_ABRECV2, ab_recv2, "ab_recv2",
	PR_GBRECV1, gb_recv1, "gb_recv1",
	PR_GBRECV2, gb_recv2, "gb_recv2",
	PR_GBRECVABORT, gb_recvabort, "gb_recvabort",
	PR_GBRECVCONFIRM, gb_recvconfirm, "gb_recvconfirm",
	PR_CBRECVPKT, cb_recvpkt, "cb_recvpkt",
	PR_PGLOOKUP, pglookup_rcv, "pglookup_rcv",
	PR_FRAGMAN, fragman, "fragman",
	FD_RECVNEWVIEW, fd_recvnewview, "fd_recvnewview",
	FD_RECVACK, fd_recvack, "fd_recvack",
	FD_RECVNEGACK, fd_recvnegack, "fd_recvnegack",
	FD_RECVCOMMIT, fd_recvcommit, "fd_recvcommit",
	FD_RECVRESTART, fd_recvrestart, "fd_recvrestart",
	FD_RECVINCARN, fd_recvincarn, "fd_recvincarn",
	FD_RECVDEADLIST, fd_recvdeadlist, "fd_recvdeadlist",
	PR_IS_ALIVE, pr_is_alive, "pr_is_alive",
	-1,
};

long msg_memused, msg_memfree, msg_namsgs, msg_nfmsgs;
int msg_tracecaller, msg_tracemsgs, msg_usagestats;

struct rname_info {
	int (*r_proc) ();
	char *r_pname;
};

struct rname_info rname_defs[]
    = {
	direct_bcast, "direct_bcast",
	direct_abcast, "direct_abcast",
	direct_cbcast, "direct_cbcast",
	direct_fbcast, "direct_fbcast",
	direct_gbcast, "direct_gbcast",
	pr_cbcast, "pr_cbcast",
	pr_abcast, "pr_abcast",
	pr_gbcast, "pr_gbcast",
	pr_bcast, "pr_bcast",
	st_flush, "st_flush",
	t_fork, "t_fork",
	t_fork_urgent, "t_fork_urgent",
	pg_failed, "pg_failed",
	client_failed, "client_failed",
	fd_newview, "fd_newview",
	pg_update_pgviews, "pg_update_pgviews",
	ab_takeover, "ab_takeover",
	gb_takeover, "gb_takeover",
	client_crashed, "client_crashed",
	0, 0
};

isis_entries()
{
	register struct entry_info *ep;
	register struct rname_info *rp;

	ep = entry_defs;
	while (ep->e_name != -1) {
		isis_entry(ep->e_name, ep->e_proc, ep->e_pname);
		++ep;
	}
	rp = rname_defs;
	while (rp->r_proc != nullroutine) {
		isis_rname(rp->r_proc, rp->r_pname);
		++rp;
	}
}
