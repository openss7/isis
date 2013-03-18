/*  $RCSfile: selftest.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:41 $  */
/*
 * This program exercises state transfers and cbcast to groups
 * with rapidly changing memberships.  Start 5 copies up at once,
 * then let them run.  Reports errors, if any, to the console.
 * 
 * One unavoidable problem is that if joins fail, a process probably
 * should commit suicide. This one does't do so, and carefully avoids printing
 * complaints about the event as well (flag join_failed will be set).
 *
 * There are also some cases where this test program hangs because of perverse
 * sequences of joins and leaves, but where nothing is really wrong.
 * Probably best not to use selftest unless you are a real wizard.
 *
 * Ken Birman
 */

#include "isis.h"

int atoi();

address *gid;

#define	NPROC	5

#define ON 	1
#define OFF 	0

int counts[NPROC], myrank = -1, nmemb, running, join_failed;;

void
main(argc, argv)
	char argc;
	char **argv;
{
	void changed(), send_state(), rcv_state(), inc();
	register port = 0, n;

	for (n = 0; n < NPROC; n++)
		counts[n] = n;
	if (argc > 1)
		port = atoi(*++argv);
	print("selftest: please start %d copies\n", NPROC);
	isis_init(port);
	gid = pg_join("group", PG_MONITOR, changed, 0, PG_XFER, 0, send_state, rcv_state, 0);
	isis_logging(ON);
	isis_entry(1, inc, "inc");
	isis_start_done();
	isis_mainloop(NULLROUTINE, NULLARG);
}

static got_state;

void
init_state()
{
	got_state = 1;
}

void
send_state()
{
	if (isis_state & ISIS_LEAVING) {
		print("state xfer while ISIS_LEAVING!\n");
		isis_logging(OFF);
		panic("state xfer while ISIS_LEAVING!");
	}
	if (running)
		counts[myrank]--;
	xfer_out(0, "%D", counts, NPROC);
	print("\nmyrank %d mypid %d: sending %d %d %d %d %d\n",
	      myrank, my_process_id, counts[0], counts[1], counts[2], counts[3], counts[4]);
	if (running)
		counts[myrank]++;
}

void
rcv_state(loc, mp)
	int loc;
	message *mp;
{
	got_state = 1;
	msg_get(mp, "%D", counts, (int *) 0);
	print("\nmyrank %d from %d: got %d %d %d %d %d\n",
	      myrank, msg_getsender(mp)->addr_process, counts[0], counts[1], counts[2], counts[3],
	      counts[4]);
}

void
changed(gv)
	register groupview *gv;
{
	register address *ap;
	static firsttime;

	if ((nmemb = gv->gv_nmemb) == 0 || addr_ismine(&gv->gv_departed)) {
		print("\n%d: left group\n", myrank);
		return;
	}
	if (myrank == -1) {
		for (ap = gv->gv_members; !aptr_isnull(ap); ap++)
			if (addr_ismine(ap))
				break;
		myrank = ap - gv->gv_members;
	}
	print("\n** view %d.%d: ", VMM(gv->gv_viewid));
	paddrs(gv->gv_members);
	print("\n");
	print("** myrank %d, nmemb %d, %d %d %d %d %d\n",
	      myrank, nmemb, counts[0], counts[1], counts[2], counts[3], counts[4]);
	if (gv->gv_nmemb == NPROC)
		++running;
	if (myrank == 0 && gv->gv_nmemb == NPROC && firsttime++ == 0)
		cbcast(gid, 1, "%d,%d", -1, -1, 0);
}

void
inc(mp)
	message *mp;
{
	int rank, value;
	static nactive, counter[NPROC], inarow;

	if (join_failed)
		return;
	msg_get(mp, "%d,%d", &rank, &value);
	print("%d/%d<%d> ", rank, value, msg_getid(mp));
	fprintf(stderr, "G");
	fflush(stderr);
	if (++inarow % 10 == 9)
		print("\n");
	if (value != -1) {
		++counts[rank];
		if (rank != myrank) {
			if (counts[rank] != value) {
				print("\n--> received %d expected %d\n", value, counts[rank]);
				fprintf(stderr, "\n--> received %d expected %d\n", value,
					counts[rank]);
				counts[rank] = value;
				/* isis_logging(OFF); panic("%d: receive %d/%d<%d> expected %d\n",
				   myrank, rank, value, msg_getid(mp), counts[rank]); */
			}
			return;
		} else if (--nactive < 0)
			panic("nactive %d\n", nactive);
	} else
		++counts[myrank];
	if (alist_len(msg_getdests(mp)) != nmemb) {
		print("\ngot message sid=%d with %d dests but group has %d membs\n",
		      msg_getid(mp), alist_len(msg_getdests(mp)), nmemb);
		isis_logging(OFF);
		panic("got message sid=%d with %d dests but group has %d membs\n",
		      msg_getid(mp), alist_len(msg_getdests(mp)), nmemb);
	}
	if (++counter[myrank] == 25 + myrank * 10) {
		print("\n%d: leave group (count %d)\n", myrank, counts[myrank]);
		pg_leave(gid);
	      again:
		print("%d: sleep\n", myrank);
		sleep(3);
		print("%d: rejoin group\n", myrank);
		got_state = 0;
		gid = pg_join("group", PG_MONITOR, changed, 0, PG_XFER, 0,
			      send_state, rcv_state, PG_INIT, init_state, 0);
		if (!addr_isnull(gid)) {
			if (got_state == 0)
				panic("%d: DIDN'T GET STATE!\n");
			print("%d: rejoined group\n", myrank);
		} else {
			isis_perror("rejoin failed");
			join_failed = 1;
			goto again;
		}
		join_failed = 0;
		counter[myrank] = 1;
		counts[myrank]++;
	}
	if (++nactive > 1)
		panic("%d active cbcasts!\n", nactive);
	print("(S %d/%d) ", myrank, counts[myrank]);
	fprintf(stderr, "S");
	fflush(stderr);
	if (++inarow % 10 == 9)
		print("\n");
	cbcast(gid, 1, "%d,%d", myrank, counts[myrank], 0);
}
