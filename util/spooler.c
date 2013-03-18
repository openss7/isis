/*  $RCSfile: spooler.c,v $ $Revision: 2.32 $ $Date: 90/08/30 15:23:13 $  */
/*
 *	New ISIS Spooler
 *	Coded by Ken Birman
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

#include   "isis.h"
#include   "spooler.h"
#include   <string.h>
#include   <ctype.h>
#include   <sys/file.h>
#ifdef HPUX
#  include   <time.h>
#else
#  include   <sys/time.h>
#endif

int main_task(), spooler_send(), spooler_rcv(), handle_spool(), discard(), spool_and_discard_msg(),
replay();
int prepend_and_discard(), lookup(), cancel(), inquire(), h_spool_wait(), advise(), newview();
int h_spool_play_through(), h_spool_set_replay_pointer(), spool_set_chkpt_pointer();

int updateComState(), updateLoad(), updateOldestInMess(), updateLowerBound(), rcvSpoolReq();
int updateGView(), registerCon(), receiveStartup(), treatPartnerFailure();
int long_haul_init(), establishier(), acceptProc(), lh_receive(), sendStartup();
void set_join(), lh_cbcast(), lh_spool(), lh_rcv_join(), lh_rcv_cbcast(), lh_setsrcerr(),
lh_setdsterr();
void lh_remote_file_xfer(), lh_rcv_remote_file(), lh_rcv_failure(), lh_snd_failure();
void lh_abcast(), lh_rcv_abcast(), giveConvID(), comstate_update();

typedef struct sp_info sp_info;
address *cached_pg_lookup();

struct sp_info {
	char sp_name[PG_GLEN];		/* Name of this spool */
	char sp_gname[PG_GLEN];		/* Name of this group */
	char sp_fname[10];		/* File name containing this spool */
	address sp_hascopy[PG_ALEN];	/* Has a valid copy */
	int sp_flag;			/* Flags for this entry */
	int sp_time;			/* Time when referenced */
	int sp_recno;			/* Length in records */
	int sp_rpointer;		/* Current replay pointer */
	int sp_cpointer;		/* Current chkpt pointer */
	sp_info *sp_next;		/* Link to next entry */
	address sp_handler;		/* Spooler group handling this spool */
	address sp_gaddr;		/* Address of this group, if known */
	condition sp_wantlock;		/* Where to wait for a lock */
	int (*sp_routine) ();		/* Routine to call during replay */
	FILE *sp_fhandle;		/* File handle if currently open */
};

#define SP_NOPLAY       0x0001	/* Disables play through */
#define SP_HAVECOPY     0x0002	/* Set if I have a copy */
#define SP_NOCOPY       0x0004	/* Set if I don't (neither set if I haven't checked) */
#define SP_GROUPUP      0x0008	/* Group is operational now */
#define SP_INREPLAY     0x0010	/* Inhibits playthrough in replay mode */
#define SP_LOCKED       0x0020	/* Interlock for things that change this spool */

sp_info *sp_head, *new_sp();
void write_client();
address *spgid;
message *sp_getmsg();
extern long_haul_init();
void spooler_group_failed();
int db_spool;
FILE *lh_file;

/* die after writing thus # of messages over LH link. */
/* FOR DEBUGGING ONLY */

int diemsg = -1;

main(argc, argv)
	register char **argv;
{
	int port = 0;
	char *lh_name, *s;

	while (argc-- > 1) {
		register char *argument = *++argv;
		register c = *argument;

		if (isdigit(c))
			port = atoi(*argv);
		else if (c == '-') {
			switch (*++argument) {
			default:
				goto usage;

			case 'v':
				++db_spool;
				continue;

			case 'd':
				if (*(s = ++argument) == 0) {
					--argc;
					s = *++argv;
				}
				diemsg = atoi(s);
				continue;
			case 'l':
				if (*(lh_name = ++argument) == 0) {
					--argc;
					lh_name = *++argv;
				}
				if ((lh_file = fopen(lh_name, "r")) == (FILE *) 0)
					perror("long-haul initialization file");
			}
		} else {
		      usage:
			panic("Usage: spooler [port-no] [-l<lh-init-file>]");
		}
	}
	isis_init(port);
	isis_entry(SP_SPOOL, (vfunc *) lh_spool, "lh_spool");
	isis_entry(SP_LH_SRCERR, (vfunc *) lh_setsrcerr, "lh_setsrcerr");
	isis_entry(SP_LH_DSTERR, (vfunc *) lh_setdsterr, "lh_setdsterr");
	isis_entry(SP_DISCARD, (vfunc *) discard, "discard");
	isis_entry(SP_SPOOL_AND_DISCARD, (vfunc *) spool_and_discard_msg, "spool_and_discard");
	isis_entry(SP_PREPEND_AND_DISCARD, (vfunc *) prepend_and_discard, "prepend_and_discard");
	isis_entry(SP_REPLAY, (vfunc *) replay, "replay");
	isis_entry(SP_CANCEL, (vfunc *) cancel, "cancel");
	isis_entry(SP_INQUIRE, (vfunc *) inquire, "inquire");
	isis_entry(SP_ADVISE, (vfunc *) advise, "advise");
	isis_entry(SP_LOOKUP, (vfunc *) lookup, "lookup");
	isis_entry(SP_WAIT, (vfunc *) h_spool_wait, "h_spool_wait");
	isis_entry(SP_SET_RPOINTER, (vfunc *) h_spool_set_replay_pointer,
		   "h_spool_set_replay_pointer");
	isis_entry(SP_SET_CPOINTER, (vfunc *) spool_set_chkpt_pointer, "spool_set_chkpt_pointer");
	isis_entry(SP_PLAY_THROUGH, (vfunc *) h_spool_play_through, "h_spool_play_through");
	isis_task((vfunc *) spooler_send, "state_send");
	isis_task((vfunc *) spooler_rcv, "state_rcv");
	isis_task((vfunc *) main_task, "main_task");
	isis_task((vfunc *) newview, "newview");
	isis_task((vfunc *) spooler_group_failed, "spooler_group_failed");

	isis_entry(REG_ENTRY, (vfunc *) registerCon, "REGISTER_BACKUP_CON");
	isis_entry(WEIGHT_ENTRY, (vfunc *) updateLoad, "UPDATE_WEIGHT_TAB");
	isis_entry(STARTUP_ENTRY, (vfunc *) receiveStartup, "RECEIVE_STARTUP");
	isis_entry(UPDATE_LOWER_BOUND, (vfunc *) updateLowerBound, "UPDATE_LOWER_BOUND");
	isis_entry(UPDATE_OLDEST_INMESS, (vfunc *) updateOldestInMess, "UPDATE_OLDEST_INMESS");
	isis_entry(PARTNER_FAILED, (vfunc *) treatPartnerFailure, "PARTNER_FAILURE");
	isis_entry(COMSTATE_ADD_ENTRY, (vfunc *) updateComState, "UPDATE_COMSTATE");
	isis_entry(RESPOOL_REQUEST, (vfunc *) rcvSpoolReq, "RCV_RESPOOL_REQ");
	isis_entry(LH_JOIN, (vfunc *) set_join, "LH_JOIN_WAN");
	isis_entry(LH_CBCAST, (vfunc *) lh_cbcast, "LH_CBCAST");
	isis_entry(LH_XFER, (vfunc *) lh_remote_file_xfer, "LH_XFER");
	isis_entry(LH_JOIN_REQUEST, (vfunc *) lh_rcv_join, "LH_RCV_JOIN");
	isis_entry(LH_CBCAST_REQUEST, (vfunc *) lh_rcv_cbcast, "LH_RCV_CBCAST");
	isis_entry(LH_XFER_REQUEST, (vfunc *) lh_rcv_remote_file, "LH_XFER_REQUEST");
	isis_entry(LH_XFER_RCV_FAILURE, (vfunc *) lh_rcv_failure, "LH_XFER_RCV_FAILURE");
	isis_entry(LH_XFER_SND_FAILURE, (vfunc *) lh_snd_failure, "LH_XFER_SND_FAILURE");
	isis_entry(LH_COMSTATE_UPDATE, (vfunc *) comstate_update, "COMSTATE_UPDATE");
	isis_entry(LH_ABCAST, (vfunc *) lh_abcast, "LH_ABCAST");
	isis_entry(LH_ABCAST_REQUEST, (vfunc *) lh_rcv_abcast, "LH_RCV_ABCAST");
	isis_entry(LH_CONVID_REQUEST, (vfunc *) giveConvID, "GIVE_CONVERSATION_ID");
	isis_task((vfunc *) establishier, "conEstablishierTask");
	isis_task((vfunc *) sendStartup, "startupTask");
	isis_task((vfunc *) acceptProc, "acceptConTask");
	isis_task((vfunc *) lh_receive, "receiveTask");
	isis_task((vfunc *) updateGView, "updatingGView");
	isis_task((vfunc *) long_haul_init, "longHaulIntitTask");

	isis_mainloop((vfunc *) main_task, NULLARG);
}

main_task()
{
	spgid = pg_join("@*:spooler",
			PG_XFER, 0, spooler_send, spooler_rcv, PG_MONITOR, newview, 0, 0);
	if (chdir(isis_dir) == -1)
		print("WARNING: isis spooler can't chdir to <%s>\n", isis_dir);
	isis_start_done();
	if (lh_file)
		t_fork((vfunc *) long_haul_init, lh_file);
}

int SPSEQN;

newview(gv)
	register groupview *gv;
{
}

spooler_send(locator)
{
	register sp_info *sp = sp_head;

	while (sp && locator >= 0)
		sp = sp->sp_next;
	if (locator == -1)
		xfer_out(++locator, "%d", SPSEQN);
	while (sp) {
		xfer_out(++locator, "%s,%s,%A[1],%d,%d,%A", sp->sp_name, sp->sp_fname,
			 &sp->sp_handler, &sp->sp_time, sp->sp_recno, sp->sp_hascopy,
			 alist_len(sp->sp_hascopy));
		sp = sp->sp_next;
	}
}

spooler_rcv(locator, msg)
	register message *msg;
{
	register sp_info *sp;
	static last_locator = -1;

	if (locator != ++last_locator)
		panic("spooler: recv state expected %d got record %d\n", last_locator, locator);
	if (locator == 0)
		msg_get(msg, "%d", &SPSEQN);
	else {
		sp = (sp_info *) malloc(sizeof(sp_info));
		bzero(sp, sizeof(sp_info));
		msg_get(msg, "%s,%s,%A[1],%d,%d,%A", sp->sp_name, sp->sp_fname, &sp->sp_handler,
			&sp->sp_time, &sp->sp_recno, sp->sp_hascopy, (int *) 0);
	}
}

have_copy(sp)
	register sp_info *sp;
{
	register address *ap;

	if (sp == (sp_info *) 0)
		return (0);
	if (sp->sp_flag & (SP_HAVECOPY | SP_NOCOPY))
		return ((sp->sp_flag & SP_HAVECOPY) != 0);
	for (ap = sp->sp_hascopy; !aptr_isnull(ap); ap++)
		if (addr_isequal(ap, &my_address)) {
			sp->sp_flag |= SP_HAVECOPY;
			return (1);
		}
	sp->sp_flag |= SP_NOCOPY;
	return (0);
}

static sp_info *
sp_lookup(sname, creatflag)
	char *sname;
{
	register sp_info *sp;
	register groupview *gv;

	for (sp = sp_head; sp; sp = sp->sp_next)
		if (strcmp(sp->sp_name, sname) == 0)
			return (sp);
	if (!creatflag)
		return (0);
	sp = (sp_info *) malloc(sizeof(sp_info));
	bzero(sp, sizeof(sp_info));
	strcpy(sp->sp_name, sname);
	sprintf(sp->sp_fname, "SP%d", ++SPSEQN);
	unlink(sp->sp_fname);
	if (db_spool)
		print("CREATE spool %s for group %s\n", sp->sp_fname, sname);
	sp->sp_handler = *spgid;
	gv = pg_getview(spgid);
	bcopy(gv->gv_members, sp->sp_hascopy, sizeof(address) * (gv->gv_nmemb + 1));
	sp->sp_next = sp_head;
	sp_head = sp;
	return (sp);
}

void
spooler_group_failed(gaddrp)
	address *gaddrp;
{
	register sp_info *sp;

	for (sp = sp_head; sp; sp = sp->sp_next)
		if (addr_isequal(&sp->sp_gaddr, gaddrp)) {
			sp->sp_gaddr = NULLADDRESS;
			sp->sp_flag &= ~SP_GROUPUP;
		}
}

sp_lock(sp)
	register sp_info *sp;
{
	if (sp->sp_flag & SP_LOCKED)
		t_wait_l(&sp->sp_wantlock, "spool-lock");
	sp->sp_flag |= SP_LOCKED;
}

sp_unlock(sp)
	register sp_info *sp;
{
	if (sp->sp_flag & SP_LOCKED) {
		sp->sp_flag &= ~SP_LOCKED;
		t_sig(&sp->sp_wantlock, 0);
	}
}

void
dolookup(mp, gaddr, how, arg)
	register message *mp;
	address *gaddr;			/* Not used. */
	int how;			/* Not used. */
	char *arg;			/* Not used. */
{
	register sp_info *sp;
	register address *addr;
	char *sname;

	if (msg_get(mp, "%-s", &sname) == 0)
		addr = &NULLADDRESS;
	else {
		sp = sp_lookup(sname, 1);
		addr = &sp->sp_handler;
	}
	reply(mp, "%A[1]", addr);
}

lookup(mp)
	register message *mp;
{
	coord_cohort(mp, spgid, dolookup, NULLROUTINE, NULLARG);
}

static dospool();

spool_msg(mp)
	register message *mp;
{
	dospool(mp, 1);
}

respool(mp)
	register message *mp;
{
	dospool(mp, 0);
}

static
dospool(mp, check_net)
	register message *mp;
{
	register sp_info *sp;
	register flag;
	int entry, sid;
	char *sname;

	if ((sid = msg_getfld(mp, SYSFLD_SPSCAN, 0, "%-s,%d", &sname, &entry)) != 2)
		reply(mp, "%d", -1);
	else {
		if (check_net)
			msg_getfld(mp, SYSFLD_NETWORK, 0, "%-s", &sname);
		sp = sp_lookup(sname, 1);
		if (db_spool) {
			print("%s/%s:%x. spool ", sname, sp->sp_fname, sp->sp_fhandle);
			pmsg(mp);
		}
		sp_lock(sp);
		sid = have_copy(sp);
		sp_unlock(sp);
		if (sid) {
			sid = sp->sp_recno++;
			msg_replacefield(mp, SYSFLD_SPSEQN, (char *) &sid, FTYPE_LONG, sizeof(int));
			sp_lock(sp);
			sp_write(0, 0, mp, sp);
			sp_flush(sp);
			flag = sp->sp_flag & (SP_GROUPUP | SP_NOPLAY | SP_INREPLAY);
			sp_unlock(sp);
			if (flag == SP_GROUPUP) {
				if (db_spool)
					print("%s/%s:%x. spool write_client!\n", sname,
					      sp->sp_fname, sp->sp_fhandle);
				write_client(1, sp, mp, 0, sname, entry, sid);
			}
			reply(mp, "%d", sid);
		} else
			nullreply(mp);
	}
}

discard(mp)
	register message *mp;
{
	register sp_info *sp, *newsp;
	char *sname;
	int entry, ptr = 0;

	if (msg_getfld(mp, SYSFLD_SPSCAN, &ptr, "%-s,%d", &sname, &entry) != 2)
		reply(mp, "%d", -1);
	else {
		sp = sp_lookup(sname, 0);
		if (have_copy(sp)) {
			if (db_spool)
				print("%s/%s:%x. discard invoked\n", sname, sp->sp_fname,
				      sp->sp_fhandle);
			sp_lock(sp);
			newsp = new_sp(sp);
			sp_copy(sp, newsp, mp, 0, &ptr);
			sp_flush(newsp);
			sp_swap(sp, newsp);
			sp_unlock(sp);
			reply(mp, "%d", 1);
		} else
			nullreply(mp);
	}
}

spool_and_discard_msg(mp)
	register message *mp;
{
	h_do_spool_and_discard(mp, 0);
}

prepend_and_discard(mp)
	register message *mp;
{
	h_do_spool_and_discard(mp, 1);
}

h_do_spool_and_discard(mp, prepend)
	register message *mp;
{
	register sp_info *sp, *newsp;
	char *sname;
	int entry, ptr = 0;

	if (msg_getfld(mp, SYSFLD_SPSCAN, &ptr, "%-s,%d", &sname, &entry) != 2)
		reply(mp, "%d", -1);
	else {
		int sid;

		sp = sp_lookup(sname, 1);
		sp_lock(sp);
		if (have_copy(sp)) {
			newsp = new_sp(sp);
			if (db_spool)
				print("%s/%s:%x. spool_and_discard invoked\n", sname, sp->sp_fname,
				      sp->sp_fhandle);
			msg_replacefield(mp, SYSFLD_SPSEQN, (char *) &sid, FTYPE_LONG, sizeof(int));
			if (prepend) {
				sp->sp_cpointer = sid = 0;
				sp_write(0, 0, mp, newsp);
			}
			sp_copy(sp, newsp, mp, 1, &ptr);
			if (!prepend) {
				sp->sp_cpointer = sid = sp->sp_recno++;
				sp_write(0, 0, mp, newsp);
			}
			sp_flush(newsp);
			sp_swap(sp, newsp);
			if (db_spool)
				print("spool_and_discard: set chkpt pointer to seqn %d\n", sid);
			reply(mp, "%d", sid);
		} else
			nullreply(mp);
		sp_unlock(sp);
	}
}

doreplay(mp, gaddr, how, sp)
	register message *mp;
	address *gaddr;
	register sp_info *sp;
{
	register sid;

	sp_lock(sp);
	if (have_copy(sp)) {
		int ptr = 0, entry;
		char *sname;

		(void) msg_getfld(mp, SYSFLD_SPSCAN, &ptr, "%-s,%d", &sname, &entry);
		if (db_spool)
			print("%s/%s:%x. replay invoked\n", sname, sp->sp_fname, sp->sp_fhandle);
		sid = sp_replay(sp, mp, &ptr);
		reply(mp, "%d", sid);
	} else
		abortreply_l(mp, IE_TOTFAIL);
	sp_unlock(sp);
}

/*
 *   localreplay("norway", my_proc)
 *   localreplay("norway", NULLROUTINE)
 *   ...
 *   Calls:
 *       my_proc("norway", mp)
 */

localreplay(sname, routine)
	char *sname;
	int (*routine) ();
{
	int ptr = 0;
	register sp_info *sp = sp_lookup(sname, 1);
	register message *mp = msg_newmsg();

	sp_lock(sp);
	if (sp && (sp->sp_routine = routine)) {
		sp->sp_flag |= SP_GROUPUP;
		sp_replay(sp, mp, &ptr);
	} else if (sp)
		sp->sp_flag &= ~SP_GROUPUP;
	msg_delete(mp);
	sp_unlock(sp);
}

/* Figure out who the coordinator should be */
address *
pickcoordinator(gaddr, senderp, players, sp)
	register address *players;
	address *senderp, *gaddr;
	register sp_info *sp;
{
	register groupview *gv = pg_getlocalview(gaddr);
	register address *ap, *bp;
	address sender, *wasup = 0;
	register alen;

	sender = *senderp;
	sender.addr_entry = 0;
	for (ap = players; !aptr_isnull(ap); ap++)
		if (addr_ismine(ap))
			break;
	if (aptr_isnull(ap))
		panic("replay pick-coordinator: self not in player list");
	for (ap = players; !aptr_isnull(ap); ap++)
		if (addr_cmp(ap, &sender) == 0)
			return (ap);
	alen = ap - players;
	for (ap = players; !aptr_isnull(ap); ap++)
		if (ap->addr_site == sender.addr_site)
			break;
	if (ap->addr_site == 0)
		/* In this case, share the load */
		ap = &players[sender.addr_site % alen];
	bp = ap;
	do {
		register address *vp;

		/* See if this guy has a copy... and is up */
		for (vp = gv->gv_members; !aptr_isnull(vp); vp++)
			if (addr_cmp(vp, ap) == 0)
				break;
		if (!aptr_isnull(vp)) {
			if (addr_isnull(wasup))
				wasup = ap;
			for (vp = sp->sp_hascopy; !aptr_isnull(vp); vp++)
				if (addr_cmp(vp, ap) == 0)
					return (vp);
		}
		/* Nope, move on to the next one */
		if ((++ap)->addr_site == 0)
			ap = players;
	}
	while (ap != bp);
	if (wasup)
		return (wasup);
	return (&NULLADDRESS);
}

replay(mp)
	register message *mp;
{
	register sp_info *sp, *newsp;
	char *sname;
	int entry, sid;
	static condition want_in = 0;

	if (spool_in_replay)
		t_wait(&want_in);
	++spool_in_replay;
	if (msg_getfld(mp, SYSFLD_SPSCAN, 0, "%-s,%d", &sname, &entry) != 2)
		reply(mp, "%d", -1);
	else {
		register address *gaddr = cached_pg_lookup(sname, 1);

		sp = sp_lookup(sname, 1);
		if (addr_isnull(gaddr))
			reply(mp, "%d", -1);
		else {
			sp->sp_flag |= SP_INREPLAY | SP_GROUPUP;
			sp->sp_gaddr = *gaddr;
			coord_cohort_l(mp, spgid, (vfunc *) doreplay, NULLROUTINE, sp,
				       (address * (*)())pickcoordinator);
			sp->sp_flag &= ~SP_INREPLAY;
		}
	}
	--spool_in_replay;
	t_sig(&want_in, 0);
}

h_spool_play_through(mp)
	message *mp;
{
	register sp_info *sp;
	char *sname;
	int sp_onoff;

	msg_get(mp, "%-s,%d", &sname, &sp_onoff);
	if (sp = sp_lookup(sname, 1)) {
		sp_lock(sp);
		if (sp_onoff == SP_ON)
			sp->sp_flag &= ~SP_NOPLAY;
		else
			sp->sp_flag |= SP_NOPLAY;
		sp_unlock(sp);
	}
	reply(mp, "%d", 0);
}

h_spool_set_replay_pointer(mp)
	message *mp;
{
	register sp_info *sp;
	char *sname;
	int spseqn;

	msg_get(mp, "%-s,%d", &sname, &spseqn);
	if (db_spool)
		print("h_spool_set_replay_pointer (%s): %d\n", sname, spseqn);
	if (sp = sp_lookup(sname, 1)) {
		sp_lock(sp);
		if (db_spool)
			print("old value was %d new value %d\n", sp->sp_rpointer, spseqn);
		if (spseqn == SP_INFINITY)
			sp->sp_rpointer = sp->sp_recno;
		else if (spseqn > sp->sp_rpointer)
			sp->sp_rpointer = spseqn;
		sp_unlock(sp);
	}
	reply(mp, "%d", 0);
}

spool_set_chkpt_pointer(mp)
	message *mp;
{
	register sp_info *sp;
	char *sname;
	int spseqn;

	msg_get(mp, "%-s,%d", &sname, &spseqn);
	if (db_spool)
		print("spool_set_chkpt_pointer (%s): %d\n", sname, spseqn);
	if (sp = sp_lookup(sname, 1)) {
		sp_lock(sp);
		if (spseqn == SP_INFINITY)
			sp->sp_cpointer = sp->sp_recno;
		else
			sp->sp_cpointer = spseqn;
		sp_unlock(sp);
	}
	reply(mp, "%d", 0);
}

cancel(mp)
	register message *mp;
{
}

inquire(mp)
	register message *mp;
{
}

h_spool_wait(mp)
	register message *mp;
{
}

advise(mp)
	register message *mp;
{
}

char **SP[SP_MAX];
char **CURPAT[SP_MAX];
int patcount;

#define MAXKEY		20	/* Practical limit */

sp_unpack(sp_msg, what, ptr)
	register message *sp_msg;
	register char **what;
	int *ptr;
{
	register n, key;
	int i;

	for (n = 0; n < SP_MAX; n++)
		if (what[n]) {
			free(what[n]);
			what[n] = 0;
		}
	if ((char ***) what == CURPAT)
		patcount = 0;
	if (db_spool)
		print("entry: sp_unpack!\n");
	while (msg_getfld(sp_msg, SYSFLD_SPSCAN, ptr, "%d", &i) == 1) {
		if (db_spool)
			print("Unpack: key %d... ", i);
		switch (key = i) {
		case 0:
			return;

		default:
			if (key < 0 || key > SP_MAX) {
				print("sp_unpack: BAD KEY %d\n", key);
				return;
			} else {
				int v0, v1;

				msg_getfld(sp_msg, SYSFLD_SPSCAN, ptr, "%d,%d", &v0, &v1);
				what[key] = (char *) malloc(sizeof(int) * 2);
				((int *) what[key])[0] = v0;
				((int *) what[key])[1] = v1;
				if (db_spool)
					print("numeric, [%d-%d]\n", v0, v1);
			}
			continue;

		case SP_RTIME:
		begin {
			register *range;

			what[key] = (char *) malloc(sizeof(int) * 2);
			range = (int *) what[key];
			msg_getfld(sp_msg, SYSFLD_SPSCAN, ptr, "%d,%d", &range[0], &range[0]);
			if (db_spool)
				print("rtime, [%d-%d]\n", range[0], range[1]);
		}
			continue;

		case SP_SENDER:
			if ((char ***) what == CURPAT)
				++patcount;
		case SP_SPOOLER:
		begin {
			if (what[key] == (char *) 0)
				what[key] = (char *) malloc(sizeof(address));
			msg_getfld(sp_msg, SYSFLD_SPSCAN, ptr, "%a", what[key]);
			if (db_spool)
				print("sender/spooler: ");
			paddr((address *) what[key]);
			print("\n");
		}
			continue;

		case SP_KEYWORDS:
		begin {
			char *s, **sp;

			what[key] = (char *) malloc(sizeof(char *) * MAXKEY);
			sp = (char **) what[key];
			while (msg_getfld(sp_msg, SYSFLD_SPSCAN, ptr, "%-s", &s) == 1
			       && strcmp(s, "--END--")) {
				char *dupl = malloc(strlen(s) + 1);

				bcopy(s, dupl, strlen(s) + 1);
				*sp++ = dupl;
			}
			*sp = 0;
			if (db_spool)
				print("keywords, <%s>\n", sp[-1]);
		}
			continue;
		}
	}
}

sp_info *
new_sp(sp)
	sp_info *sp;
{
	register sp_info *newsp;

	newsp = (sp_info *) malloc(sizeof(sp_info));
	bzero(newsp, sizeof(sp_info));
	sprintf(newsp->sp_fname, "SPTMP%d", ++SPSEQN);
	if (db_spool)
		print("CREATE spool %s for TEMPORARY USE by group %s\n", newsp->sp_fname,
		      sp->sp_gname);
	return (newsp);
}

/* has_keys is true if has both keys and pattern, false if just pattern */
sp_copy(sp, newsp, sp_msg, has_keys, ptr)
	register sp_info *sp, *newsp;
	message *sp_msg;
	int *ptr;
{
	int sp_write();

	sp_unpack(sp_msg, CURPAT, ptr);
	if (has_keys)
		sp_unpack(sp_msg, CURPAT, ptr);
	if (db_spool)
		print("sp_copy from %s:%x to %s:%x\n", sp->sp_fname, sp->sp_fhandle,
		      newsp->sp_fname, newsp->sp_fhandle);
	sp_scan(sp, sp_write, newsp);
}

sp_replay(sp, sp_msg, ptr)
	sp_info *sp;
	message *sp_msg;
	int *ptr;
{
	extern void write_client();

	sp_unpack(sp_msg, CURPAT, ptr);
	if (db_spool)
		print("sp_replay %s:%x\n", sp->sp_fname, sp->sp_fhandle);
	sp_scan(sp, write_client, 0);
}

sp_swap(sp, newsp)
	register sp_info *sp, *newsp;
{
	if (db_spool)
		print("sp_swap %s:%x with %s:%x\n", sp->sp_fname, sp->sp_fhandle, newsp->sp_fname,
		      newsp->sp_fhandle);
	sp_open(newsp);
	sp_close(newsp);
	sp_open(sp);
	sp_close(sp);
	rename(newsp->sp_fname, sp->sp_fname);
	free(newsp);
}

void
write_client(match_flag, sp, sp_msg, ignored, gname, entry, seqn)
	sp_info *sp;
	message *sp_msg;
	char *gname;
{
	register bc = 0;
	int (*broutine) (), fbcast(), cbcast(), gbcast(), abcast();

	if (!match_flag || !(sp->sp_flag & SP_GROUPUP))
		return;
	if (sp->sp_routine) {
		(*sp->sp_routine) (sp->sp_name, sp_msg);
		return;
	}
	if (SP[SP_BCAST])
		bc = *(int *) SP[SP_BCAST];
	switch (bc) {
	default:
		broutine = abcast;
		break;
	case SP_CBCAST:
		broutine = cbcast;
		break;
	case SP_ABCAST:
		broutine = abcast;
		break;
	case SP_GBCAST:
		broutine = gbcast;
		break;
	}
	if (db_spool) {
		print("SPOOL: write client %s ", gname);
		paddr(&sp->sp_gaddr);
		print(", entry %d, seqn is %d\n", entry, seqn);
	}
	(*broutine) (&sp->sp_gaddr, GENERIC_SP_REPLAY, "%m%d%d", sp_msg, entry, seqn, 0);
}

address *
cached_pg_lookup(gname, force)
	char *gname;
{
	char cgname[PG_GLEN];
	static address addr;

	if (force || strcmp(gname, cgname)) {
		strcpy(cgname, gname);
		addr = *pg_lookup(gname);
		if (!addr_isnull(&addr))
			pg_detect_failure(&addr, (vfunc *) spooler_group_failed, 0);
	}
	return (&addr);
}

sp_scan(sp, routine, arg0)
	register sp_info *sp;
	void (*routine) ();
{
	register message *sp_msg;

	if (have_copy(sp) == 0)
		return;
	sp_open(sp);
	if (db_spool)
		print("sp_scan(%x,%x)  -- fhandle(%s:%x): seek to 0\n", sp, arg0, sp->sp_fname,
		      sp->sp_fhandle);
	fseek(sp->sp_fhandle, (long) 0, L_SET);
	while (sp_msg = sp_getmsg(sp)) {
		char *gname;
		int entry, seqn, ptr = 0;
		register i, match_flag = 0;

		msg_getfld(sp_msg, SYSFLD_SPSCAN, &ptr, "%-s,%d", &gname, &entry);
		seqn = *(int *) msg_getfield(sp_msg, SYSFLD_SPSEQN, 1, (int *) 0);
		if (db_spool)
			print
			    ("sp_scan: inreplay %d, message has gname %s entry %d seqn %d pointers are <%d-%d>\n",
			     spool_in_replay, gname, entry, seqn, sp->sp_cpointer, sp->sp_rpointer);
		if (spool_in_replay && seqn < sp->sp_cpointer)
			continue;
		if (!spool_in_replay || seqn <= sp->sp_rpointer) {
			sp_unpack(sp_msg, SP, &ptr);
			for (i = 1; i < SP_MAX; i++) {
				if (CURPAT[i] == 0)
					continue;
				/* Pattern match... */
				if (db_spool)
					print("CURPAT[%d] defined... SP[%d] = %x\n", i, i, SP[i]);
				switch (i) {
					register value, *pattern;
					address *spd_who;
					register char **keys, **pats;

				default:
					if (i == SP_SPSEQN)
						value = seqn;
					else if (SP[i] == 0)
						goto mismatch;
					else
						value = *(int *) SP[i];
					pattern = (int *) CURPAT[i];
					if (db_spool)
						print("numeric: value %d range [%d-%d]\n", value,
						      pattern[0], pattern[1]);
					if (pattern[0] != SP_INFINITY && value < pattern[0])
						goto mismatch;
					if (pattern[1] != SP_INFINITY && value > pattern[1])
						goto mismatch;
					continue;

				case SP_TIME:
					if (db_spool)
						print("time: ignored\n");
					continue;

				case SP_SPOOLER:
					spd_who = (address *) CURPAT[i];
					if (db_spool)
						print("sp_spooler: addr_cmp<");
					paddr(spd_who);
					paddr(msg_getsender(sp_msg));
					print(">\n");
					if (addr_cmp(spd_who, msg_getsender(sp_msg)))
						goto mismatch;
					continue;

				case SP_SENDER:
					spd_who = (address *) CURPAT[i];
					if (db_spool)
						print("sp_sender: addr_cmp<");
					paddr(spd_who);
					paddr(msg_getsender(sp_msg));
					print(">\n");
					if (patcount == 2 && addr_cmp(spd_who, (address *) SP[i]))
						goto mismatch;
					continue;

				case SP_KEYWORDS:
					if ((keys = (char **) SP[i]) == 0)
						goto mismatch;
					pats = (char **) CURPAT[i];
					while (*keys && *pats) {
						if (db_spool)
							print("keywords: cmp <%s,%s>\n", *keys,
							      *pats);
						if (strcmp(*keys, *pats))
							goto mismatch;
						++keys, ++pats;
					}
					continue;
				}
			}
		} else {
			if (db_spool)
				print("outside rpointer bounds\n");
			if (spool_in_replay)
				isis_ctp->task_act = 0;
			if (sp->sp_flag & SP_NOPLAY)
				goto mismatch;
		}
		if (SP[SP_EXPIRES]) {
			int when;
			struct timeval now;

			if (db_spool)
				print("check expiration time\n");
			when = *(int *) SP[SP_EXPIRES];
			gettimeofday(&now, (struct timezone *) 0);
			if (when < now.tv_sec)
				goto mismatch;
		}
		++match_flag;

	      mismatch:
		if (db_spool) {
			print("... message %s! ", match_flag ? "matched" : "mismatch");
			pmsg(sp_msg);
		}
		(*routine) (match_flag, sp, sp_msg, arg0, gname, entry, seqn);
	}
}

#define MAXFOPEN	16

static nopen;

sp_open(sp)
	register sp_info *sp;
{
	static now;

	sp->sp_time = ++now;
	if (sp->sp_fhandle == (FILE *) 0) {
		if (++nopen >= MAXFOPEN) {
			register sp_info *ssp, *xsp;
			register time = now;

			if (db_spool)
				print("must close a file, too many open\n");
			for (ssp = sp_head; ssp; ssp = ssp->sp_next)
				if (ssp->sp_fhandle != (FILE *) 0 && ssp->sp_time < time) {
					xsp = ssp;
					time = ssp->sp_time;
				}
			sp_close(xsp);
		}
		if (db_spool)
			print("fopen(%s)\n", sp->sp_fname);
		if ((sp->sp_fhandle = fopen(sp->sp_fname, "a+")) == (FILE *) 0) {
			perror("<isis-spooler>");
			panic("can't create a spool file");
		}
		if (db_spool)
			print("after fopen %s:%x\n", sp->sp_fname, sp->sp_fhandle);
	}
}

sp_close(sp)
	register sp_info *sp;
{
	if (sp->sp_fhandle != (FILE *) 0) {
		if (db_spool)
			print("fclose(%s)\n", sp->sp_fname);
		fclose(sp->sp_fhandle);
		sp->sp_fhandle = 0;
		--nopen;
	}
}

sp_rewind(sp)
	register sp_info *sp;
{
	if (db_spool)
		print("rewind(%s)\n", sp->sp_fname);
	sp_open(sp);
	rewind(sp->sp_fhandle);
}

sp_write(match_flag, ignored, msg, newsp)
	register sp_info *ignored, *newsp;
	message *msg;
{
	register off;

	if (match_flag)
		return;
	sp_open(newsp);
	fseek(newsp->sp_fhandle, (long) 0, L_XTND);
	msg_fwrite(newsp->sp_fhandle, msg);
	fseek(newsp->sp_fhandle, (long) 0, L_SET);
	if (db_spool)
		print("fhandle(%s:%x): seek to 0\n", newsp->sp_fname, newsp->sp_fhandle);
}

message *
sp_getmsg(sp)
	register sp_info *sp;
{
	if (db_spool)
		print("sp_getmsg(%s)\n", sp->sp_fname);
	return (msg_fread(sp->sp_fhandle));
}

sp_flush(sp)
	register sp_info *sp;
{
	sp_open(sp);
	fflush(sp->sp_fhandle);
	if (db_spool)
		print("fflush(%s)\n", sp->sp_fname);
}
