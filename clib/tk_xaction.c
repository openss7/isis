/*  $RCSfile: tk_xaction.c,v $ $Revision: 2.104 $ $Date: 90/09/12 13:26:02 $  */
/******************************************************************************
 The Transaction Tool.

 Coded by Robert Cooper

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

 Supports a non-resilient distributed transaction which interacts
 with one or more disjoint databases, files, or other other non-volatile
 storage. By "non-resilient" we mean that the computation is attempted
 once and will fail if any component of the transaction fails.
 If the transaction succeeds, any updates are committed to the database.
 If it fails, the database storage is left unchanged. 

 The tool provides a framework within which the transaction executes and
 which permits the various processes involved in the transaction to agree
 on whether the transaction committed or aborted. It is up to these processes
 themselves to actually perform database writes. This tool only provides
 the agreement protocol, and a stable (actually really only non-volatile)
 log of commit records.

 The task initiating a transaction (the "coordinator") calls x_begin at the
 start of the transaction. To terminate the transaction this task calls
 x_commit or x_abort. With x_commit the coordinator may perform a 1- or
 2-phase commit.  Processes performing database operations on behalf of a
 transaction (called "participants") register interest in the termination of
 the transaction by calling x_term. The coordinator may also be a
 participant.  If the transaction commits, participants should commit their
 database updates. If the transaction aborts, they should abort their
 updates.  These participants may also vote about whether the transaction
 should commit or abort when a 2-phase commit is performed by the
 transaction coordinator. If the coordinator dies during the transaction
 commit protocol one of the participants becomes the new coordinator and
 completes the commit or abort.

 
 The transaction tool can be combined (in ways yet to be fully thought
 through!) with the coordinator-cohort tool, or the redundant computation
 tool to yield a resilient transaction.
******************************************************************************/

#include "isis.h"
#ifndef HPUX
# include <strings.h>
#endif
#include <sys/types.h>

typedef bool bfunc();

#if FUN_TYPES
/* Brain damaged C language requires ME to type all these forward declarations
   because ITS too lazy to use two passes! */
static void x_group_change(groupview * new_view, void *arg);
static void handle_x_phase(message * msg);
static void cleanup(x_id * id, x_info * info);
static bool do_prepare(x_id * id, x_info * info);
static void do_commit(x_id * id, x_info * info);
static void do_abort(x_id * id, x_info * info);
static void do_finish(x_id * id, x_info * info);
static void x_log_in_progress(char *part_name, x_id * id, message * data);
static void x_log_outcome(x_id * id, x_info * info);
static void x_pstate(char *s, int state);
static bool do_part_action(x_id * id, x_info * info, int phase);
static int x_remember_commit(x_id * id);
static int x_doterm(char *participant_name,
		    bool (*on_prepare) (x_id * id, x_info * info),
		    bool (*on_commit) (x_id * id, x_info * info),
		    bool (*on_abort) (x_id * id, x_info * info), message * data);
#else

static void x_group_change(), handle_x_phase(), cleanup(), do_commit(), do_abort(),
do_finish(), x_log_in_progress(), x_log_outcome(), x_pstate();

static bool do_prepare(), do_part_action();
static int x_remember_commit();
static int x_doterm();

#if    !(HPUX|AIXRS|SGI)
int umask(), mkdir(), fclose(), fflush(), fread(), fwrite(), ftruncate();
void free();
char *realloc();
#endif				/* HPUX */

#endif

/* x_info describes a single, in-progress transaction. */
struct x_info {
	address gaddr;			/* Group with unique name derived from x_id. */
	bool i_am_coord;		/* TRUE iff I am oldest member of group. */
	int nparticipants;		/* Total number of participants. I.e. the total number of
					   members who have ever been in the group. */
	int state;			/* Local knowledge of state of the transaction, one of
					   X_COMPUTING, X_PREPARE, X_COMMIT, X_ABORT or X_FINISHED. 
					   If state == X_PREPARE then we wish to commit, but the
					   final result maybe commit or abort. If state == X_COMMIT 
					   or X_ABORT then this is the final result, although not
					   all other sites may know this yet. If state ==
					   X_FINISHED then outcome is identical, and logged at all
					   surviving participants, and if any participants have
					   failed the outcome has been recorded at the recovery
					   manager. */
	qnode *local_participants;	/* Participants (if any) within this process, keyed by
					   participant name. */
};
static adesc xinfo_ad = { sizeof(x_info), sizeof(x_info), 8 };

/* The x_info's for all the transactions for which the coordinator or a participant
   is in this process are collected in the xactions list. Keyed by x_id. */
qnode *xactions;

/* x_part describes a local participant of some transaction, i.e. someone
   who is interested in the outcome of a transaction.
   A participant is identified by a participant name. There can be more than one
   participant for a given transaction at one process. */
struct x_part {
	union {
		struct {
			bfunc *prepare;	/* Actually returns a boolean. */
			bfunc *commit;	/* Actually returns nothing. */
			bfunc *abort;	/* Actually returns nothing. */
		} on;
		bfunc *(func[3]);	/* Indexed by X_PREPARE, X_COMMIT and X_ABORT. */
	} callbacks;
	bool term_by_ref;		/* Argument passing convention for callbacks */
};
static adesc xpart_ad = { sizeof(x_part), sizeof(x_part), 8 };

/* When the size of a log file exceeds this, it should be compacted. */
int max_log_size = 100000;

#define max_log_increment  10000	/* Amount by which to increase max_log_file \ when log
					   cannot be compacted further. */

static init_done = FALSE;		/* Only needed by dump_trans. */

void
x_init()
{
	xactions = qu_null();
	isis_entry(GENERIC_X_PHASE, handle_x_phase, "handle_x_phase");
	init_done = TRUE;
}

/*******************************************************************************
 Routines for managing the xactions list.                                    
*******************************************************************************/
static bool
qu_isnull(qp)
	qnode *qp;
{
	return (qp && (qp == qp->qu_next));
}

static qnode *
qu_find_xinfo(qp, id)
	x_id *id;
	register qnode *qp;
{
	register qnode *np;

	for (np = qp->qu_next; np != qp; np = np->qu_next) {
		if (xid_cmp(&np->qu_xid, id) == 0) {
			return (np);
		}
	}
	return (NULLQP);
}

static qnode *
qu_add_xinfo(qp, id, info)
	register qnode *qp;
	x_id *id;
	x_info *info;

  /* add a node to qp, returns np */
{
	register qnode *np;

	qu_alloc1(np, 0, 0, NULLROUTINE);
	np->qu_xid = *id;
	np->qu_xinfo = info;
	qu_append(qp, np);
	return (np);
}

static x_info *
x_find(id)
	x_id *id;

  /* Returns a pointer to the x_info, if any, for this group name stored in the xactions list.
     Otherwise returns a null pointer. */
{
	qnode *qp;

	if (qp = qu_find_xinfo(xactions, id)) {
		return (qp->qu_xinfo);
	} else {
		return ((x_info *) 0);
	}
}

static x_info *
x_add(id, gview)
	x_id *id;
	groupview *gview;

  /* Construct a new x_info for this id, append it to the xactions list and return the x_info. An
     x_info for this id must not already exist in xactions. */
{
	register x_info *info;

	info = (x_info *) mallocate(&xinfo_ad);
	info->gaddr = gview->gv_gaddr;
	info->i_am_coord = addr_ismine(&gview->gv_members[0]);
	info->nparticipants = gview->gv_nmemb;
	info->state = X_COMPUTING;
	info->local_participants = qu_null();

	qu_add_xinfo(xactions, id, info);
	return (info);
}

static void
x_free(id)
	x_id *id;

  /* If an x_info for this id exists in xactions, remove it and reclaim its storage. */
{
	register qnode *qp;

	if (qp = qu_find_xinfo(xactions, id)) {

		register qnode *parts = qp->qu_xinfo->local_participants;
		register qnode *np;

		np = parts->qu_next;
		while (np != parts) {
			mdeallocate(np->qu_xpart, &xpart_ad);
			np = np->qu_next;	/* Must get next pointer before its trashed. */
			qu_free(np->qu_last);
		}

		mdeallocate(qp->qu_xinfo, &xinfo_ad);
		qu_free(qp);
	}
}

/*******************************************************************************
 Routines for managing the local_participants lists.
*******************************************************************************/

static qnode *
qu_find_string(qp, name)
	register qnode *qp;
	char *name;
{
	register qnode *np;

	for (np = qp->qu_next; np != qp; np = np->qu_next) {
		if (strcmp(np->qu_string, name) == 0) {
			return (np);
		}
	}
	return (NULLQP);
}

#define qu_find_xpart(qp, name) qu_find_string(qp, name)
#define qu_add_xpart(qp, name, part) \
    qu_add(qp, (int) name, part, NULLROUTINE)

static x_part *
x_find_participant(info, name)
	x_info *info;
	char *name;

  /* Find the (unique) x_part, if any, for this id and name in the x_participants list. Otherwise
     return a null pointer. */
{
	qnode *qp;

	if (qp = qu_find_xpart(info->local_participants, name)) {
		return (qp->qu_xpart);
	} else {
		return ((x_part *) 0);
	}
}

static void
x_add_participant(info, name, on_prepare, on_commit, on_abort, by_ref)
	x_info *info;
	char *name;
	bfunc *on_prepare;
	bfunc *on_commit;
	bfunc *on_abort;
	bool by_ref;

  /* Construct a new x_part for this transaction and name, and append it to info->
     local_participants. An x_part for this id must not already exist in info-> local_participants. 
   */
{
	register x_part *part;

	part = (x_part *) mallocate(&xpart_ad);
	part->callbacks.on.prepare = on_prepare;
	part->callbacks.on.commit = on_commit;
	part->callbacks.on.abort = on_abort;
	part->term_by_ref = by_ref;

	qu_add_xpart(info->local_participants, name, part);

}

/*******************************************************************************
 Transaction id routines.
 We use the activity id mechanism. An activity id is a globally unique identifier.
 Each task has zero on one activity ids. A task's activity id is copied to any
 tasks it forks directly via t_fork etc., or indirectly including message handlers,
 watch procedures, and monitor procedures. Activity ids are managed in cl_isis.c

*******************************************************************************/

x_id *
x_getid()
    /* Return a pointer to the id of the current transaction, if any. This pointer may not be valid 
       after the calling task terminates, so the x_id referred to should be copied if the value is
       required beyond the lifetime of this task. If this task is not computing on behalf of any
       transaction isis_errno will contain IE_NOTRANS and a mostly harmless x_id value will be
       returned. */
{
	register int act;

	ISIS_ENTER();
	act = isis_ctp->task_act;
	if (act) {
		ISIS_RETURN(&map_act(act));
	}
	isis_errno = IE_NOTRANS;
	ISIS_RETURN(&NULLADDRESS);
}

static
x_id *
getid()
  /* Version of previous routine which doesn't call ISIS_ENTER/RETURN. Only used internally within
     this file. (Its safe to make nested pairs of ISIS_ENTER/RETURN, but it wastes a little time.) */
{
	register int act = isis_ctp->task_act;

	if (act) {
		ISIS_RETURN(&map_act(act));
	}
	isis_errno = IE_NOTRANS;
	return (&NULLADDRESS);
}

#define gnamemax ((sizeof "trans-group-999999-999999-999999-999999-999999")+1)
char *
xid_to_groupname(id, name)
	x_id *id;
	char *name;			/* Must be at least gnamemax long. */

  /* Generate a unique group name from id and store it in name. Also returns name. name must be at
     least gnamemax chars long. */
{
	sprintf(name, "trans-group-%d-%d-%d-%d-%d",
		id->addr_site, id->addr_incarn, id->addr_process, id->addr_portno, id->addr_entry);
	return (name);
}

/*******************************************************************************
 Transaction coordinator routines.
*******************************************************************************/

int
x_begin()
  /* Begin a transaction. Transacations may not nest. If we are already in a transaction, -1 will
     be returned, and isis_errno will contain IE_NESTEDTRANS. Normally 0 is returned. */
{
	address *gid;
	char gname[gnamemax];
	int old_act;

	ISIS_ENTER();

	if (old_act = act_begin()) {
		/* We're already in a transaction (or in the start up or group join activity --
		   even worse). */
		act_end(old_act);
		isis_errno = IE_NESTEDTRANS;
		ISIS_RETURN(-1);
	}
	/* From now on we will execute under this new activity id. So will all the tasks which
	   handle messages this task sends, and any watch routines we set up. */
	gid = pg_join(xid_to_groupname(getid(), gname), PG_MONITOR, x_group_change, 0, 0);
	if (aptr_isnull(gid)) {
		x_id *id;
		x_info *info;
		int err = isis_errno;

		id = getid();
		info = x_find(id);
		cleanup(id, info);

		isis_errno = err;
		ISIS_RETURN(-1);
	}

	ISIS_RETURN(0);
}

int
x_commit(phases)
	int phases;			/* Either 1 or 2 */

  /* If phase = 1, terminate the current transaction and instruct all participants of this
     transaction to commit. If phase = 2, instruct all participants to prepare, and collect their
     votes on whether to commit or abort. If at least one participant votes no, or is unable to be
     contacted, abort the transaction. All participants are then informed of the final commit/abort 
     decision. In either case, if all participants are successfully informed of the outcome,
     information about this transaction is deleted. Otherwise participant supplied data concerning
     the transaction is retained on non-volatile storage. If the transaction successfully commits 0 
     is returned. If the transaction aborts (only possible during 2 phase commit), -1 will be
     returned and isis_errno will contain IE_ABORT. If we are not the coordinator of the
     transaction, -1 will be returned and isis_errno will contain IE_NOTALLOWED. If we are not in a 
     transaction, -1 will be returned and isis_errno will contain IE_NOTRANS.  Normally 0 is
     returned. */
{
	x_id *id;
	x_info *info;

	ISIS_ENTER();
	id = getid();
	info = x_find(id);

	if (!info) {
		isis_errno = IE_NOTRANS;
		ISIS_RETURN(-1);
	}

	if (!info->i_am_coord) {
		isis_errno = IE_NOTALLOWED;
		ISIS_RETURN(-1);
	}

	switch (phases) {
	case 1:		/* Do 1 phase commmit. */
		do_commit(id, info);
		break;

	case 2:		/* Do 2 phase commit. */
		if (do_prepare(id, info)) {
			do_commit(id, info);
		} else {
			do_abort(id, info);	/* An optimization would be to send the abort only
						   to the processes which voted to commit. */
			cleanup(id, info);
			isis_errno = IE_ABORT;
			ISIS_RETURN(-1);
		}
		break;

	default:
		isis_errno = IE_BADARG;
		ISIS_RETURN(-1);
	}

	cleanup(id, info);
	ISIS_RETURN(0);
}

int
x_abort()
  /* Abort the current transaction and instruct all participants of this transaction to abort. If
     all participants are successfully informed of the abort, information about this transaction is 
     deleted. Otherwise participant supplied data concerning the transaction is retained on
     non-volatile storage. If we are not the coordinator of the transaction, -1 will be returned
     and isis_errno will contain IE_NOTALLOWED. If we are not in a transaction, -1 will be returned 
     and isis_errno will contain IE_NOTRANS. Normally 0 is returned. */
{
	x_id *id;
	x_info *info;

	ISIS_ENTER();
	id = getid();
	info = x_find(id);

	if (!info) {
		isis_errno = IE_NOTRANS;
		ISIS_RETURN(-1);
	}

	if (!info->i_am_coord) {
		isis_errno = IE_NOTALLOWED;
		ISIS_RETURN(-1);
	}

	do_abort(id, info);
	cleanup(id, info);
	ISIS_RETURN(0);
}

static void
cleanup(id, info)
	x_id *id;
	x_info *info;
{
	if (info) {
		pg_leave(&info->gaddr);
	}
	x_free(id);
	act_end(0);
};

static bool
do_prepare(id, info)
	x_id *id;
	x_info *info;
{
	register int i;
	int nreplies;
	int votes[PG_ALEN];

	nreplies = cbcast(&info->gaddr, GENERIC_X_PHASE,
			  "%d", X_PREPARE, info->nparticipants, "%d", votes);

	for (i = 0; (i < nreplies) && votes[i]; i++) {
	};
	return (i == info->nparticipants);
}

static void
do_commit(id, info)
	x_id *id;
	x_info *info;
{
	int nreplies = cbcast(&info->gaddr, GENERIC_X_PHASE,
			      "%d", X_COMMIT, info->nparticipants, "", 0);

	if (nreplies < info->nparticipants && x_remember_commit(id) < 0) {
		isis_perror("Transaction outcome not logged");
	} else {
		do_finish(id, info);
	}
}

static void
do_abort(id, info)
	x_id *id;
	x_info *info;
{
	cbcast(&info->gaddr, GENERIC_X_PHASE, "%d", X_ABORT, info->nparticipants, "", 0);
	/* We don't check the number of replies or take any special action if not all participants
	   got the abort because of the presumed abort strategy. */
	do_finish(id, info);
	/* Actually we could omit the do_finish and change handle_x_phase to do the cleanup work
	   upon receiving the X_ABORT message. */
}

static void
do_finish(id, info)
	x_id *id;
	x_info *info;
{
	cbcast_l("z", &info->gaddr, GENERIC_X_PHASE, "%d", X_FINISHED, 0);
}

/*******************************************************************************
 Group Change Monitor.
*******************************************************************************/

static void
x_group_change(new_view, arg)
	groupview *new_view;
	VOID *arg;			/* Ignored. */

  /* Called upon every group membership change. There are the following possibilities: First member 
     of group: This is the coordinator of the transaction. Other new member: This is a new
     participant process in the transaction. Coordinator departing: Must choose a new coordinator.
     If the termination protocol has not begun, the transaction can be aborted, otherwise the
     termination protocol must be pushed forward to completion. Other participant departing:
     Similarly, if the termination protocol has not begun, abort, otherwise complete the
     termination protocol. */
{
	register x_info *info = (x_info *) 0;
	bool i_was_coord;
	x_id *id;

	/* Create/find the x_info entry for this transaction. */
	id = getid();
	info = x_find(id);

#ifdef trans_debug
	print("called x_group_change: gname %s xid", new_view->gv_name);
	paddr(id);
	print(" viewid %d.%d nmemb %d\n", VMM(new_view->gv_viewid), new_view->gv_nmemb);
#endif

	if (!info) {
		/* Group change is my addition to transaction: add to xactions list. */
		x_add(id, new_view);
		return;		/* No failures to worry about. */
	}

	/* Already exists in xactions list: just update info. */
	if (info->i_am_coord) {
		i_was_coord = TRUE;
	} else {
		i_was_coord = FALSE;
		info->i_am_coord = addr_ismine(&new_view->gv_members[0]);
	}

	if (!aptr_isnull(&new_view->gv_joined)) {
		++(info->nparticipants);
		return;		/* No failures to worry about. */
	}

	if (info->i_am_coord && !aptr_isnull(&new_view->gv_departed)) {
		/* Someone has died and I am the (possibly new) coordinator of the transaction. */

		if (i_was_coord) {
#           ifdef trans_debug
			print("participant terminated\n");
#           endif
			/* A participant failed. The mainline body of the transaction or the
			   mainline x_commit or x_abort is still running and will (eventually)
			   notice the failure, so nothing needed here. (It would be nice to notify
			   the mainline transaction here to allow immediate abort but there isn't a 
			   good mechanism to do that). */
		} else {
			/* Takeover from the failed coordinator. */
#           ifdef trans_debug
			print("taking over from failed coordinator\n");
#           endif
			switch (info->state) {
			case X_COMPUTING:
				/* The commit/abort protocol hasn't begun so just abort the whole
				   transaction. */
				do_abort(id, info);
				break;

			case X_PREPARE:
				/* Every operational member received the prepare message but we
				   don't know what they voted. So re-collect the votes and continue 
				   the protocol. */
				if (do_prepare(id, info)) {
					do_commit(id, info);
				} else {
					do_abort(id, info);
				}
				break;

			case X_COMMIT:
				/* Every operational member received the commit message. */
				if (x_remember_commit(id) < 0) {
					isis_perror("Transaction outcome not logged");
				} else {
					do_finish(id, info);
				}
				break;

			case X_ABORT:
				/* Either: I received an abort message and so did all other
				   operational members, Or: I received a prepare message, voted to
				   abort and moved to the abort state, while other members may be
				   waiting for a commit/abort decision. We can't tell these two
				   cases apart so (re)do the abort. */
				do_abort(id, info);
				break;
			case X_FINISHED:
				break;
			}	/* switch */
		}		/* if i_was_coord */
	}			/* if i_am_coord && departed */
}

/*******************************************************************************
 Participant routines.
*******************************************************************************/

/* Expected arguments for x_term. 
int 
x_term(participant_name, on_prepare, on_commit, on_abort, fmt, arg1, arg2, ...)
  char *participant_name;
  bfunc *on_prepare;
  bfunc *on_commit;
  bfunc *on_abort;
  char *fmt;
  Number and types of remaining arguments are specified by fmt string.
*/
int
x_term(va_alist)
	va_dcl

    /* Register interest in the termination of current transaction. The participant_name (a string) 
       identifies the "entity" that is participating in this transaction. An entity might be a
       database system for instance. The participant_name must be unique among all processes in all 
       executing Isis applications. This prevents confusion when multiple entities participate in
       the same transaction. Using the current process id to make a unique participant name is not
       a good idea if you want to recover from participant failures since the recovered process
       will have a different process id.

       If the transaction terminates with a 2-phase commit then the on_prepare routine of every
       participant is called and the votes collected. If any participant votes FALSE, or is unable
       to be contacted, the transaction will abort, and the participant's on_abort routines will be 
       called. Otherwise the transaction commits and all the on_commit routines are called. For a
       1-phase commit, only the on_commit routines are called. If the transaction aborts the
       on_abort routines are called.

       The fmt and the variable number of following arguments provide some data associated with
       this transaction. This data is stored at least until the participant's on_commit or on_abort 
       routine is called for this transaction. If the participant fails before one of these
       routines is called, and then recovers, it should call x_outcomes to determine if the
       transaction did in fact abort or commit. The x_outcomes routine will return the participant
       supplied data to enable the participant to locate partially modified data objects etc. which 
       must be brought to consistency.

       Typically if the transaction failed before the prepare phase was complete (i.e. x_info.state 
       == X_COMPUTING or X_PREPARE) the transaction will abort. Otherwise the transaction will
       commit or abort as indicated by x_info.state equalling X_COMMIT or X_ABORT. In the latter
       case the participant may be unable to achieve database consistency (e.g. because the
       database assumed that an abort had occurred, while Isis had decided to commit) and the
       participant should inform some higher authority (e.g. a human). */
{
	char *participant_name;
	bfunc *on_prepare;
	bfunc *on_commit;
	bfunc *on_abort;
	message *data;
	va_list vargs;

	ISIS_ENTER();
	data = msg_newmsg();
	va_start(vargs);
	begin {
		participant_name = VA_REF(vargs, char *);

		on_prepare = VA_REF(vargs, bfunc *);
		on_commit = VA_REF(vargs, bfunc *);
		on_abort = VA_REF(vargs, bfunc *);
		if (msg_doputf(data, SYSFLD_SCAN, &vargs) == -1) {
			msg_delete(data);
			ISIS_RETURN(-1);
		}
	}
	va_end(vargs);
	ISIS_RETURN(x_doterm(participant_name, on_prepare, on_commit, on_abort, data));
}

int
x_term_msg(participant_name, on_prepare, on_commit, on_abort, data)
	char *participant_name;
	bfunc *on_prepare, *on_abort, *on_commit;
	message *data;
{
	ISIS_ENTER();
	ISIS_RETURN(x_doterm(participant_name, on_prepare, on_commit, on_abort, data));
}

static int
x_doterm(participant_name, on_prepare, on_commit, on_abort, data)
	char *participant_name;
	bfunc *on_prepare, *on_commit, *on_abort;
	message *data;
{
	x_id *id;
	x_info *info;
	char gname[gnamemax];
	address *gid;

	id = getid();
	info = x_find(id);
	if (!info) {
		/* First time transaction has touched this process. */
		gid = pg_join(xid_to_groupname(id, gname), PG_MONITOR, x_group_change, 0, 0);
		if (aptr_isnull(gid)) {
			int err = isis_errno;

#ifdef trans_debug
			print("x_term: join failed\n");
#endif
			info = x_find(id);
			cleanup(id, info);
			msg_delete(data);
			isis_errno = err;
			return (-1);
		}
		info = x_find(id);	/* Guaranteed to succeed after the join. */
		if (!info) {
			cl_dump(-1, "about to panic after x_find");
			paddr(id);
			paddr(gid);
			panic(" cl_xaction:x_term: x_find failed after join");
		}
	}

	if (x_find_participant(info, participant_name)) {
		/* Already a participant. */
		msg_delete(data);
		isis_errno = IE_PARTICIPANT;
		return (-1);
	}

	x_add_participant(info, participant_name, on_prepare, on_commit, on_abort,
			  isis_state & ISIS_XBYREF);
	x_log_in_progress(participant_name, id, data);
	msg_delete(data);
	return (0);
}

static void
handle_x_phase(msg)
	message *msg;

  /* Input: "%d" phase (one of X_PREPARE, X_COMMIT, X_ABORT, or X_FINISHED). Output: "%d", vote
     (only meaningful for phase == X_PREPARE). There is no reply for phase == X_FINISHED. */
{
	x_id *id;
	x_info *info;
	int phase;
	int result = FALSE;		/* Really only meaningful for X_PREPARE phase. */

	msg_get(msg, "%d", &phase);
	id = getid();
	info = x_find(id);

	if (!info) {
#       ifdef trans_debug
		print("cl_xaction.c:handle_x_phase: no info on trans");
		paddr(id);
		x_pstate(" in phase ", phase);
		print("\n");
#       endif

		if (phase == X_FINISHED) {
			return;	/* We must be the coordinator, and x_commit or x_abort has already
				   cleaned up. */
		} else {
			reply(msg, "%d", FALSE);	/* Must have aborted. */
			return;
		}
	}

	switch (phase) {
	case X_PREPARE:
		switch (info->state) {
		case X_COMPUTING:	/* Two phase commit: time to prepare. */
#           ifdef trans_debug
			paddr(id);
			print("Two phase commit: time to prepare\n");
#           endif
			result = do_part_action(id, info, X_PREPARE);
			if (result) {
				info->state = X_PREPARE;
			} else {
				do_part_action(id, info, X_ABORT);
				info->state = X_ABORT;
				x_log_outcome(id, info);
			}
			break;
		case X_PREPARE:	/* Duplicate prepare. */
#           ifdef trans_debug
			paddr(id);
			print("Duplicate prepare, prepared\n");
#           endif
			result = TRUE;
			break;
		case X_ABORT:	/* Duplicate prepare. */
#           ifdef trans_debug
			paddr(id);
			print("Duplicate prepare, aborted\n");
#           endif
			result = FALSE;
			break;
		default:
			goto trouble;
		};
		break;
	case X_COMMIT:
		switch (info->state) {
		case X_COMPUTING:	/* One phase commit: time to commit. */
		case X_PREPARE:	/* Two phase commit: time to commit. */
#           ifdef trans_debug
			if (info->state == X_COMPUTING) {
				paddr(id);
				print("One phase commit: time to commit\n");
			} else {
				paddr(id);
				print("Two phase commit: time to commit\n");
			}
#           endif
			do_part_action(id, info, X_COMMIT);
			info->state = X_COMMIT;
			x_log_outcome(id, info);
			break;
		case X_COMMIT:	/* Duplicate commit. */
#           ifdef trans_debug
			paddr(id);
			print("Duplicate commit\n");
#           endif
			break;
		default:
			goto trouble;
		};
		break;
	case X_ABORT:
		switch (info->state) {
		case X_COMPUTING:	/* One or two phase commit: time to abort. */
		case X_PREPARE:	/* Two phase commit: time to abort. */
#           ifdef trans_debug
			paddr(id);
			print("One or two phase commit: time to abort\n");
#           endif
			do_part_action(id, info, X_ABORT);
			info->state = X_ABORT;
			x_log_outcome(id, info);
			break;
		case X_ABORT:	/* Duplicate abort. */
#           ifdef trans_debug
			paddr(id);
			print("Duplicate abort\n");
#           endif
			break;
		default:
			goto trouble;
		};
		break;
	case X_FINISHED:
		switch (info->state) {
		case X_COMMIT:
		case X_ABORT:
#           ifdef trans_debug
			paddr(id);
			print("Time to finish\n");
#           endif
			info->state = X_FINISHED;
			/* Outcome is stable at all sites, no-one will ever ask about the outcome
			   again: delete all data about this transaction. */
			cleanup(id, info);
			return;	/* No reply to X_FINISHED message. */
		default:
			goto trouble;
		};
		break;

	default:
		goto trouble;
	};

	/* Send reply to coordinator. */
	reply(msg, "%d", result);
	return;

      trouble:
	x_pstate("cl_xaction.c:handle_x_phase: bad phase ", phase);
	x_pstate(" for state", info->state);
	print(" in transaction ");
	paddr(id);
	panic("");
}

static bool
do_part_action(id, info, phase)
	x_id *id;
	x_info *info;
	int phase;

  /* The phase argument is one of X_PREPARE, X_COMMIT, or X_ABORT. For each element in participants 
     execute the on_prepare, on_commit, or on_abort routines, as indicated by "phase". In the case 
     of X_PREPARE, AND together the results from the on_prepare calls and return this. In the case
     of X_COMMIT and X_ABORT, the return value is immaterial. */
{
	bool vote = TRUE;
	register qnode *np, *qp;

	qp = np = info->local_participants;
	while ((np = np->qu_next) != qp) {
		if (np->qu_xpart->term_by_ref) {
			vote = vote
			    && _ISISCALL1((ifunc *) np->qu_xpart->callbacks.func[phase], &id);
		} else {
			vote = vote
			    && _ISISCALL1((ifunc *) np->qu_xpart->callbacks.func[phase], id);
		}
	}
	return (vote);
}

/*******************************************************************************
 Routines for recovering transactions after failure.
*******************************************************************************/

/* 
  Each participant log contain three kinds of records:
     X_COMPUTING transaction-id user-supplied-data
     X_COMMIT    transaction-id
     X_ABORT     transaction-id
  For a given transaction id there should be the following possible record
  sequences:  X_COMPUTING 
              X_COMPUTING X_COMMIT
              X_COMPUTING X_ABORT
  and thus after a crash of the participant's process the log records those
  transactions for which the commit/abort protocol may not have completed.
  When x_outcomes is called the final outcome of such incomplete transactions
  is obtained from the transaction recovery group.
  (Actually its not even necessary for the log to record the
  commit or abort, only whether the transaction completely terminated.) 

  We require the client to choose participant names such that only one process
  is reading or writing a given participant log file. We construct log files
  names as:   <isis-directory>/translogs/<part-name>
  Thus users must choose participant names which are unique among all processes
  in all executing Isis applications.
  We could use advisory file locks on those Unixes that have them to enforce
  this---but we don't.
*/

static FILE *
x_open_log(part_name, mode)
	char *part_name;
	char *mode;
{
	char *name;
	FILE *file;

	name = malloc(strlen(isis_dir) + sizeof(xlog_subdir) + strlen(part_name) + 3);
	strcpy(name, isis_dir);
	strcat(name, "/");
	strcat(name, xlog_subdir);
	strcat(name, "/");
	strcat(name, part_name);

	file = fopen(name, mode);
	if (!file) {
		/* Can't open file: Try to create ISIS xlogs directory. */
		char *dirname;
		int old_mask;

		dirname = malloc(strlen(isis_dir) + sizeof(xlog_subdir) + 2);
		strcpy(dirname, isis_dir);
		strcat(dirname, "/");
		strcat(dirname, xlog_subdir);

		old_mask = umask(0);
		mkdir(dirname, 0777);
		umask(old_mask);

		/* Try to open the file again. */
		file = fopen(name, mode);
		free(dirname);
	}
	free(name);
	return (file);
}

static FILE *
x_append_log(part_name)
	char *part_name;
{
	/* Keep the last log file we used open, since we are likely to use it again. */
	static FILE *log_file = (FILE *) 0;	/* Current open log file, null pointer if there is
						   no open file. */
	static char null_str[] = "";
	static char *prev_part_name = null_str;	/* Participant name from previous call. Undefined
						   if log_file is null. */

	if (log_file) {
		if (strcmp(prev_part_name, part_name) == 0) {	/* Same file as last time. */
			return (log_file);
		} else {
			fclose(log_file);
		}
	}

	log_file = x_open_log(part_name, "a+");
	if (log_file) {		/* Success. */
		prev_part_name = part_name;
	}
	return (log_file);	/* Return possibly null pointer. */
}

static x_list *
x_read_log(log_file)
	FILE *log_file;

  /* Reads log_file and returns an x_list structure derived from it. There will be an item in the
     x_list for every X_COMPUTING log entry which does not have a subsequent X_COMMIT/X_ABORT
     entry. The x_item.outcome fields are not filled in yet (that information is not in the
     participant logs). The caller should free the x_list. */
{
	long entry_type;
	x_id id;
	message *msg;
	int max_size = 100, size = 0;

#   define xlist_size(n) (sizeof(int) + sizeof(x_item) * n)
	x_list *result = (x_list *) malloc(xlist_size(max_size));
	x_item *items = result->items;
	bool found;
	int i;

	if (!log_file) {
		/* Non-existant log file. */
		result->len = 0;
		return (result);
	}

	while (fread(&entry_type, sizeof(long), 1, log_file)) {
		if (!fread(&id, sizeof(x_id), 1, log_file)) {
			print("Transaction tool: badly truncated log file: repairing\n");
			break;
		}

		/* Lookup items for matching entry, if any. */
		found = FALSE;
		for (i = 0; i < size; i++) {
			if (xid_cmp(&(items[i].id), &id) == 0) {
				found = TRUE;
				break;
			}
		}

		switch (entry_type) {
		case X_COMPUTING:
			if (!(msg = msg_fread(log_file))) {
				print("Transaction tool: badly truncated log file: repairing\n");
				break;
			}
			if (!found) {	/* Add new item. */
				if (size >= max_size) {
					/* Double the size of the items array. */
					max_size *= 2;
					result = (x_list *) realloc(result, xlist_size(max_size));
					items = result->items;	/* Pointer may have moved! */
				}

				items[size].id = id;
				items[size].info = msg;
				size++;
			} else {
				print("Transaction tool: duplicate items for ");
				paddr(&id);
				print(" in log file: repairing\n");
			}
			break;
		case X_COMMIT:
		case X_ABORT:
			if (found) {
				/* Delete ith item by copying last item over it. */
				msg_delete(items[i].info);
				size--;

				if (i < size) {	/* Check we're not deleting size'th item. */
					items[i].id = items[size].id;
					items[i].info = items[size].info;
				}
			} else {
				print("Transaction tool: missing item for ");
				paddr(&id);
				print(" in log file: repairing\n");
			}
			break;
		}
	}

	result->len = size;
	return (result);
}

static void
x_write_log_item(entry_type, id, data, log_file)
	int entry_type;
	x_id *id;
	message *data;
	FILE *log_file;
{
	fwrite(&entry_type, sizeof(long), 1, log_file);
	fwrite(id, sizeof(x_id), 1, log_file);
	switch (entry_type) {
	case X_COMPUTING:
		msg_fwrite(log_file, data);
		fflush(log_file);
		break;
	case X_COMMIT:
		fflush(log_file);
		break;
	case X_ABORT:
		break;		/* Presumed abort: don't need to flush. */
	default:
		panic("cl_xaction.c:x_write_log_item: bad outcome\n");
	}
}

static void
x_compact_log(log_file)
	FILE *log_file;

  /* Compact the log_file by removing matching X_COMPUTING--X_COMMIT/X_ABORT pairs. We currently do 
     this by overwriting the old log file. This is not robust to failures during the compaction
     process, we should write a new file and do a changename and then a delete. */
{
	x_list *outcomes;
	register int i, size;
	register x_item *items;

	rewind(log_file);
	outcomes = x_read_log(log_file);
	size = outcomes->len;
	items = outcomes->items;

	ftruncate(fileno(log_file), 0);
	for (i = 0; i < size; i++) {
		x_write_log_item(X_COMPUTING, &items[i].id, items[i].info, log_file);
	}
	free(outcomes);

	size = ftell(log_file);
	if (size > max_log_size - max_log_increment) {
		/* Ensure file has some room for growth before next compaction. */
		max_log_size = size + max_log_increment;
	}
}

static void
x_log_in_progress(part_name, id, data)
	char *part_name;
	x_id *id;
	message *data;
{
	FILE *log_file;

	log_file = x_append_log(part_name);
	if (!log_file) {
		print("Transaction tool (cl_xaction.c) error: ");
		print("could not open log file %s/%s/%s", isis_dir, xlog_subdir, part_name);
		perror("");
		panic("Transaction will not be logged\n");
	}
	x_write_log_item(X_COMPUTING, id, data, log_file);
}

static void
x_log_outcome(id, info)
	x_id *id;
	x_info *info;

  /* Write X_COMMIT/X_ABORT entries to all local participant logs. */
{
	qnode *parts, *qp;
	int outcome = info->state;

	parts = info->local_participants;
	for (qp = parts->qu_next; qp != parts; qp = qp->qu_next) {
		char *part_name = qp->qu_string;
		FILE *log_file;

		log_file = x_append_log(part_name);
		if (!log_file) {
			print("Transaction tool (cl_xaction.c) error: ");
			print("could not open log file %s/%s/%s", isis_dir, xlog_subdir, part_name);
			perror("");
			panic("Transaction will not be logged\n");
		}
		x_write_log_item(outcome, id, NULLMP, log_file);

		if (ftell(log_file) > max_log_size) {
			x_compact_log(log_file);
		}
	}
}

static void
group_mon(gview, cond_ptr)
	groupview *gview;
	int cond_ptr;
{
	if (gview->gv_nmemb == 0) {
		t_sig((condition *) cond_ptr, 0);
	}
}

static void
wait_for_term(gaddr)
	address *gaddr;

  /* Wait for the transaction being processed by group gaddr to terminate. */
{
	int mon_id;
	condition *group_death = (condition *) malloc(sizeof(condition));

#   ifdef trans_debug
	print("waiting for transaction completion...");
#   endif
	pg_client(gaddr, "");
	mon_id = pg_monitor(gaddr, group_mon, group_death);
	if (mon_id > 0) {	/* Group still alive. */
		t_wait_l(group_death, "waiting for transaction termination");
		pg_watch_cancel(mon_id);
	}
#   ifdef trans_debug
	print("\n");
#   endif
	free(group_death);
}

x_list *
x_outcomes(part_name)
	char *part_name;

  /* Returns a pointer to an x_list structure which describes the outcomes of transactions which
     were in-progress when we crashed. A null pointer is returned and isis_errno is set to
     IE_XNORECOV if the transaction recovery manager cannot be contacted. Caller should deallocate
     result and purge log by calling x_outcomes_done. */
{
	address *recov_gaddr;
	FILE *log_file;
	x_list *result;
	register int i;

	ISIS_ENTER();

	recov_gaddr = pg_lookup(xmgr_service);
	if (aptr_isnull(recov_gaddr)) {
		isis_errno = IE_NOTRANSRECOV;
		ISIS_RETURN((x_list *) 0);
	}

	log_file = x_open_log(part_name, "r+");
	result = x_read_log(log_file);

	for (i = 0; i < result->len; i++) {
		x_id *id;
		x_info *info;
		char x_gname[gnamemax];	/* Transaction group. */
		address *x_gaddr;
		int nreplies;

		id = &(result->items[i].id);

		/* Check we are already in this transaction. */
		info = x_find(id);
		if (info) {
			continue;	/* Ignore this log record. */
		}

		/* See if we are recovering while the transaction is still in progress. */
		x_gaddr = pg_lookup(xid_to_groupname(id, x_gname));
		if (!aptr_isnull(x_gaddr)) {
			/* Transaction still in progress: wait for completion. */
			wait_for_term(x_gaddr);

		}

		/* Consult transaction recovery manager. */
		nreplies = cbcast(recov_gaddr, XR_GET_OUTCOME, "%A", id, 1,
				  1, "%d", &(result->items[i].outcome));
		if (nreplies != 1) {
			/* Give up? Maybe we should just ignore this record. */
			isis_errno = IE_NOTRANSRECOV;
			free(result);
			ISIS_RETURN((x_list *) 0);
		}
	}

	if (log_file) {
		fclose(log_file);
	}
	ISIS_RETURN(result);
}

void
x_outcomes_flush(part_name, outcomes)
	char *part_name;
	x_list *outcomes;
{
	FILE *log_file;

	ISIS_ENTER();
	free(outcomes);
	log_file = x_open_log(part_name, "r+");
	if (log_file) {
		ftruncate(fileno(log_file), 0);
		fclose(log_file);
	}
	ISIS_EXIT();
}

static int
x_remember_commit(id)
	x_id *id;

  /* -1 is returned and isis_errno is set to IE_XNORECOV if the transaction recovery manager cannot 
     be contacted. Otherwise 0 is returned. */
{
	address *gaddr;
	int nreplies;

	gaddr = pg_lookup(xmgr_service);
	if (aptr_isnull(gaddr)) {
		isis_errno = IE_NOTRANSRECOV;
		return (-1);
	}

	nreplies = cbcast(gaddr, XR_SAVE_OUTCOME, "%A %d", id, 1, X_COMMIT, 1, "");

	if (nreplies != 1) {
		isis_errno = IE_NOTRANSRECOV;
		return (-1);
	}
	return (0);
}

/*******************************************************************************
 Dump routines.
*******************************************************************************/

static void
x_pstate(s, state)
	char *s;			/* Prefix message. */
	int state;
{
	switch (state) {
	case X_COMPUTING:
		print("%sCOMPUTING\n", s);
		break;
	case X_PREPARE:
		print("%sPREPARE\n", s);
		break;
	case X_COMMIT:
		print("%sCOMMIT \n", s);
		break;
	case X_ABORT:
		print("%sABORT\n", s);
		break;
	case X_FINISHED:
		print("%sFINISHED\n", s);
		break;
	default:
		print("%sBad state %d\n", s, state);
		break;
	}
}

void
dump_trans()
{
	register qnode *qp1;
	qnode *parts;
	x_id *id;
	int act;

	if (!init_done) {
		/* we can be called before x_init is called. */
		return;
	}

	if (!qu_isnull(xactions)) {
		print("Transactions: \n");
		for (qp1 = xactions->qu_next; qp1 != xactions; qp1 = qp1->qu_next) {
			x_info *info = qp1->qu_xinfo;

			id = &(qp1->qu_xid);

			print("\tID=");
			paddr(id);
			if ((act = find_act(id)) >= 0) {
				print("[%d]", act);
			}
			x_pstate(" ", info->state);

			print("\t%d Participant Processes\n", info->nparticipants);

			if (!qu_isnull(parts = info->local_participants)) {
				register qnode *qp2;

				print("\tParticipants in this process: ");
				for (qp2 = parts->qu_next; qp2 != parts; qp2 = qp2->qu_next) {
					print("\"%s\" ", qp2->qu_string);
				}
				print("\n");
			}
		}
	}
}
