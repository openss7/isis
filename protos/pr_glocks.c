/*  $RCSfile: pr_glocks.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:30 $  */
/*
 *      Originally coded by Tommy Joseph
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
#include "pr_glocks.h"

adesc glock_adesc = { sizeof(glock), sizeof(glock), 16 };

/* This code is NOT a general purpose locking mechanism.  It is designed    */
/* specifically for gbcasts, and takes advantage of the higher level gbcast */
/* abort mechanism that ensures that there can be at most one gbcast trying */
/* for an exclusive lock at any time.                                       */

shr_glock(msg_id, lname)
	register int msg_id, lname;
{
	register glock *lock;
	register qnode *qp;
	int status = 0;

#   ifdef GL_DEBUG
	print("shr_glock: msg %x lock %d\n", msg_id, lname);
#   endif GL_DEBUG
	if (lname == PROTOCOLS)
		return (status);

	lock = glock_find(lname);
	if (lock->exlock || lock->want_exlock) {
		if (!lock->want_shrlock)
			lock->want_shrlock = qu_null();
		qp = qu_add(lock->want_shrlock, msg_id, (char *) 0, nullroutine);
		status = t_wait(&qp->qu_cond, "want_shrlock");
	} else {
		if (!lock->shrlock)
			lock->shrlock = qu_null();
		qu_add(lock->shrlock, msg_id, (char *) 0, nullroutine);
	}

#   ifdef GL_DEBUG
	print("shr_glock: msg %x lock %d done\n", msg_id, lname);
#   endif GL_DEBUG

	return (status);
}

register_shrglock(msg_id, lname)
	register int msg_id, lname;
{
	register glock *lock;

#   ifdef GL_DEBUG
	print("register_shrglock: msg %x lock %d\n", msg_id, lname);
#   endif GL_DEBUG
	if (lname == PROTOCOLS)
		return;

	lock = glock_find(lname);
	if (!lock->shrlock)
		lock->shrlock = qu_null();
	qu_add(lock->shrlock, msg_id, (char *) 0, nullroutine);
}

shr_gunlock(msg_id, lname)
	register int msg_id, lname;
{
	register glock *lock;
	register qnode *qp;

#   ifdef GL_DEBUG
	print("shr_gunlock: msg %x lock %d\n", msg_id, lname);
#   endif GL_DEBUG
	if (lname == PROTOCOLS)
		return;

	if (!(qp = qu_find(glocks, lname)))
		return;

	lock = (glock *) qp->qu_data;
	if (lock->shrlock == (qnode *) 0 || !(qp = qu_find(lock->shrlock, msg_id)))
		return;
	qu_free(qp);
	if (!qu_head(lock->shrlock)) {
		qu_free(lock->shrlock);
		lock->shrlock = (qnode *) 0;
		if (!lock->exlock && lock->want_exlock) {
			qp = qu_head(lock->want_exlock);
			t_sig(&qp->qu_cond, 0);
			lock->exlock = qp->qu_name;
			qu_free(qp);
			if (!qu_head(lock->want_exlock)) {
				qu_free(lock->want_exlock);
				lock->want_exlock = (qnode *) 0;
			}
		}
	}
	if (!lock->shrlock && !lock->exlock && !lock->want_shrlock && !lock->want_exlock)
		glock_delete(lname);
}

ex_glock(msg_id, lname)		/* if already ex_locked, waits till shrlock = 0 */
	register int msg_id, lname;
{
	register glock *lock;
	register qnode *qp;
	int status = 0;

#   ifdef GL_DEBUG
	print("ex_glock: msg %x lock %d\n", msg_id, lname);
#   endif GL_DEBUG

	lock = glock_find(lname);
	if (lock->shrlock || (lock->exlock && lock->exlock != msg_id)) {
		if (!lock->want_exlock)
			lock->want_exlock = qu_null();
		qp = qu_add(lock->want_exlock, msg_id, (char *) 0, nullroutine);
		status = t_wait(&qp->qu_cond, "want_exlock");
	} else
		lock->exlock = msg_id;

#   ifdef GL_DEBUG
	print("ex_glock: msg %x lock %d done\n", msg_id, lname);
#   endif GL_DEBUG

	return (status);
}

ex_gunlock(msg_id, lname)
	register int msg_id, lname;
{
	register glock *lock;
	register qnode *qp;

#   ifdef GL_DEBUG
	print("ex_gunlock: msg %x lock %d\n", msg_id, lname);
#   endif GL_DEBUG

	if (!(qp = qu_find(glocks, lname)))
		return;
	lock = (glock *) qp->qu_data;
	if (lock->exlock != msg_id)
		return;
	lock->exlock = 0;
	if (lock->want_exlock) {
		qp = qu_head(lock->want_exlock);
		lock->exlock = qp->qu_name;
		t_sig(&qp->qu_cond, 0);
		qu_free(qp);
		if (qu_head(lock->want_exlock) == 0) {
			qu_free(lock->want_exlock);
			lock->want_exlock = (qnode *) 0;
		}
	}
	if (lock->want_shrlock) {
		if (!lock->shrlock)
			lock->shrlock = qu_null();
		while (qp = qu_head(lock->want_shrlock)) {
			t_sig(&qp->qu_cond, 0);
			qu_remove(qp);
			qu_append(lock->shrlock, qp);
		}
		qu_free(lock->want_shrlock);
		lock->want_shrlock = (qnode *) 0;
	}

	if (!lock->shrlock && !lock->exlock && !lock->want_shrlock && !lock->want_exlock)
		glock_delete(lname);
}

abort_glock(msg_id, lname)
	int msg_id, lname;
{
	register glock *lock;
	register qnode *node, *qp;

#   ifdef GL_DEBUG
	print("abort_glock: msg %x lock %d\n", msg_id, lname);
#   endif GL_DEBUG

	if (node = qu_find(glocks, lname)) {
		lock = (glock *) node->qu_data;
		if (lock->want_shrlock && (qp = qu_find(lock->want_shrlock, msg_id))) {
			t_sig(&qp->qu_cond, GB_ABORT);
			qu_free(qp);
			if (!qu_head(lock->want_shrlock)) {
				qu_free(lock->want_shrlock);
				lock->want_shrlock = (qnode *) 0;
			}
		}
		if (lock->want_exlock && (qp = qu_find(lock->want_exlock, msg_id))) {
			t_sig(&qp->qu_cond, GB_ABORT);
			qu_free(qp);
			if (!qu_head(lock->want_exlock)) {
				qu_free(lock->want_exlock);
				lock->want_exlock = (qnode *) 0;
			}
		}
		if (lock->want_shrlock && !lock->exlock && !lock->want_exlock) {
			if (!lock->shrlock)
				lock->shrlock = qu_null();
			while (qp = qu_head(lock->want_shrlock)) {
				t_sig(&qp->qu_cond, 0);
				qu_remove(qp);
				qu_append(lock->shrlock, qp);
			}
			qu_free(lock->want_shrlock);
			lock->want_shrlock = (qnode *) 0;
		}
		if (!lock->shrlock && !lock->exlock && !lock->want_shrlock && !lock->want_exlock)
			glock_delete(lname);
	}
}

glock *
glock_find(lname)
	register int lname;
{
	register qnode *node;
	register glock *lock;

	if ((node = qu_find(glocks, lname)) == 0) {
		lock = (glock *) mallocate(&glock_adesc);
		qu_add(glocks, lname, (char *) lock, nullroutine);
	} else
		lock = (glock *) node->qu_data;
	return (lock);
}

glock_delete(lname)
	register int lname;
{
	register qnode *node;

	if (node = qu_find(glocks, lname)) {
		mdeallocate(node->qu_data, &glock_adesc);
		qu_free(node);
	}
}
