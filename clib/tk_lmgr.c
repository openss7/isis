/*  $RCSfile: tk_lmgr.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:53 $  */
/*
 *	Originally coded by Ken Kane
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
#define ISIS_SYS
#include "isis.h"

int log_in_replay = 0;			/* Process currently replaying logged messages flag */
int PG_LOGGED = _PG_LOGGED;

static log_nod *l_list = (log_nod *) 0;	/* List of log nodes */
static long log_count = 0;		/* Timestamp of last logged message */
static int l_initialized = 0;		/* Log manager initialized? */
static condition l_newtimer = (condition) 0;	/* Condition on which timer waits */

					     /* when there are no flush timers */
static int l_replay_entry_set = 0;	/* GENERIC_ENDLOGREPLAY entry bound? */
static long l_req_proc_count = 0L;	/* Number of messages automatically logged by system whose
					   task's have not yet completed */
static int in_manual_write = 0;		/* Currently writing a manually logged message? */

/* The following variables are used when a new checkpoint is taken */

static FILE *ckpt_fp;			/* FILE ptr for temporary log */
static int ckpt_fd;			/* File descriptor for temporary log */
static char *ckpt_tname;		/* Name of temporary log file */
static log_hdr ckpt_hdr;		/* Header of new log file */

static int
 l_rec_flush(), l_open_log(), l_create_log(), l_flush_reqs(), l_next_replay_req(), l_take_ckpt();
static void
 l_lookup_log_reqs(), l_replay_reset(), l_endlogreplay(), l_timer();

/* LOG_INIT:    Create/Re-open a log for the given group and return a pointer
                to the log info structure for the new log.

                        fname           Name of file containing log
                        gname           Group name
                        replay_entno    Entry to replay manually logged msgs
                        flush_type      Auto or manual flush of logged msgs
                        end_of_replay   Routine to call at end-of-replay
			cold_start	L_WARM = Recover from log if log exists
					L_COLD = Ignore contents of log
			ignore_old	L_ERROR = Error if old format log found
					L_COLD = Ignore log contents if old format */
log_nod *
log_init(fname, gname, replay_entno, flush_type, end_of_replay, cold_start, ignore_old)
	register char *fname, *gname;
	int
	 replay_entno, flush_type;
	int (*end_of_replay) ();
	int
	 cold_start, ignore_old;
{
	register log_nod *new_node;
	FILE *l_fp;
	register char *l_name;
	log_hdr l_hdr;
	register ginfo *gip;
	int err_stat;

	/* Bind the entry for receiving log_flush requests if not yet bound */
	if (!l_initialized) {
		isis_entry(GENERIC_LOG_FLUSH, (vfunc *) l_rec_flush, "l_rec_flush");
		isis_task(l_timer, "ISIS Log Manager Flush Timer");
		t_fork(l_timer, 0);
		l_initialized++;
	}

	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		return (NULL);
	}
	if (gip->gi_lnode != NULL) {
		isis_errno = IE_RELOG;
		return (NULL);
	}

	/* Create the full path name for the log and open it. */
	l_name = malloc(strlen(isis_dir) + sizeof(LOG_SUBDIR) + strlen(fname) + 3);
	strcpy(l_name, isis_dir);
	strcat(l_name, "/");
	strcat(l_name, LOG_SUBDIR);
	strcat(l_name, "/");
	strcat(l_name, fname);

	if (cold_start) {
		err_stat = unlink(l_name);
		if ((err_stat != 0) && (errno != ENOENT))
			panic("Can't erase old log for a cold start %d", errno);
	}

	if (err_stat = l_open_log(l_name, &l_fp, &l_hdr, ignore_old)) {
		free(l_name);
		isis_errno = err_stat;
		return (NULL);
	}

	/* Create and fill in the log info structure */
	new_node = (log_nod *) malloc(sizeof(log_nod));
	new_node->fname = malloc(strlen(fname) + 1);
	strcpy(new_node->fname, fname);
	new_node->l_name = l_name;
	new_node->l_fp = l_fp;
	new_node->I_recovered = 0;
	new_node->end_replay = end_of_replay;
	new_node->replay_entry = replay_entno;
	new_node->flush_type = flush_type;
	new_node->req_q_len = 0;
	new_node->count_thresh = DEF_CNT_THRESH;
	new_node->reqs_thresh = DEF_REQS_THRESH;
	new_node->len_thresh = DEF_LEN_THRESH;
	new_node->last_flush = time((time_t *) 0);
	new_node->timer = DEF_TIMER;
	if (new_node->timer)
		t_sig(&l_newtimer, 0);
	new_node->inhibited = 0;
	new_node->cold_start = cold_start;
	new_node->l_hdr = l_hdr;
	new_node->req_q_hd = NULL;
	new_node->req_q_tl = NULL;
	new_node->last_ts = 0L;
	l_lookup_log_reqs(new_node);
	if ((new_node->next = l_list) != NULL)
		l_list->prev = new_node;
	new_node->prev = (log_nod *) 0;
	l_list = new_node;

	gip->gi_lnode = new_node;

	return (new_node);
}

/* LOG_CNTTHRESH:       Set the number of messages that should be buffered
			for the named process group before the message buffer
			is flushed to the log.  */
int
log_cntthresh(gname, cnt)
	char *gname;
	int cnt;
{
	register log_nod *l_node;
	register ginfo *gip;

	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		return (isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		return (isis_errno);
	}

	l_node->count_thresh = cnt;
	return (0);
}

/* LOG_REQSTHRESH:      Set the number of messages that should be logged
			for the named group before a checkpoint is forced.  */
int
log_reqsthresh(gname, cnt)
	char *gname;
	int cnt;
{
	register log_nod *l_node;
	register ginfo *gip;

	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		return (isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		return (isis_errno);
	}

	l_node->reqs_thresh = cnt;
	return (0);
}

/* LOG_LENTHRESH:       Set the size of the log file for the named group
			at which a checkpoint is forced.  */
int
log_lenthresh(gname, cnt)
	char *gname;
	int cnt;
{
	register log_nod *l_node;
	register ginfo *gip;

	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		return (isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		return (isis_errno);
	}

	l_node->len_thresh = cnt;
	return (0);
}

/* LOG_TIMER:           Set the frequency in seconds at which the named group's
			message buffer should be flushed.  A frequency of zero
			indicates that no timer should be used.  */
int
log_timer(gname, freq)
	char *gname;
	unsigned freq;
{
	register log_nod *l_node;
	register ginfo *gip;

	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		return (isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		return (isis_errno);
	}

	l_node->timer = freq;
	if (freq)
		t_sig(&l_newtimer, 0);
	return (0);
}

/* LOG_DISABLE_CKPT:	Inhibit checkpointing at the specified group. */

int
log_disable_ckpt(gname)
	char *gname;
{
	register log_nod *l_node;
	register ginfo *gip;

	ISIS_ENTER();
	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		ISIS_RETURN(isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		ISIS_RETURN(isis_errno);
	}

	l_node->inhibited = 1;
	ISIS_RETURN(0);
}

/* LOG_ENABLE_CKPT:	Allow checkpointing at the specified group. */

int
log_enable_ckpt(gname)
	char *gname;
{
	register log_nod *l_node;
	register ginfo *gip;

	ISIS_ENTER();
	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		ISIS_RETURN(isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		ISIS_RETURN(isis_errno);
	}

	l_node->inhibited = 0;
	if ((l_node->flush_type == L_AUTO) &&
	    ((l_node->replay_entry != 0) || (l_req_proc_count == 0)) &&
	    (((l_node->log_reqs + l_node->req_q_len) > l_node->reqs_thresh) ||
	     (l_node->wri_off > l_node->len_thresh)))
		if (l_take_ckpt(gip) != 0)
			panic("Allowed checkpoint failure %d", isis_errno);

	ISIS_RETURN(0);
}

/* LOG_ACTION:  Determine if this process can create and initialize the named
                group (L_INIT), recovered the group from its log (L_RECOVER),
                or should only join, not create, the group (L_JOIN).  Also,
                return the next incarnation number for the group.

                        gname           Group name
                        new_incarn_p    Ptr to variable to hold new incarn no.  */

int
log_action(gname, new_incarn_p)
	char *gname;
	int *new_incarn_p;
{
	register log_nod *l_node;
	int old_incarn;
	register ginfo *gip;
	int stat;

	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		return (isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		return (isis_errno);
	}

	stat = rmgr_lasttofail(gip->gi_gname, l_node->fname, &old_incarn, 0);

	if (l_node->cold_start)
		return (L_INIT);	/* Cold start so log erased */
	if (stat == 0)
		return (L_JOIN);	/* Not last to fail */
	if (stat == 1) {	/* Last to fail */
		*new_incarn_p = old_incarn + 1;
		return (L_RECOVER);
	}
	if (stat == 2) {	/* No recorded view for group */
		/* Group did not exist for long, if at all.  Can either initialize or recover from
		   log because nothing intentionally log_flushed by application.  If it were, a
		   view would have been logged */
		if ((l_node->log_reqs == 0) && (l_node->l_hdr.is_ckpt == L_NOCKPT))
			return (L_INIT);
		*new_incarn_p = 1;
		return (L_RECOVER);
	}
	panic("Unknown log status %d in log_action", stat);
	return (-1);
}

/* LOG_START_RECOVERY:  Used by pg_join to signal log manager that the named
                        group will be recovered from the log at this process */
int
log_start_recovery(gname)
	char *gname;
{
	register log_nod *l_node;
	register ginfo *gip;

	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		return (isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		return (isis_errno);
	}

	l_node->I_recovered = 1;

	return (0);
}

/* LOG_REMOVE:  Stop logging the named group and remove the logging info
                structure for it.  Note that the log will not actually
                be deleted, in case we wish to recover from it later. */
int
log_remove(gname)
	char *gname;
{
	register log_nod *l_node;
	register req_nod *req_del, *req_next;
	register ginfo *gip;

	gip = map_gname(gname);
	if (gip == NULL)
		return (0);
	l_node = gip->gi_lnode;
	if (l_node == NULL)
		return (0);

	fclose(l_node->l_fp);

	free(l_node->fname);
	free(l_node->l_name);

	/* Clear the qnode of buffered messages for this log */
	req_del = l_node->req_q_tl;
	while (req_del != NULL) {
		req_next = req_del->next;
		msg_delete(req_del->msg);
		free(req_del);
		req_del = req_next;
	}

	gip->gi_lnode = NULL;
	if (l_list == l_node)
		l_list = l_node->next;
	if (l_node->prev != NULL)
		l_node->prev->next = l_node->next;
	if (l_node->next != NULL)
		l_node->next->prev = l_node->prev;
	free(l_node);

	return (0);
}

/* LOG_HAS_CKPT:        Returns TRUE iff there is a checkpoint in the log
                        for the named group.  Note, there may still be
                        logged messages even if there is no checkpoint.  In
                        this case, it is assumed that the initial group
                        state should be the start state from which the logged
                        messages are replayed.  */
int
log_has_ckpt(gname)
	char *gname;
{
	register log_nod *l_node;
	register ginfo *gip;

	ISIS_ENTER();
	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		ISIS_RETURN(isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		ISIS_RETURN(isis_errno);
	}

	ISIS_RETURN(l_node->l_hdr.is_ckpt);
}

/* LOG_CHECKPOINT:      Force a checkpoint to the log of the named group. */

int
log_checkpoint(gname)
	char *gname;
{
	register log_nod *l_node;
	register ginfo *gip;

	ISIS_ENTER();
	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		ISIS_RETURN(isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		ISIS_RETURN(isis_errno);
	}
	ISIS_RETURN(l_take_ckpt(gip));
}

/* LOG_WRITE_MSG:       Write the given message (destined to the given entry)
                        to the log of the group whose ginfo block is
                        specified.  Actually, the message is buffered to be
                        written at the time of the next log_flush.

                                gip     Ptr to group ginfo structure
                                msg     Ptr to message to write
                                entry   Entry pt where msg received by application */
int
log_write_msg(gip, msg, entry)
	register ginfo *gip;
	message *msg;
	int entry;
{
	register log_nod *l_node;
	register req_nod *r_node;

	l_node = gip->gi_lnode;
	if (l_node == NULL)
		panic("Can't log for a non-logged group");

	/* If this message is automatically being logged, record the fact that a logged message has 
	   started but not completed its task.  This is so that checkpoints can be taken at times
	   consistent with the set of logged messages. */
	if (!in_manual_write)
		l_req_proc_count++;

	/* Don't re-log replayed messages */
	if (log_in_replay)
		return (0);

	/* Buffer the message to be written at the time of the next log_flush */
	r_node = (req_nod *) malloc(sizeof(req_nod));
	msg_increfcount(msg);
	r_node->msg = msg;
	r_node->tstamp = ++log_count;
	r_node->entry = entry;
	r_node->prev = NULL;
	r_node->next = l_node->req_q_tl;
	if (r_node->next != NULL)
		r_node->next->prev = r_node;
	l_node->req_q_tl = r_node;
	if (l_node->req_q_hd == NULL)
		l_node->req_q_hd = r_node;
	l_node->req_q_len++;

	/* If this is an automatically flushed, manually logged group, decide if a checkpoint
	   should be taken.  Automatically logged groups must wait for uncompleted logged message
	   to finish their task before a consistent checkpoint can be taken. */
	if ((l_node->flush_type == L_AUTO) && (l_node->replay_entry != 0) &&
	    (((l_node->log_reqs + l_node->req_q_len) > l_node->reqs_thresh) ||
	     (l_node->wri_off > l_node->len_thresh)))
		if (l_take_ckpt(gip) != 0)
			panic("Auto checkpoint failure %d", isis_errno);

	/* If this is an automatically flushed group, decide if the buffer of logged messages
	   should be flushed. */
	if ((l_node->flush_type == L_AUTO) && (l_node->req_q_len > l_node->count_thresh))
		l_flush_reqs(l_node);

	return (0);
}

/* LOG_WRITE:   Used by an application to write a message to the log of
                a manually logged group.

                        gaddr   Address of group where msg should be logged
                        msg     Message to log                                  */
int
log_write(gaddr, msg)
	address *gaddr;
	register message *msg;
{
	register ginfo *gip;
	register log_nod *l_node;

	ISIS_ENTER();
	gip = map_gaddr(gaddr);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		ISIS_RETURN(isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		ISIS_RETURN(isis_errno);
	}
	if (l_node->replay_entry == 0) {
		isis_errno = IE_NOTALLOWED;
		ISIS_RETURN(isis_errno);
	}

	in_manual_write = 1;
	log_write_msg(gip, msg, l_node->replay_entry);
	in_manual_write = 0;

	ISIS_RETURN(0);
}

/* LOGGING_OUT: Receive a block of the checkpoint being currently taken and
                write it out to the temporary log file */
void
logging_out(msg)
	message *msg;
{
	if (msg_fwrite(ckpt_fp, msg))
		panic("Message write failure in logging_out");

	ckpt_hdr.ckpt_blks++;
	ckpt_hdr.ckpt_len += msg_getlen(msg);
}

/* LOG_END_LOG_MSG:     Signal the completion of an automatically logged message.
                        If all automatically logged messages have now been completed,
                        a checkpoint may be taken that is consistent with the set
                        of logged messages. */
int
log_end_log_msg()
{
	register log_nod *l_node;
	register ginfo *gip;

	l_req_proc_count--;	/* Record that a logged message has completed */

	/* Cannot checkpoint consistently if we are in the middle of replaying the logs or if there 
	   are still automatically logged messages that have not yet completed */
	if (log_in_replay)
		return (0);
	if (l_req_proc_count != 0)
		return (0);

	/* Determine which automatically flushed, automatically logged groups can now be
	   checkpointed */
	for (gip = isis_groups; gip; gip = gip->gi_next) {
		if ((l_node = gip->gi_lnode) != NULL)
			if ((l_node->flush_type == L_AUTO) && (l_node->replay_entry == 0) &&
			    (((l_node->log_reqs + l_node->req_q_len) > l_node->reqs_thresh) ||
			     (l_node->wri_off > l_node->len_thresh)))
				if (l_take_ckpt(gip) != 0)
					panic("Auto checkpoint 2 failure %d", isis_errno);
	}

	return (0);
}

/* LOG_FLUSH:   Have all members of the given process group flush the set
                of buffered log messages for that group, possibly checkpointing
                the group state. */
int
log_flush(gaddr)
	address *gaddr;
{
	register int nresps;

	ISIS_ENTER();
	/* Only members of a logged group may invoke this function */
	if (pg_rank(gaddr, &my_address) < 0) {
		isis_errno = IE_NOTMEMB;
		ISIS_RETURN(isis_errno);
	}

	if (log_in_replay)
		ISIS_RETURN(0);	/* Ignore if in replay */

	/* GBCAST the flush request.  Note that replies are needed to be sure that every
	   functioning server stably logged all messages. This is so that we can be sure that all
	   of these messages will be recovered, in the case of failure, regardless of who is last
	   to fail. */
	nresps = gbcast(gaddr, GENERIC_LOG_FLUSH, "%A[1]", gaddr, ALL, "");
	if (nresps < 0)
		ISIS_RETURN(nresps);
	if (nresps == 0) {
		isis_errno = IE_TOTFAIL;
		ISIS_RETURN(isis_errno);
	}

	ISIS_RETURN(0);
}

/* LOG_RECOVERED:       Returns TRUE iff this process recovered (from its log)
                        the specified group. */
int
log_recovered(gaddr)
	address *gaddr;
{
	log_nod *l_node;
	register ginfo *gip;

	ISIS_ENTER();
	gip = map_gaddr(gaddr);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		ISIS_RETURN(isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		ISIS_RETURN(isis_errno);
	}

	ISIS_RETURN(l_node->I_recovered);
}

/* LOG_REPLAY_MSGS:     Replay the set of all logged messages for all logged
                        groups.  Note that all messages will be replayed in the
                        order in which they were logged. */
int
log_replay_msgs()
{
	message *msg;
	int entry;
	address l_addr;

	/* Bind the entry for the end-of-replay message, if it has not already been bound */
	if (!l_replay_entry_set) {
		isis_entry(GENERIC_ENDLOGREPLAY, (vfunc *) l_endlogreplay, "l_endlogreplay");
		l_replay_entry_set++;
	}

	log_in_replay = 1;
	l_addr = my_address;

	/* Replay the logged messages */
	l_replay_reset();
	while (l_next_replay_req(&msg, &entry) == 0) {
		l_addr.addr_entry = entry;
		msg_setdest(msg, &l_addr);
		cl_local_delivery(msg);
		msg_delete(msg);
	}

	/* Start the end-of-replay processing by delivering the end-of-replay message */
	msg = msg_newmsg();
	l_addr.addr_entry = GENERIC_ENDLOGREPLAY;
	msg_setdest(msg, &l_addr);
	cl_local_delivery(msg);
	msg_delete(msg);

	log_in_replay = 0;
	return (0);
}

/* LOG_REPLAY_CKPT:     Replay the checkpoint of the named group by
                        delivering it to the application using the normal
                        state xfer mechanism */
int
log_replay_ckpt(gname)
	char *gname;
{
	register log_nod *l_node;
	register ginfo *gip;
	register int i;
	message *msg;
	int stat;
	address gaddr;

	gip = map_gname(gname);
	if (gip == NULL) {
		isis_errno = IE_UNKNOWN;
		return (isis_errno);
	}
	l_node = gip->gi_lnode;
	if (l_node == NULL) {
		isis_errno = IE_NOTLOGGED;
		return (isis_errno);
	}

	if (l_node->l_hdr.is_ckpt == L_NOCKPT) {
		isis_errno = IE_NOCKPT;
		return (isis_errno);
	}

	/* Reset file pointer to beginning of checkpoint */
	stat = fseek(l_node->l_fp, (long) sizeof(log_hdr), 0);
	if (stat == -1)
		panic("log_replay_ckpt fseek error %d", errno);

	/* Read each checkpoint block and deliver its contents */
	for (i = 0; i < l_node->l_hdr.ckpt_blks; i++) {
		msg = msg_fread(l_node->l_fp);
		if (msg == NULL)
			panic("Message read error in log_replay_ckpt");
		msg_get(msg, "%a", &gaddr);
		if (xfer_rcv_unpack(gip, msg) < 0)
			panic("Checkpoint receive failure %d", isis_errno);
		msg_delete(msg);
	}

	return (0);
}

/* L_OPEN_LOG:  Open/Create a log file with the given name.  Place its
                FILE pointer and log header in the specified locations.

                        l_name  Name of log file
                        l_fp    Addr of location to hold log FILE ptr
                        l_hdr   Addr of location to hold log header     */
static int
l_open_log(l_name, l_fp, l_hdr, ignore_old)
	char *l_name;
	register FILE **l_fp;
	register log_hdr *l_hdr;
	int ignore_old;
{
	register int cc, stat;

	/* Try opening, but not creating, log file */
	*l_fp = fopen(l_name, "r+");
	chmod(l_name, 0666);

	/* If log does not exist, try creating it */
	if (*l_fp == NULL) {
		if (errno != ENOENT)
			return (IE_LOGIO);
		else
			return (l_create_log(l_name, l_fp, l_hdr));
	}

	/* Otherwise, read in the log header */
	else {
		fseek(*l_fp, 0L, 0);
		cc = fread(l_hdr, 1, sizeof(log_hdr), *l_fp);
		if (cc < 0) {
			fclose(*l_fp);
			stat = unlink(l_name);
			if (stat == -1)
				panic("l_open_log unlink error %d", errno);
			return (IE_LOGIO);
		}
		if (cc < sizeof(log_hdr)) {
			fclose(*l_fp);
			stat = unlink(l_name);
			if (stat == -1)
				panic("l_open_log unlink error %d", errno);
			panic("l_open_log corrupted log file");
		}
		if (strcmp(l_hdr->version, L_VERSION) != 0) {
			if (ignore_old == L_COLD) {
				fclose(*l_fp);
				return (l_create_log(l_name, l_fp, l_hdr));
			} else {
				fclose(*l_fp);
				return (IE_OLDLOG);
			}
		}
	}

	return (0);
}

/* L_CREATE_LOG:        Create a log file with the given name.  Place its
                        FILE pointer and header in the given locations.

                                l_name  Name of log file
                                l_fp    Addr of location to hold log FILE ptr
                                l_hdr   Addr of location to hold log header     */
static int
l_create_log(l_name, l_fp, l_hdr)
	char *l_name;
	register FILE **l_fp;
	register log_hdr *l_hdr;
{
	register int cc;
	char *t_name;
	int t_fd, stat, i;

	/* Create a temporary file to hold the log while we are setting it up. A temporary file is
	   used so that no real log exists in a corrupted form if we fail in the middle of the
	   setup */
	t_name = malloc(strlen(isis_dir) + sizeof(LOG_SUBDIR) + sizeof(LOG_TTMPLT) + 3);
	strcpy(t_name, isis_dir);
	strcat(t_name, "/");
	strcat(t_name, LOG_SUBDIR);
	strcat(t_name, "/");
	strcat(t_name, LOG_TTMPLT);
	mktemp(t_name);
	t_fd = open(t_name, O_CREAT | O_RDWR | O_EXCL, 0666);
	chmod(t_name, 0666);
	if (t_fd < 0) {
		panic("Error creating temporary log file %s", t_name);
	}
	close(t_fd);

	/* Open the temporary file as a stream */
	*l_fp = fopen(t_name, "r+");
	chmod(t_name, 0666);
	if (*l_fp == NULL) {
		unlink(t_name);
		free(t_name);
		return (IE_LOGIO);
	}

	/* Initialize and write the header of the new log */
	l_hdr->is_ckpt = L_NOCKPT;
	l_hdr->ckpt_blks = 0;
	l_hdr->ckpt_len = 0;
	for (i = 0; i <= L_VERLEN; i++)
		l_hdr->version[i] = L_VERSION[i];
	fseek(*l_fp, 0L, 0);
	cc = fwrite(l_hdr, 1, sizeof(log_hdr), *l_fp);
	fflush(*l_fp);
	fclose(*l_fp);
	if (cc < sizeof(log_hdr)) {
		unlink(t_name);
		free(t_name);
		return (IE_LOGIO);
	}

	/* Rename the temporary file to the correct log name */
	if (rename(t_name, l_name)) {
		unlink(t_name);
		free(t_name);
		return (IE_LOGIO);
	}
	free(t_name);

	/* Open the new log file */
	*l_fp = fopen(l_name, "r+");
	chmod(l_name, 0666);

	if (*l_fp == NULL) {
		fclose(*l_fp);
		stat = unlink(l_name);
		if (stat == -1)
			panic("l_create_log unlink error %d", errno);
		return (IE_LOGIO);
	}

	return (0);
}

/* L_LOOKUP_LOG_REQS:   Lookup information in the log for the specified log info
                        structure.  This function determines the end of the last
                        completed message entry, or the end of the last completed
                        log_flush block if the group is manually flushed.  It
                        also determines the last timestamp used and number of
                        messages logged. */
static void
l_lookup_log_reqs(l_node)
	register log_nod *l_node;
{
	message *msg;
	int off, cc, reqs, ent_no, blk_reqs, blk_size;
	long tstamp, last_tstamp;

	reqs = 0;		/* Number of logged messages read */
	blk_reqs = 0;		/* Message number where current log_flush block ends, if manually
				   flushed group */
	last_tstamp = 0L;	/* Last timestamp read */

	off = sizeof(log_hdr) + l_node->l_hdr.ckpt_len;
	fseek(l_node->l_fp, (long) off, 0);

	/* Location of end of last message read or end of last log_flush block if group is manually 
	   flushed. */
	l_node->wri_off = off;

	l_node->last_ts = 0;	/* Timestamp of last read message or last msg in log_flush block */
	l_node->log_reqs = 0;	/* Number of messages read or messages in complete log_flush blocks 
				   read */

	/* Read through the end of the log messages */
	while (1) {

		/* If we are at the start of a log_flush block then update the appropriate info
		   concerning log_flush blocks */
		if ((l_node->flush_type == L_MANUAL) && (reqs == blk_reqs)) {
			cc = fread(&blk_size, 1, sizeof(blk_size), l_node->l_fp);
			if (cc < 0)
				panic("Block size read failure");
			if (cc < sizeof(blk_size))
				break;
			off += sizeof(blk_size);
			blk_reqs += blk_size;
		}

		/* Read the timestamp, entry point, and message contents of the next logged message 
		 */
		cc = fread(&tstamp, 1, sizeof(tstamp), l_node->l_fp);
		if (cc < 0)
			panic("Read failure on tstamp in lookup_log_reqs");
		if (cc < sizeof(tstamp))
			break;
		cc = fread(&ent_no, 1, sizeof(ent_no), l_node->l_fp);
		if (cc < 0)
			panic("Read failure on ent_no in lookup_log_reqs");
		if (cc < sizeof(ent_no))
			break;
		msg = msg_fread(l_node->l_fp);
		if (msg == NULL)
			break;

		/* Update the appropriate info for the newly read message */
		reqs++;
		off += sizeof(tstamp) + sizeof(ent_no) + msg_getlen(msg);
		last_tstamp = tstamp;

		/* Update the log_flush block info if we just completed reading an entire log_flush 
		   block */
		if ((l_node->flush_type == L_MANUAL) && (reqs == blk_reqs)) {
			l_node->wri_off = off;
			l_node->last_ts = last_tstamp;
			l_node->log_reqs = reqs;
		}

		msg_delete(msg);
	}

	/* Record the final info determined.  Note that info for manually flushed groups is
	   recorded above */
	if (l_node->flush_type == L_AUTO) {
		l_node->log_reqs = reqs;
		l_node->wri_off = off;
		l_node->last_ts = last_tstamp;
	}
	if (last_tstamp > log_count)
		log_count = last_tstamp;

	/* Truncate the log file at the last complete message or last complete log_flush block */
	fseek(l_node->l_fp, (long) l_node->wri_off, 0);
	fflush(l_node->l_fp);
	if (ftruncate(fileno(l_node->l_fp), (off_t) l_node->wri_off) != 0)
		panic("File truncation error %d", errno);
}

/* L_TAKE_CKPT: Checkpoint the specified group given group info pointer */

static int
l_take_ckpt(gip)
	ginfo *gip;
{
	register log_nod *l_node;
	register req_nod *r_node;
	int cc, i;

	l_node = gip->gi_lnode;
	if (l_node == NULL)
		panic("Can't checkpoint non-logged group");

	if (l_node->inhibited)
		return (0);

	/* Create a temporary log file while taking checkpoint.  A temporary file is used so that a 
	   failure during checkpointing will not corrupt the real log file. */
	ckpt_tname = malloc(strlen(isis_dir) + sizeof(LOG_SUBDIR)
			    + sizeof(LOG_TTMPLT) + 3);
	strcpy(ckpt_tname, isis_dir);
	strcat(ckpt_tname, "/");
	strcat(ckpt_tname, LOG_SUBDIR);
	strcat(ckpt_tname, "/");
	strcat(ckpt_tname, LOG_TTMPLT);
	mktemp(ckpt_tname);
	ckpt_fd = open(ckpt_tname, O_CREAT | O_RDWR | O_EXCL, 0666);
	if (ckpt_fd < 0)
		panic("Can't create temporary file for checkpoint");
	close(ckpt_fd);

	/* Open the temporary log file as a stream */
	ckpt_fp = fopen(ckpt_tname, "r+");
	chmod(ckpt_tname, 0666);
	if (ckpt_fp == NULL)
		panic("Can't re-open temp file to take checkpoint");

	/* Initialize and write out the temporary log file header */
	ckpt_hdr.is_ckpt = L_CKPT;
	ckpt_hdr.ckpt_blks = 0;
	ckpt_hdr.ckpt_len = 0;
	for (i = 0; i <= L_VERLEN; i++)
		ckpt_hdr.version[i] = L_VERSION[i];
	fseek(ckpt_fp, 0L, 0);
	cc = fwrite(&ckpt_hdr, 1, sizeof(log_hdr), ckpt_fp);
	if (cc < sizeof(log_hdr))
		panic("Ckpt l_hdr write returned %d", cc);

	/* Take the checkpoint, writing it to the temporary log */
	xfer_to_checkpoint(gip->gi_gaddr);

	/* Write the new log header containing the info on the new checkpoint */
	fseek(ckpt_fp, 0L, 0);
	cc = fwrite(&ckpt_hdr, 1, sizeof(log_hdr), ckpt_fp);
	if (cc < sizeof(log_hdr))
		panic("Ckpt 2 l_hdr write returned %d", cc);
	fclose(ckpt_fp);

	/* Rename the temporary log file as the real log file and open it */
	fclose(l_node->l_fp);
	cc = rename(ckpt_tname, l_node->l_name);
	if (cc != 0)
		panic("Take checkpoint rename failure");
	l_node->l_fp = fopen(l_node->l_name, "r+");
	chmod(l_node->l_name, 0666);
	if (l_node->l_fp == NULL)
		panic("Re-open failure in l_take_ckpt");
	fseek(l_node->l_fp, (long) (sizeof(log_hdr) + ckpt_hdr.ckpt_len), 0);

	/* Update the log info structure to reflect the new checkpoint */
	l_node->wri_off = sizeof(log_hdr) + ckpt_hdr.ckpt_len;
	l_node->last_ts = 0L;
	l_node->log_reqs = 0;
	l_node->req_q_len = 0;
	l_node->l_hdr = ckpt_hdr;

	/* Delete any buffered messages because their effects should be reflected in the checkpoint 
	 */
	r_node = l_node->req_q_tl;
	while (r_node != NULL) {
		msg_delete(r_node->msg);
		r_node = r_node->next;
		free(l_node->req_q_tl);
		l_node->req_q_tl = r_node;
	}
	l_node->req_q_hd = NULL;
	l_node->last_flush = time((time_t *) 0);

	free(ckpt_tname);
	return (0);
}

/* L_FLUSH_REQS:        Flush buffered messages for the specified log to
                        stable storage. */
static int
l_flush_reqs(l_node)
	register log_nod *l_node;
{
	register req_nod *r_node;
	register message *wmsg;
	int cc;
	long tstamp;
	int entry;

	if (l_node->req_q_len == 0)
		return (0);

	fseek(l_node->l_fp, (long) (l_node->wri_off), 0);

	/* If this group is manually flushed then start a log_flush block. Write down the number of 
	   entries in this block so that on recovery we can determine if it was written without
	   failure.  Note, incomplete log_flush blocks are discarded */
	if (l_node->flush_type == L_MANUAL) {
		cc = fwrite(&l_node->req_q_len, 1, sizeof(l_node->req_q_len), l_node->l_fp);
		if (cc < sizeof(l_node->req_q_len))
			panic("Flush count write error %d", errno);
		l_node->wri_off += sizeof(l_node->req_q_len);
	}

	/* Write all buffered messages, along with their timestamps and destination entry points,
	   to the log file */
	while ((r_node = l_node->req_q_hd) != NULL) {
		wmsg = r_node->msg;
		tstamp = r_node->tstamp;
		entry = r_node->entry;
		l_node->req_q_hd = r_node->prev;
		if (l_node->req_q_hd == NULL)
			l_node->req_q_tl = NULL;
		else
			l_node->req_q_hd->next = NULL;
		free(r_node);
		l_node->log_reqs++;
		l_node->req_q_len--;
		l_node->wri_off += sizeof(tstamp) + sizeof(entry) + msg_getlen(wmsg);
		l_node->last_ts = tstamp;
		cc = fwrite(&tstamp, 1, sizeof(tstamp), l_node->l_fp);
		if (cc < sizeof(tstamp))
			panic("Log request timestamp write failure");
		cc = fwrite(&entry, 1, sizeof(entry), l_node->l_fp);
		if (cc < sizeof(entry))
			panic("Log entry point write failure");
		if (msg_fwrite(l_node->l_fp, wmsg))
			panic("Log message write error %d", errno);
		msg_delete(wmsg);
	}
	fflush(l_node->l_fp);
	l_node->last_flush = time((time_t *) 0);

	return (0);
}

/* L_REC_FLUSH: Receive a flush request.  Flush all buffered messages for the
                appropriate log to the log file.  Also, decide if a checkpoint
                should be taken. */
static int
l_rec_flush(msg)
	register message *msg;
{
	register log_nod *l_node;
	register ginfo *gip;
	address gaddr;

	/* Get the group and log to flush */
	if (msg_get(msg, "%a", &gaddr) < 0)
		panic("Bad flush message received");
	gip = map_gaddr(&gaddr);
	if (gip == NULL)
		return (0);
	l_node = gip->gi_lnode;
	if (l_node == NULL)
		return (0);

	/* If this group is automatically logged and the tasks for all automatically logged
	   messages have completed then a consistent checkpoint can be taken.  If so, decide if we
	   want to take a checkpoint now. */
	if ((l_node->replay_entry == 0) && (l_req_proc_count == 0) &&
	    (((l_node->log_reqs + l_node->req_q_len) > l_node->reqs_thresh) ||
	     (l_node->wri_off > l_node->len_thresh)))
		if (l_take_ckpt(gip) != 0)
			panic("Flush checkpoint failure %d", isis_errno);

	/* If this group is manually logged then decide if we wish to checkpoint it now. */
	if ((l_node->replay_entry != 0) &&
	    (((l_node->log_reqs + l_node->req_q_len) > l_node->reqs_thresh) ||
	     (l_node->wri_off > l_node->len_thresh)))
		if (l_take_ckpt(gip) != 0)
			panic("Flush checkpoint 2 failure %d", isis_errno);

	/* Flush all buffered messages for this log to the log file */
	l_flush_reqs(l_node);

	/* Signal the log_flush completion at this process */
	reply(msg, "");

	return (0);
}

/* L_REPLAY_RESET:      Set up the logs and log info structures to begin
                        replaying logged messages. */
static void
l_replay_reset()
{
	register log_nod *l_node;
	register ginfo *gip;
	register int cc;

	/* Setup each logged group for message replay */
	for (gip = isis_groups; gip; gip = gip->gi_next) {
		if ((l_node = gip->gi_lnode) != NULL)

			/* If there are no logged messages for this group, mark the group log info
			   structure as done replaying */
			if (l_node->log_reqs == 0)
				l_node->next_req = END_REPLAY;

		/* Otherwise, read the info on the first logged message */
			else {
				fseek(l_node->l_fp, (long) (sizeof(log_hdr) +
							    l_node->l_hdr.ckpt_len), 0);
				l_node->next_req = 1;

				/* If this group is manually flushed, read the number of entries in 
				   the first log_flush block */
				if (l_node->flush_type == L_MANUAL) {
					cc = fread(&l_node->end_blk, 1, sizeof(l_node->end_blk),
						   l_node->l_fp);
					if (cc < sizeof(l_node->end_blk))
						panic("Block size read error in l_replay_reset");
				}

				/* Record the timestamp of the first logged message */
				cc = fread(&l_node->next_ts, 1, sizeof(l_node->next_ts),
					   l_node->l_fp);
				if (cc < sizeof(l_node->next_ts))
					panic("Read failure on tstamp in l_replay_reset");
			}
	}
}

/* L_NEXT_REPLAY_REQ:   Determine the next message to be replayed in the
                        replay sequence.  Put the information about this
                        message in the specified locations.

                                msg     Addr to contain ptr to next replay msg
                                entry   Addr to contain entry pt of msg */
static int
l_next_replay_req(msg, entry)
	message **msg;
	int *entry;
{
	register log_nod *l_node, *replay_log;
	register ginfo *gip;
	register int cc;
	int blk_size;
	long replay_ts;

	/* Determine the next message in the replay sequence by searching through all logged groups 
	   to find the message with the next lowest timestamp */
	replay_ts = -1L;
	for (gip = isis_groups; gip; gip = gip->gi_next) {
		if ((l_node = gip->gi_lnode) != NULL)
			if ((l_node->next_req != END_REPLAY) &&
			    ((l_node->next_ts < replay_ts) || (replay_ts == -1L))) {
				replay_ts = l_node->next_ts;
				replay_log = l_node;
			}
	}

	if (replay_ts == -1L)
		return (-1);	/* All groups completed replay */

	/* Read the message entry point and message contents of the next message in the replay
	   sequence. */
	cc = fread(entry, 1, sizeof(*entry), replay_log->l_fp);
	if (cc < sizeof(*entry))
		panic("Entry number read error in l_next_replay_req");
	*msg = msg_fread(replay_log->l_fp);
	if (*msg == NULL)
		panic("Message read error in l_next_replay_req");

	/* Update the log info structure to reflect the message about to be replayed */

	replay_log->next_req++;

	/* If this is the last message in the group log, then mark the group log as having
	   completed replaying. */
	if (replay_log->next_req > replay_log->log_reqs)
		replay_log->next_req = END_REPLAY;

	/* Otherwise, read in the info on the next message to replay from this group. */
	else {			/* If we are starting a new log_flush block then read in the new
				   log_flush block info */
		if ((replay_log->next_req > replay_log->end_blk) &&
		    (replay_log->flush_type == L_MANUAL)) {
			cc = fread(&blk_size, 1, sizeof(blk_size), replay_log->l_fp);
			if (cc < sizeof(blk_size))
				panic("Block size 2 read error in l_next_replay_req");
			replay_log->end_blk += blk_size;
		}

		/* Read in the timestamp of the next message to replay from this log */
		cc = fread(&replay_log->next_ts, 1, sizeof(replay_log->next_ts), replay_log->l_fp);
		if (cc < sizeof(replay_log->next_ts))
			panic("Tstamp 2 read error in l_next_replay_req");
	}

	return (0);
}

/* L_ENDLOGREPLAY:      Process the end-of-replay of logged messages. */

static void
l_endlogreplay(msg)
	message *msg;
{
	register log_nod *l_node;
	register ginfo *gip;

	/* Invoke any end-of-replay processing functions specified by the application */
	for (gip = isis_groups; gip; gip = gip->gi_next)
		if ((l_node = gip->gi_lnode) != NULL)
			if ((l_node->I_recovered != 0) && (l_node->end_replay != NULL))
				ISISCALL0(l_node->end_replay);
}

/* L_TIMER:		Log flushing timer task.  This function periodically
			goes through the list of logged process groups to
			determine if the timer for flushing any group has
			expired.  If it has, the message buffer for the group
			is flushed.  Once all group flushes have completed,
			the function sleeps for an appropriate amount of
			time until the next flush timer will expire.  */

static void
l_timer(arg)
	int arg;
{
	int is_active_timer, flush_delay;
	unsigned sleep_time;
	time_t current;
	register log_nod *l_node;
	register ginfo *gip;

	while (1) {
		is_active_timer = 0;
		sleep_time = 0;
		current = time((time_t *) 0);

		/* Loop through the list of logged groups and determine which ones should be
		   flushed due to a timer expiration. */

		for (gip = isis_groups; gip; gip = gip->gi_next) {
			if ((l_node = gip->gi_lnode) != NULL) {
				if ((l_node->timer != 0) && (l_node->flush_type == L_AUTO)) {
					is_active_timer = 1;
					flush_delay =
					    (l_node->last_flush + l_node->timer) - current;

					/* Flush this group's message buffer if its timer has
					   expired. */

					if (flush_delay <= 0) {
						if (((l_node->log_reqs + l_node->req_q_len) >
						     l_node->reqs_thresh)
						    || (l_node->wri_off > l_node->len_thresh)) {

							/* Only checkpoint if this is a manually
							   logged group or if its an automatically
							   logged group where all logged messages
							   have been processed. */

							if ((l_node->replay_entry != 0)
							    || (l_req_proc_count == 0)) {
								if (l_take_ckpt(gip) != 0)
									panic
									    ("Timer auto checkpoint failure %d",
									     isis_errno);
							}
						}

						if (l_flush_reqs(l_node) != 0)
							panic("Timer auto flush failure %d",
							      isis_errno);
						flush_delay = l_node->timer;
					}

					if ((sleep_time == 0) || (flush_delay <= sleep_time))
						sleep_time = flush_delay;
				}
			}
		}

		/* If no log flushing timers are active then wait to be signalled when one is
		   created.  Otherwise, if there are active timers, sleep until the next one is
		   supposed to go off. */

		if (!is_active_timer)
			t_wait_l(&l_newtimer, "No logged groups currently use flush timers");
		else {
			if (sleep_time <= 0)
				sleep_time = 1;
			sleep(sleep_time);
		}
	}

}
