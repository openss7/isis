/*  $RCSfile: lmgr_dummy.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:34 $  */
/*
 *	Dummy lmgr routines
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

log_nod
*log_init(fname, gname, replay_entno, flush_type, end_of_replay, cs, ignore)
  register char *fname, *gname;
  int replay_entno, flush_type;
  int (*end_of_replay)();
  {
	isis_errno = IE_LOGIO;
	return(NULL);
  }

int
log_action(gname, new_incarn_p)
  char *gname;
  int  *new_incarn_p;
  {
        return(-1);
  }

int
log_start_recovery(gname)
  char *gname;
  {
        return(-1);
  }

int
log_remove(gname)
  char *gname;
  {
        return(-1);
  }

int
log_has_ckpt(gname)
  char *gname;
  {
        return(0);
  }

int
log_checkpoint(gname)
  char *gname;
  {
        return(-1);
  }

int
log_write_msg(gip, msg, entry)
  register ginfo *gip;
  message *msg;
  int entry;
  {
        return(-1);
  }

int
log_write(gaddr, msg)
  address *gaddr;
  register message *msg;
  {
        return(-1);
  }

void
logging_out(msg)
  message *msg;
  {
  }

int
log_end_log_msg()
  {
        return(0);
  }

int
log_flush(gaddr)
  address *gaddr;
  {
        return(0);
  }

int
log_recovered(gaddr)
  address *gaddr;
  {
        return(-1);
  }

int
log_replay_msgs()
  {
        return(0);
  }

int
log_replay_ckpt(gname)
  char *gname;
  {
        return(0);
  }

log_cntthresh(gname, nmbuf)
  char *gname;
  {
  }

log_reqsthresh(gname, nmdsk)
  char *gname;
  {
  }

log_lenthresh(gname, lflen)
  char *gname;
  {
  }

log_timer(gname, timer)
  char *gname;
  unsigned timer;
  {
  }
