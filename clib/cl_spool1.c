/*  $RCSfile: cl_spool1.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:27 $  */
/*
 *      Originally coded by Ken Birman
 *      spooler callback routine
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
#include "spooler.h"

static  void sp_rcv_replay();
qnode   *spool_waitq;

void
sp_init()
  {
        isis_entry(GENERIC_SP_REPLAY, (vfunc *) sp_rcv_replay,
                   "isis-spooler: receive spool replay");
        spool_waitq = qu_null();
  }

int	spool_seqn = -1;

static void
sp_rcv_replay(msg)
  register message *msg;
  {
	register address *ap;
	int entry, seqn;
	message *sp_msg;
	if(msg_get(msg, "%m,%d,%d", &sp_msg, &entry, &seqn) != 3)
	    panic("sp_rcv_replay");
	if(spool_seqn >= seqn)
	{
	    msg_delete(sp_msg);
	    return;
	}
	spool_seqn = seqn;
	ap = msg_setdests(sp_msg, msg_getdests(msg));
	while(!aptr_isnull(ap))
	{
	    if(ap->addr_entry == GENERIC_SP_REPLAY)
		ap->addr_entry = entry;
	    ++ap;
	}
	cl_local_delivery(sp_msg);
	msg_delete(sp_msg);
  }
