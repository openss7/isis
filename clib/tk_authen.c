/*  $RCSfile: tk_authen.c,v $ $Revision: 2.4 $ $Date: 90/06/11 10:14:23 $  */
/*
 *	Originally coded by Ken Birman
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
 *
 */
#include "isis.h"

static  qnode *au_queue;
static  ifunc *authenticator;
static  vfunc *old_filter;

void
au_request_verify(routine)
  ifunc *routine;
  {
        void au_filter();
        authenticator = routine;
        old_filter = isis_setfilter(au_filter);
  }

void
au_filter(mp)
  register message *mp;
  {
        switch(_ISISCALL1(authenticator, mp))
        {
          case -1:
            nullreply(mp);
            return;

          case 0:
            ISISCALL1(old_filter, mp);
            return;

          default:
            return;
        }
  }

void
au_permit(who)
  address *who;
  {
        if(au_queue == NULLQP)
            au_queue = qu_null();
        if(pg_find(au_queue, who) == NULLQP)
            pg_add(au_queue, who, NULLARG, NULLROUTINE);
  }

void
au_revoke_perm(who)
  address *who;
  {
        register qnode *qp;
        if(au_queue && (qp = pg_find(au_queue, who)))
            qu_free(qp);
  }
