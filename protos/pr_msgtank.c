/*  $RCSfile: pr_msgtank.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:59 $  */
/*
 *	Originally coded by Robert Cooper
 *
 * A message tank is a bounded qnode of pending messages. The bound is
 *  on the total bytes of message storage. When that bound is exceeded,
 *  the enqueue operation simply discards the new message. We try to be
 *  "fair" (or at least consistent) by not favouring small messages. Thus
 *  we accept messages until the bound is actually exceeded, and then discard all
 *  subsequent messages until the bound is satisfied again. The alternative, 
 *  of accepting a message so long as doing so would not exceed the bound, favours 
 *  small messages when we are near the bound. This is bad news since, 
 *  acknowledgements and keep-alives are small and would succeed while large
 *  data messages would be consistently lost. Such behaviour would make flow control
 *  and failure detection difficult.
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
#define tankdebug 1

msg_tank *msg_tank_create(max_size)
  register long max_size;
  {
    static adesc ad = {sizeof(msg_tank), sizeof (msg_tank), 1};
                                 /* Describes the memory being allocated to msg_tank
                                    objects (used by mallocate and mdeallocate). */
    register msg_tank *tank = (msg_tank *) mallocate(&ad);
    tank-> head = qu_null();
    tank-> max_size = (max_size == 0 ? default_max_size : max_size);
    return(tank);
  }

msg_tank_set_max(tank, max_size)
  register msg_tank *tank;
  register long max_size;
  {
     tank-> max_size = (max_size == 0 ? default_max_size : max_size);
  }

msg_tank_enqueue(tank, msg)
  register msg_tank *tank;
  register message *msg;
  {
    register intersite *is;
    register s;
    if(is = (intersite*)msg_getfield(msg, INTERSITE_HDR, 1, (int*)0))
    {
        ++tankcount[s = SITE_NO(is->is_from)];
        peek_ack(is);
        qu_add_mp(tank-> head, s, msg, nullroutine);
        if (tank-> size > tank-> max_size)
            panic("pr_msgtank: Tank overflow on message %d bytes long.\n", msg_getlen(msg));
        tank-> size += msg_getlen(msg);
    }
    else
        msg_delete(msg);
  }

/* Returns null (zero) message pointer if tank is empty. */
message *msg_tank_dequeue(tank)
  register msg_tank *tank;
  {
    register qnode *q;
    register message *msg;
    if (q = qu_head(tank->head)) {
        msg = q-> qu_msg;
        --tankcount[q->qu_name];
        qu_free(q);
        tank-> size -= msg_getlen(msg);
        return(msg);
    }
    else
        return((message *)0);
  }

msg_tank_dump(tank)
  register msg_tank *tank;
  {
    qnode *p;
    int n = 0;

    for (p = tank-> head-> qu_next; p != tank-> head; p = p-> qu_next)
        n++;
    print("  Message tank: %d messages, %d bytes\n", n, tank-> size);
  }

