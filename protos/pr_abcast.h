/*  $RCSfile: pr_abcast.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:47 $  */
/*
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
#ifndef PR_ABCAST
#define PR_ABCAST

#include "pr.h"

qnode   *abq, *ablocalq;

struct abq_item
{
    int     msg_id;
    int     priority;
    char    tag;
    message *msg;
};

extern  adesc   abq_adesc;
#define abq_alloc()   ((abq_item *) mallocate (&abq_adesc))
int     abq_free();

int         pr_abcast(), ab_send2(), ab_recv1(), ab_recv2(), ab_takeover(),
            ab_deliver(), ab_init(); 
int         ab_send1(), ab_makedlist(), ab_makeplist(), ab_addtoqueues(),
            abq_changeprops();
abq_item    *abq_add(), *abq_find(); 
message     *ab_createreply();

#endif
