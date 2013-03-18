/*  $RCSfile: pr_glocks.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:32 $  */
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
 */
#ifndef PR_GLOCKS
#define PR_GLOCKS

#include "pr.h"

qnode  *glocks;

struct glock
{
    qnode       *shrlock;
    int         exlock;
    qnode       *want_shrlock;
    qnode       *want_exlock;
};

extern  adesc   glock_adesc;
glock   *glock_find();

#endif  PR_GLOCKS

