/*  $RCSfile: pr_watch.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:21 $  */
/*
 *      Originally coded by Ken Birman
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

static  WID;

qu_dofree(qp)
  register qnode *qp;
  {
        qu_free(qp);
  }

watch_on (site, incarn, routine, arg)
  int   site, incarn, (*routine)();
  char  *arg;
{
    register qnode *qp;

    if (!SITE_IS_UP (site, incarn))
        return (0);
    if (watch_queue[site] == 0)
        (watch_queue[site] = qu_null())->qu_name = incarn;
    qp = qu_add (watch_queue[site], (++WID ? WID : ++WID), 0, wmagic);
    qu_add(watching, WID, (char*)qp, qu_dofree);
    qp->qu_args[0] = (char*)routine;
    qp->qu_args[1] = arg;
    return (WID);
}



watch_off(wid)
{
    register qnode *wp, *qp;

    if ((wp = qu_find(watching, wid)) == (qnode*)0)
        return;
    qp = wp->qu_queue;
    qp->qu_freeroutine = nullroutine;
    qu_free (wp);
}



watch_newview()
{
    register qnode  *wq, *qp;
    register        s;

    for (s = 0; s < MAX_SITES; s++)
    {
        if((wq = watch_queue[s]) == 0)
            continue;
        if (bit (&current_view.sv_failed, s) || current_view.sv_incarn[s] != wq->qu_name)
        {
            while (qp = qu_head(wq))
            {
                register (*routine)();
                register char *arg;

                routine = (ifunc*)qp->qu_args[0];
                arg = qp->qu_args[1];
                watch_off(qp->qu_name);
                t_fork (routine, arg, (message *) 0);
            }
            qu_free (wq);
            watch_queue[s] = 0;
        }
    }
}



wmagic ()
{
    panic ("wmagic");
}
