/*  $RCSfile: pr_viewlocks.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:20 $  */
/*
 *      Originally coded by Tommy Joseph
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
 *
 */
#include "pr.h"

acquire_view_r_lock (sites)
  bitvec    sites;
{
    register int    i;

    while (bitv (&sview_wlocks, &sites) || bitv (&sview_want_wlocks, &sites))
        (void) t_wait (&wait_r_lock, "wait_rlock");
    for (i = 1; i < MAX_SITES; i++)
        if (bit (&sites, i))
        {
            sview_nlocks[i]++;
            bis (&sview_rlocks, i);
        }
}
            

release_view_r_lock (sites)
  bitvec    sites;
{
    register int    i;

    for (i = 1; i < MAX_SITES; i++)
        if (bit (&sites, i) && --sview_nlocks[i] == 0)
            bic (&sview_rlocks, i);
    if (btst (&sview_want_wlocks) && !bitv (&sview_rlocks, &sview_want_wlocks))
    {
        sview_wlocks = sview_want_wlocks;
        bclr (&sview_want_wlocks);
        t_sig (&wait_w_lock, 0);
    }
}



fd_lockview (sites)
  bitvec    sites;
{
    if (btst (&sview_wlocks) || btst (&sview_want_wlocks))
        print ("fd_lockview: WARNING: view is already locked\n");
    if (bitv (&sview_rlocks, &sites))
    {
        bisv (&sview_want_wlocks, &sites);
        (void) t_wait (&wait_w_lock, "wait_w_lock");
    }
    else
        bisv (&sview_wlocks, &sites);
}



fd_unlockview ()
{
    bclr (&sview_wlocks);
    while (wait_r_lock)
        t_sig (&wait_r_lock, 0);
}

