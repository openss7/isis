/*  $RCSfile: pr_dlist.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:09 $  */
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

/*
 * pr_splitdests:  Split the destination list of a given message into
 *   a list of local process names and a list of external site-id's,
 *   without duplicates on either list.
 */
pr_splitdests (msg, localprocs, ext_dests)
  message   *msg;
  int       *localprocs;
  site_id   *ext_dests;
  {
        register address *dests;
        register int i, j, n_procs, n_dests;
        register site_id dest_id;

        dests = msg_getdests(msg);
        for(i = n_procs = n_dests = 0; dests[i].addr_site != 0; i++)
        {
            if(dests[i].addr_site == my_site_no && dests[i].addr_incarn == my_site_incarn)
            {
                for(j = 0; j < n_procs && localprocs[j] != dests[i].addr_process; j++)
                    continue;
                if(j == n_procs)
                   localprocs[n_procs++] = dests[i].addr_process;
            }
            else
            {
                dest_id = MAKE_SITE_ID(dests[i].addr_site, dests[i].addr_incarn);
                for(j = 0; j < n_dests && ext_dests[j] != dest_id; j++);
                if(j == n_dests)
                   ext_dests[n_dests++] = dest_id;
            }
        }
        localprocs[n_procs] = 0;
        ext_dests[n_dests] = 0;
  }

/*
 * Convert dlist into a non-local slist terminated by 0
 * and return number of local entries
 */
dltosl(ap, pp, sp)
  register address *ap;
  register int *pp;
  register site_id *sp;
  {
        site_id *slist = sp;
        register site_id *sl = sp;
        register lcount = 0;

        *pp = 0;
        while(ap && !aptr_isnull(ap))
        {
            if(ap->addr_site == my_site_no)
            {
                if(ap->addr_incarn == my_site_incarn || ap->addr_incarn == RECOVERY_INCARN)
                {
                    register int *xp;
                    ++lcount;
                    for(xp = pp; *xp; xp++)
                        if(*xp == ap->addr_process)
                            break;
                    if(*xp == 0)
                    {
                        *xp++ = ap->addr_process;
                        *xp = 0;
                    }
                }
            }
            else
            {
                *sp = MAKE_SITE_ID(ap->addr_site, ap->addr_incarn);
                if(*sl == *sp && sl != sp)
                    continue;
                sl = slist;
                while(*sl != *sp)
                    ++sl;
                if(sl == sp)
                    ++sp;
            }
            ++ap;
        }
        *sp = (site_id)0;
        return(lcount);
  }
