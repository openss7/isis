/*  $RCSfile: pr_fdect.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:16 $  */
/*
 *	Originally coded by Ken Birman 1986
 *      Extensively recoded by Tommy Joseph 1987
 *      Some bug fixes by Ken Birman 1989
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
#include "pr_cbcast.h"

sview       current_view;
int         fd_coordinator, exists_proposal, exists_old_proposal,
            sent_old_proposal;
int         proposed_viewid, old_proposed_viewid;
site_id     proposed_slist[MAX_SITES + 1], old_proposed_slist[MAX_SITES + 1];
bitvec   proposed_failed, proposed_recovered,
            old_proposed_failed, old_proposed_recovered;
qnode       *pending_failures, *pending_recoveries, *replies_wanted;
char        fd_forked, participant_failed;
condition   got_all_replies;
int         lastviewid;
bitvec   sview_wlocks, sview_rlocks, sview_want_wlocks;
condition   wait_r_lock, wait_w_lock;

int isis_state;
int isis_errno;


condition    want_view;
site_id      recov_coordinator;
extern       t_sig();

#define      SI(who)            SITE_NO(who),SITE_INCARN(who)

fd_restart (recovery_type, coordinator)
  char      recovery_type;
  site_id   coordinator;

{
    address         dest;
    message         *fd_msg;
    register int    i;

#   ifdef FD_DEBUG
        print ("fd_restart: %s recovery, coordinator %d/%d\n",
             (recovery_type == FD_PARTIAL ? "partial" : "total"), SI(coordinator));
#   endif FD_DEBUG

    current_view.sv_viewid = RECOV_VIEWID;
    bzero ((char *) current_view.sv_slist, sizeof (current_view.sv_slist));
    for (i = 0; i < (sizeof (current_view.sv_incarn) /
                                    sizeof (*current_view.sv_incarn)); i++)
        current_view.sv_incarn[i] = DOWN_INCARN;
    current_view.sv_slist[0] = my_site_id;
    current_view.sv_incarn[my_site_no] = my_site_incarn;
    exists_proposal = FALSE;
    proposed_viewid = 1;
    bzero ((char *) proposed_slist, sizeof (proposed_slist));
    bclr (&proposed_failed);
    bclr (&proposed_recovered);
    exists_old_proposal = FALSE;
    old_proposed_viewid = 1;
    bzero ((char *) old_proposed_slist, sizeof (old_proposed_slist));
    bclr (&old_proposed_failed);
    bclr (&old_proposed_recovered);
    if (recovery_type == FD_PARTIAL)
    {
        fd_coordinator = FALSE;
        recov_coordinator = coordinator;
        net_isalive (coordinator);
        fd_msg = msg_newmsg ();
        dest = ADDRESS (SITE_NO (coordinator), SITE_INCARN (coordinator),
                                                  PROTOCOLS, FD_RECVRESTART);
        msg_setdest (fd_msg, &dest);
        pr_bcast (fd_msg);
        msg_delete (fd_msg);
        if (current_view.sv_incarn[my_site_no] == RECOVERY_INCARN)
            (void) t_wait (&want_view, "want_view");
        recov_coordinator = 0;
    }
    else if (recovery_type == FD_TOTAL)
    {
        exists_proposal = TRUE;
        proposed_viewid = INCR_VIEWID (current_view.sv_viewid);
        proposed_slist[0] = MAKE_SITE_ID (my_site_no, 0);
        bis (&proposed_recovered, my_site_no);
        fd_setincarn (0);
        fd_localcommit (proposed_viewid);
    }
    else
        panic ("fd_restart: unknown recovery type");
}


fd_recvrestart (fd_msg)
  message   *fd_msg;

{
    register address* sender;
    address dest;
    message         *msg;

#   ifdef FD_DEBUG
        print ("fd_recvrestart: received ");
        pmsg (fd_msg);
#   endif FD_DEBUG

    msg_increfcount (fd_msg);
    sender = msg_getsender (fd_msg);
    if (!fd_coordinator)
    {
        net_isalive (MAKE_SITE_ID (sender->addr_site, sender->addr_incarn));
        msg = msg_newmsg ();
        dest = ADDRESS (sender->addr_site, sender->addr_incarn, PROTOCOLS,
                                                             FD_RECVNEGACK);
        msg_setdest (msg, &dest);
        pr_bcast (msg);
        msg_delete (msg);
    }
    else
        if (current_view.sv_incarn[sender->addr_site] != DOWN_INCARN)
            fd_seemsdead (MAKE_SITE_ID(sender->addr_site,
                                       current_view.sv_incarn[sender->addr_site]));
        if (!qu_find (pending_recoveries, (int)sender->addr_site))
        {
            qu_add (pending_recoveries, sender->addr_site, (char *) 0,
                                                               nullroutine);
            if (!fd_forked)
            {
                fd_forked = TRUE;
                fd_newview ();
            }
        }
    msg_delete (fd_msg);
}


fd_recvnegack (fd_msg)
  message   *fd_msg;

{
    register address*    sender;

    sender = msg_getsender (fd_msg);
    panic ("fd_recvnegack: %d/%d claims to not be coordinator",
                                                sender->addr_site, sender->addr_incarn);
}


int     crash_self;

fd_recvincarn (fd_msg)
  message   *fd_msg;

{
    register address    *dest;

#   ifdef FD_DEBUG
        print ("fd_recvincarn: received ");
        pmsg (fd_msg);
#   endif FD_DEBUG
    if(crash_self)
        panic("fd_recvincarn: crash self");
    dest = msg_getdests (fd_msg);
    if (dest->addr_incarn != my_site_incarn && my_site_incarn != RECOVERY_INCARN)
        panic ("fd_recvincarn:  Received incarn %d, but I have an incarn %d!",
                                                 dest->addr_incarn, my_site_incarn);
    else if(my_site_incarn == RECOVERY_INCARN)
        fd_setincarn (dest->addr_incarn);
}


fd_setincarn (incarn)
  register incarn;

{
    my_site_incarn = incarn;
    my_site_id = MAKE_SITE_ID (my_site_no, my_site_incarn);
    my_address.addr_incarn = my_site_incarn;
    net_learned_incarn();
    cl_learned_incarn();
}


    
fd_newview ()

{
    register qnode      *qp;
    register int        i;
    register site_id    *sp, s_id;
    register address    *dp;
    message             *fd_msg;
    address             dests[MAX_SITES + 1];
    qnode               *recovering;
    int                 fd_type;
    bitvec              upsites;
    
#   ifdef FD_DEBUG
        print ("fd_newview:\n");
#   endif FD_DEBUG

    recovering = qu_null ();
    do
    {
        if (!exists_proposal)
        {
            proposed_viewid = INCR_VIEWID (current_view.sv_viewid);
            for (i = 0; i < (sizeof (proposed_slist) /
                                               sizeof (*proposed_slist)); i++)
                proposed_slist[i] = current_view.sv_slist[i];
            exists_proposal = TRUE;
        }
        
        if (qu_head (pending_failures))
        {
            fd_type = FD_SHRINK;
            while (qp = qu_head (pending_failures))
            {
                fd_shrinkslist (qp->qu_name);
                qu_free (qp);
            }
        }
        else if (qp = qu_head (pending_recoveries))
        {
            fd_type = FD_GROW;
            bclr (&upsites);
            do
            {
                address addr;
                s_id = fd_growslist (qp->qu_name);
                bis (&upsites, qp->qu_name);
                qu_add (recovering, qp->qu_name, (char *) 0, nullroutine);
                fd_msg = msg_newmsg();
                addr =  ADDRESS (SITE_NO (s_id), SITE_INCARN (s_id), PROTOCOLS, FD_RECVINCARN);
                msg_setdest (fd_msg, &addr);
                net_isalive (s_id);
                pr_bcast (fd_msg);
                msg_delete (fd_msg);
                qp = qp->qu_next;
            }
            while (qp != pending_recoveries);
            fd_lockview (upsites);
        }
        if (fd_computechanges() == FD_TIMEOUT)
            participant_failed = TRUE;
        else
        {
            participant_failed = FALSE;
            while (qp = qu_head (replies_wanted))
                qu_free (qp);
            for (sp = proposed_slist, dp = dests; *sp; sp++)
                if (*sp != my_site_id)
                {
                    *(dp++) = ADDRESS (SITE_NO (*sp), SITE_INCARN (*sp),
                                                   PROTOCOLS, FD_RECVNEWVIEW);
                    qu_add (replies_wanted, *sp, (char *) 0, nullroutine);
                }
            *dp = NULLADDRESS;
            if (dp != dests)
            {
                fd_msg = msg_newmsg ();
                msg_addfield (fd_msg, FLD_FDTYPE, (char*)&fd_type, FTYPE_LONG,
                                                             sizeof (fd_type));
                msg_addfield (fd_msg, FLD_VIEWID, (char*)&proposed_viewid,
                                         FTYPE_LONG, sizeof (proposed_viewid));
                msg_addfield (fd_msg, FLD_SLIST, (char*)proposed_slist, FTYPE_SITEID,
                                                      sizeof (proposed_slist));
                if (exists_old_proposal && !sent_old_proposal)
                {
                    msg_addfield (fd_msg, FLD_OLDVIEWID, (char*)&old_proposed_viewid,
                                     FTYPE_LONG, sizeof (old_proposed_viewid));
                    msg_addfield (fd_msg, FLD_OLDSLIST, (char*)old_proposed_slist,
                                    FTYPE_SITEID, sizeof (old_proposed_slist));
                    sent_old_proposal = TRUE;
                }
                msg_setdests (fd_msg, dests);
                pr_bcast (fd_msg);
                msg_delete (fd_msg);
                t_wait (&got_all_replies, "got_all_replies");
            }

#           ifdef FD_DEBUG
                print ("fd_newview: got all replies\n");
#           endif FD_DEBUG
        }

        if (participant_failed)
        {
            if (fd_type == FD_GROW)
            {
                fd_unlockview ();
                while (qp = qu_head (recovering))
                {
                    fd_ungrowslist (qp->qu_name);
                    qu_free (qp);
                }
            }
        }
        else
        {
            fd_sendcommit (FD_PROPOSED);/*Must send before local commit, else*/
            fd_localcommit (proposed_viewid);    /* takeover code will block */
            if (fd_type == FD_GROW)
                while (qp = qu_head (recovering))
                {
                    register qnode *np = qu_find (pending_recoveries, qp->qu_name);
                    qu_free (np);
                    qu_free (qp);
                }
        }
    }
    while (qu_head (pending_failures) || qu_head (pending_recoveries));

    fd_forked = FALSE;
}


fd_recvnewview (fd_msg)
  message   *fd_msg;

{
    register int        i;
    register site_id    *s_id;
    int                 view_id, fd_type, *old_idp;
    site_id             *slist, deadlist[MAX_SITES + 1], sender_id;
    message             *msg;
    register address*    sender;
    address           dest;
    qnode               *qp;
    bitvec              upsites;
    char                kill_sender, refused;
    
#   ifdef FD_DEBUG
        print ("fd_recvnewview: received ");
        pmsg (fd_msg);
#   endif FD_DEBUG

    if (my_site_incarn == RECOVERY_INCARN)
        return;

    msg_increfcount (fd_msg);
    sender = msg_getsender (fd_msg);
    sender_id = MAKE_SITE_ID (sender->addr_site, sender->addr_incarn);
    view_id = *(int *) msg_getfield (fd_msg, FLD_VIEWID, 1, (int *) 0);
    if (view_id != current_view.sv_viewid)
    {
        slist = (site_id *) msg_getfield (fd_msg, FLD_SLIST, 1, (int *) 0);
        kill_sender = (VIEW_IS_GT (current_view.sv_viewid, view_id) ||
              (VIEW_NO (view_id) == VIEW_NO (current_view.sv_viewid) &&
                                     sender_id != current_view.sv_slist[0]) ||
                   (exists_proposal && VIEW_IS_GT (proposed_viewid, view_id)));
        if (!kill_sender && exists_proposal &&
                               VIEW_NO (view_id) == VIEW_NO (proposed_viewid))
            for (s_id = slist; *s_id; s_id++)
                if (sender_id != proposed_slist[0] &&
                                                 *s_id == proposed_slist[0])
                {
                    kill_sender = TRUE;
                    break;
                }
        if (kill_sender && !qu_find (pending_failures, sender_id))
            qu_add (pending_failures, sender_id, 0, nullroutine);

        refused = FALSE;
        for (s_id = slist; *s_id; s_id++)
            if (qu_find (pending_failures, *s_id))
            {
                refused = TRUE;
                break;
            }

        if (!refused)
        {
            exists_proposal = TRUE;
            proposed_viewid = view_id;
            for (i = 0; i < (sizeof (proposed_slist) /
                                    sizeof (*proposed_slist)); i++)
                proposed_slist[i] = slist[i];

            fd_type = * (int *) msg_getfield (fd_msg, FLD_FDTYPE, 1,
                                                                  (int *) 0);
            if (fd_type == FD_GROW)
            {
                bclr (&upsites);
                for (s_id = proposed_slist; *s_id; s_id++)
                    if (current_view.sv_incarn[SITE_NO (*s_id)] == DOWN_INCARN)
                        bis (&upsites, SITE_NO (*s_id));
                fd_lockview (upsites);
            }
            else
                fd_unlockview ();

            if (old_idp = (int *) msg_getfield (fd_msg, FLD_OLDVIEWID, 1,
                                                                  (int *) 0))
            {
                if (fd_type == FD_GROW)
                    panic ("fd_recvview: extra view with growing view");
                exists_old_proposal = TRUE;
                old_proposed_viewid = *old_idp;
                slist = (site_id *) msg_getfield (fd_msg, FLD_OLDSLIST, 1,
                                                                  (int *) 0);
                for (i = 0; i < (sizeof (old_proposed_slist) /
                                    sizeof (*old_proposed_slist)); i++)
                    old_proposed_slist[i] = slist[i];
            }

            if (fd_computechanges() == FD_TIMEOUT)
            {
                exists_proposal = exists_old_proposal = FALSE;
                if (fd_type == FD_GROW)
                    fd_unlockview ();
                refused = TRUE;
            }
        }
    }

    dest = ADDRESS (sender->addr_site, sender->addr_incarn, PROTOCOLS, FD_RECVACK);
    msg = msg_newmsg ();
    msg_setdest (msg, &dest);
    msg_addfield (msg, FLD_VIEWID, (char*)&view_id, FTYPE_LONG, sizeof (view_id));
    if (refused)
    {
        for (qp = pending_failures->qu_next, s_id = deadlist;
                              qp != pending_failures; qp = qp->qu_next, s_id++)
            *s_id = (site_id) qp->qu_name;
        *s_id++ = 0;

        msg_addfield (msg, FLD_DEADLIST, (char*)deadlist, FTYPE_SITEID,
                                        (s_id - deadlist) * sizeof (site_id));
    }

#   ifdef FD_DEBUG
        print ("fd_recvnewview: sending ack%s\n", refused ? " (refused)" : "");
#   endif FD_DEBUG

    pr_bcast (msg);
    msg_delete (msg);
    msg_delete (fd_msg);
}


fd_computechanges ()
{
    register int    i, j, nloops;
    char            up;
    site_id         *old_slist, *new_slist, prop_id, old_id,
                    deadlist[MAX_SITES];
    bitvec          prop_failed, prop_recovered;
    int             ndead;

#   ifdef FD_DEBUG
        print ("fd_computechanges:\n");
#   endif FD_DEBUG

    bclr (&old_proposed_failed);
    bclr (&old_proposed_recovered);
    bclr (&proposed_failed);
    bclr (&proposed_recovered);
    bclr (&prop_failed);
    bclr (&prop_recovered);
    ndead = 0;

    old_slist = current_view.sv_slist;
    if (exists_old_proposal)
    {
        nloops = 2;
        new_slist = old_proposed_slist;
    }
    else
    {
        nloops = 1;
        new_slist = proposed_slist;
    }

    while (nloops-- > 0)
    {
        for (i = 0, up = TRUE; i < (sizeof (proposed_slist) /
                                             sizeof (*proposed_slist)); i++)
        {
            prop_id = new_slist[i];
            if ((prop_id == 0) && up)
                up = FALSE;
            for (j = 0; (old_id = old_slist[j]) &&
                               (SITE_NO (old_id) != SITE_NO (prop_id)); j++)
                continue;
            if (old_id)
            {
                if (up && (SITE_INCARN (old_id) != SITE_INCARN (prop_id)))
                {
                    if (SITE_NO (prop_id) == my_site_no)
                    {
                        if (SITE_INCARN (prop_id) != my_site_incarn)
                            panic ("fd_computechanges: I refuse to change %s",
                                                     "my incarnation number");
                    }
                    else
                    {
                        net_isdead (old_id);
                        deadlist[ndead++] = old_id;
                        bis (&prop_failed, SITE_NO (old_id));
                        if (!fd_coordinator && !qu_find (pending_failures,
                                                                      old_id))
                            qu_add (pending_failures, old_id, (char *) 0,
                                                                  nullroutine);
                        net_isalive (prop_id);
                    }
                    bis (&prop_recovered, SITE_NO (prop_id));
                }
                if (!up)
                {
                    net_isdead (old_id);
                    deadlist[ndead++] = old_id;
                    bis (&prop_failed, SITE_NO (old_id));
                    if (!fd_coordinator && !qu_find (pending_failures, old_id))
                        qu_add (pending_failures, old_id, (char *) 0,
                                                                  nullroutine);
                }
            }
            else
                if (up)
                {
                    net_isalive (prop_id);
                    bis (&prop_recovered, SITE_NO (prop_id));
                }
        }

        if (new_slist == old_proposed_slist)
        {
            old_proposed_failed = prop_failed;
            old_proposed_recovered =  prop_recovered;
        }
        else
        {
            proposed_failed =  prop_failed;
            proposed_recovered = prop_recovered;
        }
        old_slist = old_proposed_slist;
        new_slist = proposed_slist;
    }

#   ifdef FD_DEBUG
        print ("fd_computechanges: proposed_slist: ");
        for (i = 0, up = TRUE; i < MAX_SITES + 1; i++)
            if (proposed_slist[i])
                print (" %d/%d", SI(proposed_slist[i]));
            else
                if (up)
                {
                    print (" (");
                    up = FALSE;
                }
                else
                    break;
        print (" )\n");
#   endif FD_DEBUG

    for (i = 0; i < ndead; i++)
        if (fd_siteflush (deadlist[i]) == FD_TIMEOUT)
            return (FD_TIMEOUT);
    return (0);
}



fd_recvack (fd_msg)
  message   *fd_msg;

{
    register qnode  *node;
    site_id         sender_id;
    register address* sender;

    if (my_site_incarn == RECOVERY_INCARN)
        return;

    if (*(int*)msg_getfield (fd_msg, FLD_VIEWID, 1, (int *) 0) != 
                                                              proposed_viewid)
        print ("fd_recvack: late ack, current proposal %x\n", proposed_viewid);

#   ifdef FD_DEBUG
        print ("fd_recvack: received ack for viewid %x ", *(int*)msg_getfield (fd_msg, FLD_VIEWID, 1, (int *) 0));
        pmsg (fd_msg);
#   endif FD_DEBUG

    msg_increfcount (fd_msg);
    sender = msg_getsender (fd_msg);
    sender_id = MAKE_SITE_ID (sender->addr_site, sender->addr_incarn);
    if (node = qu_find (replies_wanted, sender_id))
    {
        qu_free (node);
        if (!qu_head (replies_wanted))
            /* Don't change to delayed! */
            t_sig_immed (&got_all_replies, 0);
    }
    else
        print ("fd_recvack: late ack from site %d/%d\n", SI(sender_id));
    if (msg_getfield (fd_msg, FLD_DEADLIST, 1, (int *) 0))
        fd_recvdeadlist (fd_msg);
    msg_delete (fd_msg);
}





fd_sendcommit (which)
  int   which;

{
    register message    *fd_msg;
    register site_id    *sp;
    register address    *dp;
    long                id;
    address             dests[MAX_SITES + 1];

    if (which == FD_CURRENT)
    {
        sp = current_view.sv_slist;
        id = current_view.sv_viewid;
    }
    else
    {
        sp = proposed_slist;
        id = proposed_viewid;
    }

    for (dp = dests; *sp; sp++)
        if (*sp != my_site_id)
            *(dp++) = ADDRESS (SITE_NO (*sp), SITE_INCARN (*sp), PROTOCOLS,
                                                               FD_RECVCOMMIT);
    *dp = NULLADDRESS;
    if (dp != dests)
    {
        fd_msg = msg_newmsg ();
        msg_addfield (fd_msg, FLD_VIEWID, (char*)&id, FTYPE_LONG, sizeof (id));
        msg_setdests (fd_msg, dests);

#       ifdef FD_DEBUG
            print ("fd_sendcommit: sending commit message\n");
#       endif FD_DEBUG

        pr_bcast (fd_msg);
        msg_delete (fd_msg);
    }
}


fd_recvcommit (fd_msg)
  message   *fd_msg;

{
    int         view_id;
    register address*  sender;
    site_id     sender_id;

    if (my_site_incarn == RECOVERY_INCARN)
        return;

    msg_increfcount (fd_msg);
    sender = msg_getsender (fd_msg);
    sender_id = MAKE_SITE_ID (sender->addr_site, sender->addr_incarn);
    view_id = *(int *) msg_getfield (fd_msg, FLD_VIEWID, 1, (int *) 0);

#   ifdef FD_DEBUG
        print ("fd_recvcommit: received commit for view %x ", view_id);
        pmsg (fd_msg);
#   endif FD_DEBUG

    if (exists_proposal)
        if (view_id == proposed_viewid)
            fd_localcommit (view_id);
        else
            fd_seemsdead (sender_id);
    else if (my_site_incarn == RECOVERY_INCARN)
        panic("Site recovery occured too quickly after a failure! (Wait a while and try again)");
    msg_delete (fd_msg);
}



fd_localcommit (view_id)
  int   view_id;

{
    register int    i;
    register qnode  *f_node, *f_next;
    site_id         s_id, *prop_slist;
    int             prop_viewid, nloops, old_n, new_n;
    char            up, old_up,old;
    extern condition want_incarn;

    nloops = 1;
    if (exists_old_proposal)
    {
        if (exists_proposal && view_id == proposed_viewid)
            nloops++;
        else if (view_id != old_proposed_viewid)
            panic ("fd_localcommit: view id %d is invalid", view_id);
        old = TRUE;
        prop_viewid = old_proposed_viewid;
        prop_slist = old_proposed_slist;
    }
    else if (exists_proposal && view_id == proposed_viewid)
    {
        old = FALSE;
        prop_viewid = proposed_viewid;
        prop_slist = proposed_slist;
    }
    else
        panic ("fd_localcommit: But there is no proposed view %d!", view_id);

    while (nloops-- > 0)
    {
        current_view.sv_viewid = prop_viewid;
        for (i = 0; i < (sizeof (current_view.sv_incarn) /
                                  sizeof (*current_view.sv_incarn)) ; i++)
            current_view.sv_incarn[i] = DOWN_INCARN;
        for (i = old_n = 0, old_up = up = TRUE; i < (sizeof (proposed_slist) /
                                               sizeof (*proposed_slist)); i++)
        {
            if (!current_view.sv_slist[i] && old_up)
                old_up = FALSE;
            if (old_up)
                old_n++;
            s_id = prop_slist[i];
            current_view.sv_slist[i] = s_id;
            if (!s_id && up)
            {
                up = FALSE;
                new_n = i;
            }
            if (up)
                current_view.sv_incarn[SITE_NO (s_id)] = SITE_INCARN (s_id);
        }
        if (new_n < (old_n+1) / 2)
        {
            char *str = "fd_localcommit: Possible partition, with this site being in minority partition";
            if (old_n >= 4 || new_n < old_n / 2)
                panic (str);
            print ("*** WARNING: %s\n", str);
            fprintf (stderr, "*** WARNING: %s\n", str);
        }
        if (old)
        {
            current_view.sv_failed = old_proposed_failed;
            current_view.sv_recovered = old_proposed_recovered;
        }
        else
        {
            current_view.sv_failed = proposed_failed;
            current_view.sv_recovered = proposed_recovered;
        }
        for (f_node = pending_failures->qu_next; f_node != pending_failures;
                                                             f_node = f_next)
        {
            f_next = f_node->qu_next;
            if (current_view.sv_incarn[SITE_NO (f_node->qu_name)] !=
                                               SITE_INCARN (f_node->qu_name))
                qu_free (f_node);
        }
        fd_coordinator = (current_view.sv_slist[0] == my_site_id);
        if (old)
        {
            exists_old_proposal = FALSE;
            old_proposed_viewid = 0;
            old_proposed_slist[0] = 0;
            bclr (&old_proposed_failed);
            bclr (&old_proposed_recovered);
        }
        else
        {
            exists_proposal = FALSE;
            proposed_viewid = 0;
            proposed_slist[0] = 0;
            bclr (&proposed_failed);
            bclr (&proposed_recovered);
        }

        while (want_view)
            t_sig_immed (&want_view, 0);

#       ifdef FD_DEBUG
            print ("fd_localcommit: commiting ");
            dump_sview (&current_view);
#       endif FD_DEBUG

        if(fd_coordinator)
        {
            static null_views;
            if(btst(&current_view.sv_failed) || btst(&current_view.sv_recovered))
                null_views = 0;
            else if(++null_views == 5)
                panic("fd_coordinator: 5 attempts to install new views have aborted!");
        }

        /* These routines should not block */
        multi_newview ();
        client_newview ();
        pg_newview ();
        watch_newview ();
        net_newview ();
        st_newview ();

	if(want_incarn)
            t_sig_all(&want_incarn, 0);

        old = FALSE;
        prop_viewid = proposed_viewid;
        prop_slist = proposed_slist;
    }
    fd_unlockview ();
}



fd_seemsalive (site)
  site_id   site;

{
    register qnode      *qp;

#   ifdef FD_DEBUG
        print ("fd_seemsalive: site %d/%d seems alive\n", SI(site));
#   endif FD_DEBUG

    if (qp = qu_find (pending_failures, site))
    {
        qu_free (qp);
    }
    else
        if (!fd_coordinator)
            print ("fd_seemsalive: %d/%d was alive in the first place!\n", SI(site));
}



fd_seemsdead (failed_site)
  site_id   failed_site;

{
    site_id     failed_sites[2];

    failed_sites[0] = failed_site;
    failed_sites[1] = 0;
    fd_seemdead (failed_sites);
}



fd_seemdead (failed_sites)
  register site_id      *failed_sites;

{
    register qnode      *node;
    register site_id    *s_id, *ss_id;
    int                 fd_newview();

#   ifdef FD_DEBUG
        print ("fd_seemdead: seemingly dead sites:");
        dump_shorthexlist (failed_sites);
        print ("\n");
#   endif FD_DEBUG

    if (fd_coordinator)
        if (exists_proposal)
        {
            for (s_id = failed_sites; *s_id; s_id++)
            {
                if (node = qu_find (replies_wanted, *s_id))
                    qu_free (node);
                for (ss_id = proposed_slist; *ss_id; ss_id++)
                    if (SITE_NO(*s_id) == SITE_NO(*ss_id))
                    {
                        if (!qu_find (pending_failures, *s_id))
                            qu_add (pending_failures, *s_id, (char *) 0, nullroutine);
                        net_death_reset (*s_id);
                        participant_failed = TRUE;
                        break;
                    }
            }
            if (!qu_head (replies_wanted) && got_all_replies != 0)
            {
#               ifdef FD_DEBUG
                    print ("fd_seemdead: signaling got_all_replies\n");
#               endif FD_DEBUG
                t_sig (&got_all_replies, 0);
            }
        }
        else
        {
            for (s_id = failed_sites; *s_id; s_id++)
                if (SITE_IS_UP (SITE_NO (*s_id), SITE_INCARN (*s_id)) &&
                                           !qu_find (pending_failures, *s_id))
                    qu_add (pending_failures, *s_id, (char *) 0, nullroutine);
            if (qu_head (pending_failures) && !fd_forked)
            {
                fd_forked = TRUE;
                t_fork_urgent (fd_newview, (char *) 0, (message *) 0);
            }
        }
    else
    {
        if (recov_coordinator)
        {
            for (s_id = failed_sites; *s_id; s_id++)
                if (*s_id == recov_coordinator)
                    panic ("site restart: remote coordinator for my recovery not responding");
            return;
        }
        for (s_id = failed_sites; *s_id; s_id++)
            for (ss_id = (exists_proposal ? proposed_slist :
                                      current_view.sv_slist); *ss_id; ss_id++)
                if (*s_id == *ss_id && !qu_find (pending_failures, *s_id))
                    qu_add (pending_failures, *s_id, (char *) 0, nullroutine);
	for (s_id = current_view.sv_slist; *s_id != my_site_id && *s_id; s_id++)
	    if (!qu_find (pending_failures, *s_id))
	    {
		fd_senddead (*s_id);
		return;
	    }
        fd_takeover ();
    }
}


fd_senddead (s_id)
  site_id   s_id;

{
    site_id         *dp, deadlist[MAX_SITES + 1];
    register qnode  *qp;
    message         *fd_msg;
    address       addr;

#   ifdef FD_DEBUG
        print ("fd_senddeadlist: pending_failures list = [ \n");
        for (qp = pending_failures->qu_next; qp != pending_failures; qp = qp->qu_next)
	    print("%x/%x ", SITE_NO((site_id)qp->qu_name), SITE_INCARN((site_id)qp->qu_name));
	print(" ]\n");
#   endif FD_DEBUG
    for (qp = pending_failures->qu_next, dp = deadlist;
                        qp != pending_failures; qp = qp->qu_next, dp++)
        *dp = (site_id) qp->qu_name;
    *dp++ = 0;

    fd_msg = msg_newmsg ();
    msg_addfield (fd_msg, FLD_DEADLIST, (char*)deadlist, FTYPE_SITEID,
                                          (dp - deadlist) * sizeof (site_id));
    addr = ADDRESS (SITE_NO (s_id), SITE_INCARN (s_id), PROTOCOLS, FD_RECVDEADLIST);
    msg_setdest (fd_msg, &addr);
#   ifdef FD_DEBUG
        print ("fd_senddeadlist: sending pending_failures to %d/%d\n", SI(s_id));
#   endif FD_DEBUG

    pr_bcast (fd_msg);
    msg_delete (fd_msg);
}



fd_recvdeadlist (fd_msg)
  message   *fd_msg;

{
    register site_id    *s_id, *deadlist;
    register address*    sender;

    sender = msg_getsender (fd_msg);

#   ifdef FD_DEBUG
        print ("fd_recvdeadlist: from %d/%d\n", sender->addr_site, sender->addr_incarn);
#   endif FD_DEBUG

    deadlist = (site_id *) msg_getfield (fd_msg, FLD_DEADLIST, 1, (int *) 0);
    for (s_id = deadlist; *s_id; s_id++)
        if (*s_id == my_site_id)
        {
            fd_iamdead (MAKE_SITE_ID (sender->addr_site, sender->addr_incarn));
            return;
        }
                                                                  
    fd_seemdead (deadlist);
}


fd_takeover ()

{
    register int        i;
    int                 fd_newview();

    if (my_site_incarn == RECOVERY_INCARN)
        panic ("fd_takeover: Sorry!  I'm still recovering");

#   ifdef FD_DEBUG
        print ("fd_takeover:\n");
#   endif FD_DEBUG

    if (exists_old_proposal)
        panic ("fd_takeover: Sorry! Takeover with more than one %s",
                                          "pending view not implemented yet");

    fd_coordinator = TRUE;
    if (!exists_proposal)
    {
        exists_old_proposal = FALSE;
        fd_sendcommit (FD_CURRENT);  /* re-commit current view */
    }
    else
    {
        exists_old_proposal = TRUE;
        old_proposed_viewid = proposed_viewid;
        for (i = 0; i < sizeof (proposed_slist); i++)
            old_proposed_slist[i] = proposed_slist[i];
        proposed_viewid = INCR_VIEWID (proposed_viewid);
    }
    sent_old_proposal = FALSE;
    fd_forked = TRUE;
    t_fork (fd_newview, (char *) 0, (message *) 0);
}



fd_iamdead (sid)
  site_id   sid;

{
    panic ("fd_iamdead: %d/%d told me to die", SITE_NO(sid), SITE_INCARN(sid));
}



site_id
fd_growslist (site_no)
  register int      site_no;

{
    register int        i, n_up;
    register site_id    s_id;
    
    for (n_up = 0; proposed_slist[n_up]; n_up++)
        continue;
    for (i = (sizeof (proposed_slist) / sizeof (*proposed_slist)) - 1;
                   i > n_up && SITE_NO (proposed_slist[i]) != site_no; i--)
        continue;
    if (i != n_up)
        s_id = proposed_slist[n_up++] = INCARN_INCR (proposed_slist[i]);
    else
    {
        s_id = proposed_slist[n_up++] = MAKE_SITE_ID (site_no, 0);
        i = (sizeof (proposed_slist) / sizeof (*proposed_slist)) - 1;
    }
    while (i > n_up)
    {
        proposed_slist[i] = proposed_slist[i - 1];
        i--;
    }
    proposed_slist[n_up] = 0;
    return (s_id);
}



fd_ungrowslist (site_no)
  register int      site_no;

{
    register int    i;
    site_id         s_id;

    for (i = 0; proposed_slist[i] && SITE_NO (proposed_slist[i]) != site_no;
                                                                          i++)
        continue;
    if (s_id = proposed_slist[i])
    {
        do
            proposed_slist[i] = proposed_slist[i + 1];
        while (proposed_slist[++i]);
        proposed_slist[i] = INCARN_DECR (s_id);
    }
    else
        panic ("fd_ungrowslist: site is not in proposed_slist");
}



fd_shrinkslist (site)
  site_id   site;

{
    register int    i;

    for (i = 0; proposed_slist[i] && proposed_slist[i] != site; i++)
        continue;
    if (proposed_slist[i])
    {
        do
            proposed_slist[i] = proposed_slist[i + 1];
        while (proposed_slist[++i]);
        proposed_slist[i] = site;
    }
}


fd_strangeviewid (view_id)
  int   view_id;

{
    if (view_id == RECOV_VIEWID || current_view.sv_viewid == RECOV_VIEWID)
        return;

    if (view_id > current_view.sv_viewid)
        if (!((exists_old_proposal && view_id == old_proposed_viewid) ||
                            (exists_proposal && view_id == proposed_viewid)))
            panic ("fd_strangeviewid: current %d, received %d",
                                             current_view.sv_viewid, view_id);
        else
            fd_localcommit (view_id);
}



fd_siteflush (site)
  site_id   site;

{
    site_id         source_id;
    register qnode  *node, *qp, *qqp, **wakeup;
    message         *msg;
    register address* source;
    int             id, proc, rval;
    wait_struct     w_struct;
    char            blocked;
    extern int      cb_count;

    cb_count = 0;
    do
    {
        w_struct.n_events = 0;
        w_struct.cond = (condition) 0;
        blocked = FALSE;
        for (node = pbufs->qu_next; !blocked && node != pbufs;
                                               node = node->qu_next)
        {
            proc = node->qu_name;
            qp = node->qu_queue;
            for (qqp = qp->qu_last; !blocked && qqp != qp; qqp = qqp->qu_last)
            {
                id = qqp->qu_name;
                msg = ((pbuf_item *) qqp->qu_data) -> msg;
                source = msg_getsender (msg);
                source_id = MAKE_SITE_ID (source->addr_site, source->addr_incarn);
                if (source_id == site)
                    blocked = cb_sendpkt (proc, id, &w_struct);
            }
        }
    }
    while (blocked);
    if (w_struct.n_events > 0)
    {
        id = timeout (FD_TIMEOUTVAL, t_sig, &w_struct.cond, FD_TIMEOUT);
        rval = t_wait (&w_struct.cond, "fd_siteflush");
        if (rval == FD_TIMEOUT)
        {
            for (node = pb_itemlist->qu_next; node != pb_itemlist;
                                                         node = node->qu_next)
            {
                wakeup = &((pbuf_item *) node->qu_data)->wakeup;
                if (*wakeup)
                {
                    for (qp = (*wakeup)->qu_next; qp != *wakeup; qp = qqp)
                    {
                        qqp = qp->qu_next;
                        if (&w_struct == (wait_struct *) qp->qu_name)
                            qu_free (qp);
                    }
                    if (qu_head (*wakeup) == (qnode*)0)
                    {
                        qu_free((*wakeup));
                        *wakeup = 0;
                    }
                }
            }
            return (FD_TIMEOUT);
        }
        else
            timeout_cancel (id);
    }
    return (0);
}



fd_init ()

{
    pending_failures = qu_null();
    pending_recoveries = qu_null();
    replies_wanted = qu_null();
}
