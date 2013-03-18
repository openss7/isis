/*  $RCSfile: pr_gbcast.c,v $ $Revision: 2.6 $ $Date: 90/07/31 14:00:07 $  */
/*
 *      Originally coded by Tommy Joseph 1987
 *	Algorithm changed by Ken Birman 1988
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
#include "pr_gbcast.h"

#if     !(MACH|AIXRS)
extern  free();
#endif  MACH

extern gb_free();
extern cb_count;

pr_gbcast(msg)
  message    *msg;
  {
        int                 msg_id, n_dests, priority, *before, bef_size, ph1,
                            *idp;
        site_id             slist[MAX_SITES + 1];
        register site_id    *sp;
        bitvec           scope;

        msg_increfcount(msg);
        if((idp =(int *) msg_getfield(msg, SYSFLD_PROTID, 1,(int *) 0)) != 0)
            msg_id = *idp;
        else
        {
            msg_id = GENMSGID;
            msg_insertfield(msg, SYSFLD_PROTID,(char *) &msg_id, FTYPE_LONG, sizeof(int));
        }
        n_dests = ab_makeslist(slist, msg);
#       ifdef GB_DEBUG
            print("pr_gbcast called for id %x sending gbcast message ", msg_id);
            pmsg(msg);
#       endif  GB_DEBUG
        bclr(&scope);
        for(sp = slist; *sp; sp++)
            bis(&scope, SITE_NO(*sp));
        msg_replacefield(msg, SYSFLD_SCOPE, (char*)&scope, FTYPE_BITVEC, sizeof(bitvec));
        if(n_dests == 0 ||(n_dests == 1 && *slist == my_site_id))
            msg_id |= LOCALFLAG;
        pr_allocinit((char **) &before, &bef_size);
        ph1 = gb_send1(msg_id, msg, slist, &priority, &before, &bef_size);
        if(ph1 == GB_ABORT)
        {
            gb_sendabort(msg_id, slist);
            msg_delete(msg);
            free((char *) before);
	    gb_abort(msg, msg_id);
            if((msg_id&LOCALFLAG) == 0)
                gb_free(msg_id);
            return(-1);
        }
        if(ph1 != 0)
            gb_send2(msg_id, priority, before, slist);
        free((char *) before);
        msg_delete(msg);
        if((msg_id&LOCALFLAG) == 0)
            gb_free(msg_id);
        return(0);
  }


gb_send1(msg_id, msg, slist, priority, before, bef_size)
  int       msg_id, *priority, **before, *bef_size;
  message   *msg;
  site_id   *slist;

  {
        message             *gb_msg;
        address             dests[MAX_SITES + 1];
        gb_answer           *answ[MAX_SITES];
        register            n_replies, n_dests;
        register address    *ap;
        register site_id    *sp;


        gb_msg = msg_newmsg();
        (void) msg_addfield(gb_msg, FLD_MSGID,(char *) &msg_id, FTYPE_LONG, sizeof(msg_id));
        (void) msg_addmsg(gb_msg, FLD_MSG, msg);

        bzero((char *) answ, sizeof(answ));
        n_dests = 0;
        for(sp = slist, ap = dests; *sp; sp++, ap++, n_dests++)
            *ap = ADDRESS(SITE_NO(*sp), SITE_INCARN(*sp), PROTOCOLS, PR_GBRECV1);
        *ap = NULLADDRESS;
#       ifdef GB_DEBUG
            print("gb_send1: for id %x sending gbcast message ", msg_id);
            pmsg(gb_msg);
            print("(set dests to: "); paddrs(dests); print(")\n");
#       endif  GB_DEBUG

        n_replies = ABCAST(dests, gb_msg, ALL, collect_answ,(char *) answ, AMALLOC);
        msg_delete(gb_msg);

        if(n_replies != n_dests)
        {
#           ifdef GB_DEBUG
                print("gb_send1: abort %x!\n", msg_id);
#           endif GB_DEBUG

            return(GB_ABORT);
        }
        else
        {
            register i, j, k, n_before;
            for(i = 0, *priority = 0, n_before = 0; i < n_replies; i++)
            {
                *priority = answ[i]->priority > *priority? answ[i]->priority: *priority;
                for(j = 0; answ[i]->idlist[j] != 0; j++)
                {
                    for(k = 0; k < n_before && (*before)[k] != answ[i]->idlist[j]; k++)
                        continue;
                    if(k == n_before)
                        pr_assign((char *) &(*before)[n_before++], (char *) &answ[i]->idlist[j], sizeof(**before),
                              (char **) before, bef_size);
                }
                free((char *) answ[i]);
                pr_assign((char *) &(*before)[n_before],(char *) 0, sizeof(*before),(char **) before, bef_size);
            }

#           ifdef GB_DEBUG
                print("gb_send1: for id %x, max. priority = %x, before list = ", msg_id, *priority);
                dump_hexlist(*before);
                print("\n");
#           endif GB_DEBUG

            return(n_replies);
        }
  }


gb_send2(msg_id, priority, before, slist)
  int       msg_id, priority, *before;
  site_id   *slist;

  {
        register message    *gb_msg;
        register int        n_before;
        char                answ[MAX_SITES + 1];

        gb_msg = msg_newmsg();
         (void) msg_addfield(gb_msg, FLD_MSGID,(char *) &msg_id, FTYPE_LONG, sizeof(msg_id));
         (void) msg_addfield(gb_msg, FLD_PRIORITY,(char *) &priority, FTYPE_LONG, sizeof(priority));
        for(n_before = 0; before[n_before]; n_before++)
            continue;
         (void) msg_addfield(gb_msg, FLD_BEFORE,(char *) before, FTYPE_LONG, (n_before + 1) * sizeof(*before));
#       ifdef GB_DEBUG
            print("gb_send2: for id %x sending gbcast message ", msg_id);
            pmsg(gb_msg);
#       endif GB_DEBUG
        BCAST_SL(slist, PROTOCOLS, PR_GBRECV2, gb_msg, ALL, collect_answ, answ, 1);
        msg_delete(gb_msg);
#       ifdef GB_DEBUG
            print("gb_send2: id %x terminated\n", msg_id);
#       endif GB_DEBUG
  }



gb_sendabort(msg_id, slist)
  int       msg_id;
  site_id   *slist;

  {
        register message    *gb_msg;
        register site_id    *sp;
        register address    *dp;
        address           dests[MAX_SITES + 1];
        wait_struct         w_str;

#       ifdef GB_DEBUG
            print("gb_sendabort: sending abort for %x\n", msg_id);
#       endif GB_DEBUG

        gb_msg = msg_newmsg();
        (void) msg_addfield(gb_msg, FLD_MSGID,(char *) &msg_id, FTYPE_LONG,
                                                                sizeof(msg_id));
        W_INIT(w_str);
        for(sp = slist, dp = dests; *sp; sp++, dp++)
        {
            *dp = ADDRESS(SITE_NO(*sp), SITE_INCARN(*sp), PROTOCOLS,
                                                                  PR_GBRECVABORT);
            w_str.n_events++;
        }
        *dp = NULLADDRESS;
        msg_setdests(gb_msg, dests);
        pr_send(gb_msg, dests, gb_callback,(char *) &w_str,(char *) 0);
        msg_delete(gb_msg);
        W_WAIT(w_str);
  }



gb_callback(site, w_strp)
  site_id       site;
  wait_struct   *w_strp;
  {
        if(--w_strp->n_events == 0)
            t_sig(&w_strp->cond, 0);
  }



gb_recv1(gb_msg)
  register message  *gb_msg;

  {
        int             msg_id, id, plist[MAX_PROCS + 1], phase;
        int             watch_id, watch_off();
        int             n_before, prc, ans_size;
        message         *msg;
        char            local;
        address*         coord;
        register qnode  *w1_qp, *qp, *ab_node = 0;
        qnode           *node, *idlist, *id_node, *id_next, *pb_node;
        bitvec       *sp;
        int             abortp;
        gb_answer       *answp, *answpp;
        wait_struct     w_struct;
        register int    *pp, i;

        msg_increfcount(gb_msg);
        coord = msg_getsender(gb_msg);
        msg_id = *(int *) msg_getfield(gb_msg, FLD_MSGID, 1,(int *) 0);
#       ifdef GB_DEBUG
            print("gb_recv1: for id %x received message ", msg_id);
            pmsg(gb_msg);
#       endif GB_DEBUG

        n_before = 0;
        local =((msg_id & LOCALFLAG) != 0);
        if(!local && (((phase = st_find_int(msg_id, QU_PHASE)) == 1) || phase == 2))
        {
            /* Was previously in phase 1 or 2 */
            if(qp = qu_find(wait1, msg_id))
                t_wait(&qp->qu_cond, "wait1");
            if((answp =(gb_answer *) st_find(msg_id, QU_GBANSW)) == 0 || answp->priority == GB_ABORT)
            {
#                ifdef GB_DEBUG
                     print("gb_recv1: %x is an old protocol that aborted\n", msg_id);
#                endif GB_DEBUG
                goto abort_case;
            }
            while(answp->idlist[n_before])
                n_before++;
        }
        else /* local or new protocol */
        {
            msg = msg_getmsg(gb_msg, FLD_MSG, 1);

#           ifdef GB_DEBUG
                print("gb_recv1: unpacked "); pmsg(msg);
#           endif GB_DEBUG

            if(local)
            {
                ab_node = qu_add(ablocalq, msg_id,(char *) 0, MSG_DELETE);
                ab_node->qu_args[0] = (char*)msg;
            }
            else
            {
                (void) st_add_int(msg_id, QU_PHASE, 1);
                (void) st_add(msg_id, QU_MSG,(char *) msg, MSG_DELETE);
                if(sp =(bitvec *) msg_getfield(msg, SYSFLD_SCOPE, 1,(int *) 0))
                    st_add_bitvec(msg_id, AS_SCOPE, *sp);
                w1_qp = qu_add(wait1, msg_id,(char *) 0, nullroutine);
                if(coord->addr_site != my_site_no)
                {
                    if(watch_id = watch_on((int) coord->addr_site, (int) coord->addr_incarn, gb_takeover,(char*) msg_id))
                        (void) st_add(msg_id, QU_WATCH,(char *) watch_id, watch_off);
                    else
                        t_fork(gb_takeover,(char *) msg_id,(message *) 0);
                }
            }
            (void) ab_makeplist(msg, plist);
            if(!local &&((answp = (gb_answer *) st_find_int(msg_id, QU_GBANSW)) != 0) && (answp->priority == GB_ABORT))
            {
#               ifdef GB_DEBUG
                    print("gb_recv1: %x already aborted (1)\n", msg_id);
#               endif GB_DEBUG
                st_abort(msg_id);
                while(w1_qp->qu_cond)
                    t_sig(&w1_qp->qu_cond, 0);
                qu_free(w1_qp);
                goto abort_case;
            }

            abortp = 0;
            for(pp = plist; *pp; pp++)
            {
                if(gb_lock(msg, msg_id, *pp) == GB_ABORT)
                {
                    abortp = *pp;
                    break;
                }
            }

            answp = (gb_answer *) st_find_int(msg_id, QU_GBANSW);
            if(abortp || (answp && answp->priority == GB_ABORT))
            {
#               ifdef GB_DEBUG
                    print("gb_recv1: %x already aborted(2)\n", msg_id);
#               endif GB_DEBUG
                if(!local)
                {
                    for(pp = plist; *pp; pp++)
                        if(*pp != abortp)
                            gb_unlock(msg, msg_id, *pp);
                    st_abort(msg_id);
                    while(w1_qp->qu_cond)
                        t_sig(&w1_qp->qu_cond, 0);
                    qu_free(w1_qp);
                }
                goto abort_case;
            }

            if(pg_verify_view(msg) == GB_ABORT)
            {
#               ifdef GB_DEBUG
                    print("gb_recv1: vid verify failed, requesting abort of %x\n", msg_id);
#               endif GB_DEBUG

                if(!local)
                {
                    for(pp = plist; *pp; pp++)
                    {
                        abq_remove(*pp, msg_id);
                        gb_unlock(msg, msg_id, *pp);
                        pr_waitq_remove(*pp);
                    }
                    answp =(gb_answer *) malloc(sizeof(gb_answer));
                    answp->priority = GB_ABORT;
                    answp->idlist[0] = 0;
                    st_add(msg_id, QU_GBANSW,(char *) answp, free);
                    while(w1_qp->qu_cond)
                        t_sig(&w1_qp->qu_cond, 0);
                    qu_free(w1_qp);

                }
                goto abort_case;
            } 

#           ifdef GB_DEBUG
                print("gb_recv1: acquired locks and verified view for id %x\n", msg_id);
#           endif GB_DEBUG

            W_INIT(w_struct);
            pr_allocinit((char **) &answp, &ans_size);
            for(pp = plist; *pp; pp++)
            {
                qnode   *done_id;
                char    blocked;

                qu_add_qu(wait_queues, *pp, qu_null());
                done_id = qu_null();
		cb_count = 0;
                do
                {
                    blocked = FALSE;
                    if(node = qu_find(idlists, *pp))
                    {
                        idlist = node->qu_queue;
                        for(id_node = idlist->qu_next; !blocked && id_node != idlist; id_node = id_next)
                        {
                            id_next = id_node->qu_next;

                            id = id_node->qu_name;
                            if(!qu_find(done_id, id))
                            {
                                for(i = 0; i < n_before && answp->idlist[i] != id; i++)
                                    continue;
                                if(i == n_before)
                                {
                                    qnode *npb_node;
                                    for(pb_node = pbufs->qu_next, prc = pb_node->qu_name; !blocked && pb_node != pbufs; pb_node = npb_node)
                                    {
                                        npb_node = pb_node->qu_next;
                                        blocked = cb_sendpkt(prc, id, &w_struct);
                                    }
                                    if(!blocked)
                                    {
                                        pr_assign((char *) &answp->idlist[n_before++], (char *) &id, sizeof(id), (char **) &answp, &ans_size);
                                        qu_add(done_id, id,(char *) 0, nullroutine);
                                    }
                                }
                            }
                        }
                    }
                }
                while(blocked);
                qu_freeall(done_id);
            }
            W_WAIT(w_struct);
            pr_assign((char *) &answp->idlist[n_before],(char *) 0, sizeof(id),(char **) &answp, &ans_size);
            answp->priority = ab_addtoqueues(msg_id, msg, plist, AB_UNDELIV_GBCAST);

            if(!local)
            {
                while(w1_qp->qu_cond)
                    t_sig(&w1_qp->qu_cond, 0);
                qu_free(w1_qp);
                if(((answpp = (gb_answer *) st_find_int(msg_id, QU_GBANSW)) != 0) && (answpp->priority == GB_ABORT))
                {
#                   ifdef GB_DEBUG
                        print("gb_recv1: %x already aborted(3)\n", msg_id);
#                   endif GB_DEBUG

                    for(pp = plist; *pp; pp++)
                    {
                        abq_remove(*pp, msg_id);
                        gb_unlock(msg, msg_id, *pp);
                        pr_waitq_remove(*pp);
                    }
                    st_abort(msg_id);
                    goto abort_case;
                }
                (void) st_add(msg_id, QU_GBANSW,(char *) answp, free);
            }
        }

        reply(gb_msg,(char *) answp, FTYPE_LONG, (n_before + 2) * sizeof(int));
        msg_delete(gb_msg);
        if(local)
            free((char *) answp);
#       ifdef GB_DEBUG
            print("gb_recv1: sent %sreply for id %x\n", local? "local ": "", msg_id);
#       endif GB_DEBUG
        return;
  abort_case:
#       ifdef GB_DEBUG
            print("gb_recv1: abortreply id %x\n", msg_id);
#       endif GB_DEBUG
        if(coord->addr_site == my_site_no)
            abortreply(gb_msg);
	if(ab_node)
	    qu_free(ab_node);
        msg_delete(gb_msg);
  }


gb_recv2(gb_msg)
  message   *gb_msg;

  {
        int             msg_id, priority, *before, plist[MAX_PROCS + 1],
                        proclist[2], id, before_len, pid, phase;
        int             watch_id, watch_off();
        message         *msg;
        qnode           *node, *w_node, *next, *waitq, *ab_qp;
        address*         coord;
        char            local;
        register int    i, j;

        msg_increfcount(gb_msg);
        msg_id = *((int *) msg_getfield(gb_msg, FLD_MSGID, 1,(int *) 0));

#       ifdef GB_DEBUG
            print("gb_recv2: for id %x received message ", msg_id);
            pmsg(gb_msg);
#       endif GB_DEBUG

        local =((msg_id & LOCALFLAG) != 0);
        if(local)
            if(ab_qp = qu_find(ablocalq, msg_id))
            {
                msg =(message *) ab_qp->qu_args[0];
                if(ab_qp->qu_args[1] ==(char *) GB_ABORT)
                    panic("gb_recv2: local msg %x was aborted", msg_id);
            }
            else
                panic("gb_recv2: can't find local message %x", msg_id);
        else
            if(st_deleteable(msg_id) || !(phase = st_find_int(msg_id, QU_PHASE)) || phase == 2)
            {
                reply(gb_msg, "+", FTYPE_CHAR, 1);
                msg_delete(gb_msg);
                return;
            }
            else
                if((msg =(message *) st_find(msg_id, QU_MSG)) == 0)
                    panic("gb_recv2: can't find message %x", msg_id);

        priority = *((int *) msg_getfield(gb_msg, FLD_PRIORITY, 1,(int *) 0));
        before =(int *) msg_getfield(gb_msg, FLD_BEFORE, 1, &before_len);

        if(!local)
        {
            gb_answer   *answp;
            int         ans_len;

            (void) st_add_int(msg_id, QU_PHASE, 2);
            pr_allocinit((char **) &answp, &ans_len);
            pr_assign((char *) &answp->idlist[0],(char *) before, before_len,(char **) &answp, &ans_len);
            answp->priority = priority;
            st_add(msg_id, QU_GBANSW,(char *) answp, free);
        }

        (void) ab_makeplist(msg, plist);
        if(prio_is_gt(priority, ab_priority))
            ab_priority = priority;
	begin
	{
	    register address *ap, *died = 0;
	    int changesview = pg_changesview(msg);
	    if(changesview && msg_getdests(msg)->addr_entry == GENERIC_DELETE &&
	        (ap = (address*)msg_getfield(msg, CL_PNAME, 1, (int*)0)))
		    died = msg_getsender(msg);
            for(i = 0; pid = plist[i]; i++)
            {
                if(w_node = qu_find(wait_queues, pid))
                {
                    waitq = w_node->qu_queue;
                    for(node = waitq->qu_next; node != waitq; node = next)
                    {
                        next = node->qu_next;
                        id = node->qu_name;
                        if(node->qu_witem.deleted || (died && addr_cmp(msg_getsender(node->qu_witem.msg), died) == 0))
                            j = -1;
                        else
			{
                            for(j = 0; before[j] != id && before[j]; j++)
		                continue;
                            if(before[j] == 0)
                                continue;
			}
                        proclist[0] = pid;
                        proclist[1] = 0;
#                       ifdef GB_DEBUG
                            print("gb_recv2: delivering %x to %x\n", id, pid);
#                       endif GB_DEBUG
                        pr_local_delivery(node->qu_witem.msg, proclist);
                        if(j >= 0 && before[j])
                            (void) idlist_add(pid, id);
                        msg_delete(node->qu_witem.msg);
                        qu_free(node);
                    }
                }
                abq_changeprops(pid, msg_id, priority, AB_DELIV_GBCAST);
                ab_deliver(pid);
                if(!changesview)
                    gb_unlock(msg, msg_id, pid);
	    }
        }
             
        if(local)
        {
            qu_free(ab_qp);
        }
        else
        {
            coord = msg_getsender(gb_msg);
            if(coord->addr_site != my_site_no)
            {
                if(watch_id = watch_on((int) coord->addr_site, (int) coord->addr_incarn, gb_takeover,(char *) msg_id))
                    (void) st_add(msg_id, QU_WATCH,(char *) watch_id, watch_off);
                else
                    t_fork(gb_takeover,(char *) msg_id,(message *) 0);
            }
        }
        reply(gb_msg, "+", FTYPE_CHAR, 1);
        msg_delete(gb_msg);

#       ifdef GB_DEBUG
            print("gb_recv2: id %x done\n", msg_id);
#       endif GB_DEBUG
  }


gb_recvabort(gb_msg)
  message   *gb_msg;

  {
        int             msg_id, phase, plist[MAX_PROCS + 1], *pp;
        message         *msg;
        gb_answer       *answp;
        qnode           *qp;

        msg_increfcount(gb_msg);
        msg_id = *((int *) msg_getfield(gb_msg, FLD_MSGID, 1,(int *) 0));
        msg_delete(gb_msg);

#       ifdef GB_DEBUG
            print("gb_recvabort: received abort for %x\n", msg_id);
#       endif GB_DEBUG

        if((msg_id & LOCALFLAG) == 0)
        {
            phase = st_find_int(msg_id, QU_PHASE);
            if(phase)
            {
                if(phase != 1)
                    panic("gb_recvabort: gbcast not in phase 1");
                if((msg =(message *) st_find(msg_id, QU_MSG)) == 0)
                    panic("gb_recvabort: can't find %x", msg_id);
                if(qu_find(wait1, msg_id))
                {
                    for(pp = plist; *pp; pp++)
                        abort_glock((msg_id & ~LOCALFLAG), *pp);
                    if((answp =(gb_answer *) st_find(msg_id, QU_GBANSW)) == 0)
                    {
                        answp =(gb_answer *) malloc(sizeof(gb_answer));
                        st_add(msg_id, QU_GBANSW,(char *) answp, free);
                    }
                    answp->priority = GB_ABORT;
                    answp->idlist[0] = 0;
                }
                else
                {
                    if((answp =(gb_answer *) st_find(msg_id, QU_GBANSW)) == 0)
                        panic("gb_recvabort: no gbansw");
                    if(answp->priority != GB_ABORT)
                        gb_abort(msg, msg_id);
                }
            }
            else
            {
                extern gb_free();
                answp =(gb_answer *) malloc(sizeof(gb_answer));
                answp->priority = GB_ABORT;
                answp->idlist[0] = 0;
                st_add(msg_id, QU_GBANSW,(char *) answp, free);
            }
            timeout(10000, gb_free, msg_id, 0);
        }
        else if(qp = qu_find(ablocalq, msg_id))
        {
            msg =(message *) qp->qu_args[0];
            (void) ab_makeplist(msg, plist);
            for(pp = plist; *pp; pp++)
                gb_unlock(msg, msg_id, *pp);
            qu_free(qp);
        }
  }


gb_abort(msg, msg_id)
  register message *msg;
  {
        int plist[MAX_PROCS + 1], *pp;
        (void) ab_makeplist(msg, plist);
        for(pp = plist; *pp; pp++)
        {
	    abq_remove(*pp, msg_id);
	    gb_unlock(msg, msg_id, *pp);
	    pr_waitq_remove(*pp);
        }
        st_abort(msg_id);
  }


gb_recvconfirm(gb_msg)
  message   *gb_msg;

  {
        int             msg_id, id, plist[MAX_PROCS + 1], n_before, prc, ans_size;
        message         *msg;
        qnode           *qp, *idlist, *id_node, *id_next, *pb_node;
        gb_answer       *answp;
        wait_struct     w_struct;
        register int    *proc;

        msg_increfcount(gb_msg);
        msg_id = *((int *) msg_getfield(gb_msg, FLD_MSGID, 1,(int *) 0));

        if(msg_id & LOCALFLAG)
            if(qp = qu_find(ablocalq, msg_id))
                msg =(message *) qp->qu_args[0];
            else
                panic("gb_recvconfirm: can't find local message %x", msg_id);
        else
            if((msg =(message *) st_find(msg_id, QU_MSG)) == 0)
                panic("gb_recvconfirm: can't find message %x", msg_id);

        pr_allocinit((char **) &answp, &ans_size);
         (void) ab_makeplist(msg, plist);
        for(proc = plist; *proc; proc++)
        {
            abq_remove(*proc, msg_id);
            pr_waitq_remove(*proc);
        }
        for(proc = plist; *proc; proc++)
            ex_glock((msg_id & ~LOCALFLAG), *proc);

#       ifdef GB_DEBUG
            print("gb_recvconfirm: acquired locks for %x\n", msg_id);
#       endif GB_DEBUG

        W_INIT(w_struct);
        pr_allocinit((char **) &answp, &ans_size);
        for(proc = plist, n_before = 0; *proc; proc++)
        {
            qnode   *done_id;
            char    blocked;

            qu_add_qu(wait_queues, *proc, qu_null());
            done_id = qu_null();
	    cb_count = 0;
            do
            {
                blocked = FALSE;
                if(qp = qu_find(idlists, *proc))
                {
                    idlist = qp->qu_queue;
                    for(id_node = idlist->qu_next; !blocked && id_node != idlist;
                                                                 id_node = id_next)
                    {
                        id_next = id_node->qu_next;
                    
                        id = id_node->qu_name;
                        if(!st_deleteable(id) && !qu_find(done_id, id))
                        {
                            for(pb_node = pbufs->qu_next, prc = pb_node->qu_name;
                                                     !blocked && pb_node != pbufs;
                                                        pb_node = pb_node->qu_next)
                                blocked = cb_sendpkt(prc, id, &w_struct);
                            if(!blocked)
                            {
                                pr_assign((char *) &answp->idlist[n_before++], 
                                  (char *) &id, sizeof(id),(char **) &answp,
                                                                      &ans_size);
                                qu_add(done_id, id,(char *) 0, nullroutine);
                            }
                        }
                    }
                }
            }
            while(blocked);
            qu_freeall(done_id);
        }
        W_WAIT(w_struct);
        pr_assign((char *) &answp->idlist[n_before],(char *) 0, sizeof(id),
                                                    (char **) &answp, &ans_size);
        answp->priority = ab_addtoqueues(msg_id, msg, plist, AB_UNDELIV_GBCAST);

        reply(gb_msg,(char *) answp, FTYPE_LONG,(n_before + 2) * sizeof(int));

        if(msg_id & LOCALFLAG)
            free((char *) answp);
        else
            (void) st_add(msg_id, QU_GBANSW,(char *) answp, free);
        msg_delete(gb_msg);

#       ifdef GB_DEBUG
            print("gb_recvconfirm: sent reply for id %x\n", msg_id);
#       endif GB_DEBUG
  }

            
gb_takeover(msg_id)
  int  msg_id;

  {
        int             priority, *before, bef_size, ph1;
        message         *msg;
        site_id         slist[MAX_SITES + 1];

#       ifdef GB_DEBUG
            print("gb_takeover: taking over for msg %x\n", msg_id);
#       endif GB_DEBUG

        if(st_deleteable(msg_id) || st_find_int(msg_id, QU_PHASE) == 0)
            return;
        if((msg =(message *) st_find(msg_id, QU_MSG)) == 0)
        {
            print("gb_takeover: cannot find message %x in astore", msg_id);
            return;
        }
        ab_makeslist(slist, msg);
        pr_allocinit((char **) &before, &bef_size);
        if((ph1 = gb_send1(msg_id, msg, slist, &priority, &before, &bef_size)) ==
                                                                          GB_ABORT)
        {
            gb_abort(msg, msg_id);
            gb_sendabort(msg_id, slist);
            free((char *) before);
            gb_free(msg_id);
            return;
        }
        if(ph1 != 0)
            gb_send2(msg_id, priority, before, slist);
        free((char *) before);
        gb_free(msg_id);
  }


pr_waitq_remove(pid)
  int   pid;

  {
        register qnode  *qp, *node, *waitq;
        int             plist[2];

        if(qp = qu_find(wait_queues, pid))
        {
            waitq = qp->qu_queue;
            while(node = qu_head(waitq))
            {
                plist[0] = pid;
                plist[1] = 0;
                pr_local_delivery(node->qu_witem.msg, plist);
                if(!(node->qu_witem.deleted))
                    (void) idlist_add(pid, node->qu_name);
                msg_delete(node->qu_witem.msg);
                qu_free(node);
            }
            qu_free(qp);
        }
  }


pr_waitq_add(pid, msg_id, msg)
  int       pid, msg_id;
  message   *msg;

  {
        register qnode  *node, *que;

        if(node = qu_find(wait_queues, pid))
        {
            que = node->qu_queue;
            if(!qu_find(que, msg_id))
            {
                msg_increfcount(msg);
                node = qu_add(que, msg_id,(char *) 0, nullroutine);
                node->qu_witem.msg = msg;
                node->qu_witem.deleted = FALSE;
            }
            return(TRUE);
        }
        else
            return(FALSE);
  }


pr_assign(loc, val, size, start, len)
  register char *loc, *val, **start;
  int           size, *len;
  {
        register char   *pt;
        register int    totlen, rndlen, dbllen, offset, nlen;

        if(*start == 0)
            panic("pr_assign: block not initialized");
        offset = loc - *start;
        totlen = offset + size;
        if(totlen > *len)
        {
            rndlen =((totlen + 0x7f) & ~0x7f);
            dbllen = 2 * *len;
            nlen =  rndlen > dbllen? rndlen: dbllen;
            pt = malloc(nlen);
            if(*len)
                bcopy(*start, pt, *len);
            free(*start);
            *start = pt;
            *len = nlen;
        }
        if(val)
            bcopy(val, *start + offset, size);
        else
            bzero(*start + offset, size);
  }


pr_allocinit(start, len)
  char  **start;
  int   *len;
  {
        *start = malloc(*len = 0x80);
  }
            
            
gb_init()
  {
        wait_queues = qu_null();
        wait1 = qu_null();
        srandom(my_site_no);
  }

/* Shr lock is enough if GBCAST doesn't change the view... */
gb_lock(msg, msg_id, pid)
  register message *msg;
  register msg_id, pid;
  {
        register rv;
        msg_id &= ~LOCALFLAG;
#ifdef  GB_DEBUG
        if(pg_changesview(msg))
            print("gb_lock: get exclusive lock on msgid %x pid %d\n", msg_id, pid);
        else
            print("gb_lock: get shared lock on msgid %x pid %d\n", msg_id, pid);
#endif  GB_DEBUG
        if(pg_changesview(msg))
            rv = ex_glock(msg_id, pid);
        else
            rv = shr_glock(msg_id, pid);
        return(rv);
  }

gb_unlock(msg, msg_id, pid)
  register message *msg;
  register msg_id, pid;
  {
        if(msg_id == 0)
            msg_id = *((int *) msg_getfield(msg, SYSFLD_PROTID, 1,(int *) 0));
        msg_id &= ~LOCALFLAG;
#ifdef  GB_DEBUG
        if(pg_changesview(msg))
            print("gb_unlock: free exclusive lock on msgid %x pid %d\n", msg_id, pid);
        else
            print("gb_unlock: free shared lock on msgid %x pid %d\n", msg_id, pid);
#endif  GB_DEBUG
        if(pg_changesview(msg))
            ex_gunlock(msg_id, pid);
        else
            shr_gunlock(msg_id, pid);
  }
