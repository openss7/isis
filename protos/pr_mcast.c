/*  $RCSfile: pr_mcast.c,v $ $Revision: 2.22 $ $Date: 90/09/07 16:39:03 $  */
/*
 *	Originally coded by Ken Birman
 *
 *      Multicast and RPC to a list of sites
 *      Reply collation mechanism is more subtle than it may look,
 *      because replies might arrive during iterated delivery when the
 *      sending task is still in SENDMSG (pr_addressing.c).  So,
 *      don't change this stuff unless you are a real wizard and know
 *      exactly how SENDMSG() works in the iterated case!
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

#ifndef	MACH
char    *malloc();
#endif

#define dl_alloc(len)   ((address*)malloc((unsigned)(sizeof(address)*(len))))
#define dl_free(ap)     free((char*)ap)

/*
 *      pr_bcast: multicast to each destination in dest list of msg.
 */
pr_bcast(msg)
  message *msg;
  {
        if(pg_readsview(msg))
        {
            address sender;
            int     *msg_idp;

            if ((msg_idp = (int*)msg_getfield(msg, SYSFLD_PROTID, 1, (int*)0)) == 0)
                panic("pr_bcast: no msg_id");
            sender = *msg_getsender(msg);
            shr_gunlock(*msg_idp, sender.addr_process);
        }
        pr_send(msg, (address*)0, nullroutine, (char*)0, (char*)0);
        return(0);
  }

adesc   pad ={ sizeof(int)*MAX_PROCS, 0, 4 };
adesc   sad ={ sizeof(site_id)*MAX_SITES, 0, 4 };

#define p_alloc()       (int*)mallocate(&pad)
#define p_free(p)       mdeallocate((char*)p,&pad)

#define s_alloc()       (site_id*)mallocate(&sad)
#define s_free(s)       mdeallocate((char*)s,&sad)

/* Multicast to various destinations by calling net_send and pr_local_delivery */
pr_send(msg, alist, callback, arg0, arg1)
 message  *msg;
 address *alist;
 int (*callback)();
 char *arg0, *arg1;
 {
        register local_cnt;
        register *plist;
        register site_id *slist;
        if(alist == 0)
            alist = msg_getdests(msg);
        if(alist == 0)
            return;
        plist = p_alloc();
        slist = s_alloc();
        local_cnt = dltosl(alist, plist, slist);
        if(slist[0])
            net_send(msg, slist, callback, arg0, arg1);
        if(local_cnt)
        {
            pr_local_delivery(msg, plist);
            if(callback)
                (*callback)(my_site_id, arg0, arg1);
        }
        s_free(slist);
        p_free(plist);
  }

#define         UNCOLLECT       0               /* Abort collection... */
#define         COLLECT         1               /* Continue collecting... */

/* Collect the replies for a multicast message */
collect_replies(msgid, alist, nwanted, collect, answ, alen, died)
  int (*collect)();
  char *answ;
  char *died;
  address *alist;
  {
        register address *ap;
        register nleft, nresp = 0, nsent = 0;
        qnode *qp;
        char done[ADDR_LEN];
        register char *dp;

        if(nwanted == 0)
            return(0);

        /* Figure out who to watch */
        dp = done;
        for(ap = alist; !aptr_isnull(ap); ap++)
        {
            if(SITE_IS_UP(ap->addr_site, ap->addr_incarn))
            {
                bis(&ctp->task_watching, ap->addr_site);
                ++nsent;
                *dp++ = 'W';
            }
            else
                *dp++ = 'D';
        }
        *dp = 0;
        nleft = nsent;
        if(nwanted == MAJORITY)
            nwanted = nsent/2+1;
        ctp->task_nwant = nsent;
        if(nsent > nwanted)
            nsent = nwanted;
        ctp->task_nsent = nleft;
        ctp->task_nullreps = ctp->task_nreplies = 0;
        ctp->task_msgid = msgid;
        ctp->task_done = done;
        ctp->task_dests = alist;
        /* Now, loop collecting replies */
        while(nresp < nsent && nleft > 0)
        {
            register message *rmsg;
            register failed;
            address from;
            if(qu_head(ctp->task_msgs) == 0)
                (void)t_wait(&ctp->task_mwant, "mwant");

            while(nresp < nsent && nleft > 0 && (qp = qu_head(ctp->task_msgs)))
            {
                if(qp->qu_name == MC_ISSNO)
                {
                    register s = 0;
                    failed = qp->qu_value;
                    for(ap = alist; !aptr_isnull(ap); ap++, s++)
                        if(failed == ap->addr_site && done[s] == 'W')
                        {
                            if(died)
                                ++died[s];
                            done[s] = 'F';
                            --nleft;
                            ++ctp->task_nullreps;
                        }
                    qu_free(qp);
                    continue;
                }
                rmsg = qp->qu_msg;
		msg_increfcount(rmsg);
                from = *msg_getsender(rmsg);
		if(addr_cmp(&from, &my_startup_addr) == 0)
		    from = my_address;
                qu_free(qp);
                from.addr_entry = 0;
                for(ap = alist; !aptr_isnull(ap); ap++)
                    if(addr_cmp(ap, &from) == 0)
                    {
                        register al = ap-alist;
                        int *abtype;
                        address *fwd;
                        if(abtype = (int*)msg_getfield(rmsg, FLD_ISABORTREP, 1, (int*)0))
                        {
                            if(collect)
                                (*collect)(UNCOLLECT, rmsg, answ, alen, nresp);
                            nleft = 0;
                            nresp = *abtype;
                            break;
                        }
                        if(msg_getfield(rmsg, FLD_FAILED, 1, (int*)0))
                        {
                            if(done[al] != 'W')
                                break;
                            if(died)
                                ++died[al];
                            done[al] = 'F';
                            ++ctp->task_nullreps;
                            --nleft;
                            break;
                        }
                        if(done[al] == 'F')
                        {
                            /*
                             * Special to deal with iterated bcast where
                             * origin failure msg could beat the reply back home...
                             */

                            /* Undo everything */
                            --ctp->task_nullreps;
                            ++nleft;
                            if(died)
                                --died[al];
                            done[al] = 'W';
                        }
                        if(done[al] != 'W' && done[al] != '*')
                             break;
                        else if(msg_getfield(rmsg, FLD_ISNULLREP, 1, (int*)0))
                        {
                            ++ctp->task_nullreps;
                            done[al] = 'N';
                        }
                        else if(fwd = msg_getforwarder(rmsg))
                        {
                            /* Forwarded message */
                            if(SITE_IS_UP(fwd[FWI_NEWDEST].addr_site, fwd[FWI_NEWDEST].addr_incarn))
                            {
                                bis(&ctp->task_watching, fwd[FWI_NEWDEST].addr_site);
                                *ap = fwd[FWI_NEWDEST];
                                done[al] = '*';
                                break;
                            }
                            else
                            {
                                *dp++ = 'D';
                                ++ctp->task_nullreps;
                            }
                        }
                        else
                        {
                            done[al] = 'R';
                            ++ctp->task_nreplies;
                            (*collect)(COLLECT, rmsg, answ, alen, nresp++);
                        }
                        --nleft;
                        break;
                    }
                    /*
                     * In case of clfailed messages, we may have been woken up
                     * unecessarily... otherwise, something is wrong
                     */
                if(aptr_isnull(ap))
                    if(msg_getfield(rmsg, FLD_FAILED, 1, (int*)0) == 0)
                    {
                        print("TASK DIDN'T SEND TO THIS DEST! ");
                        pmsg(rmsg);
                        print("sender was "); paddr(&from);
                        print(", expected replies from:"); paddrs(alist);
                        print("\n");
			pr_dump(-1, "unexpected reply");
                    }
                msg_delete(rmsg);
            }
        }
        ctp->task_msgid = 0;

        /* Flush remainder of qnode */
        while(qp = qu_head(ctp->task_msgs))
            qu_free(qp);
        bclr(&ctp->task_watching);

        ctp->task_nsent = nsent;
        ctp->task_nresp = nresp;
        return(nresp);
  }

/* Collect answers from other protocols processes, answers are in FLD_ANSW */
collect_answ(mode, rmsg, answ, alen, rep_no)
  register message *rmsg;
  register char *answ;
  {
        int flen;
        register char *cp;
        if(mode == UNCOLLECT)
        {
            if(alen == ASTRING || alen == AMALLOC)
                while(--rep_no >= 0)
                {
                    free(((char**)answ)[rep_no]);
                    ((char**)answ)[rep_no] = 0;
                }
        }
        else
        {
            cp = msg_getfield(rmsg, FLD_ANSW, 1, &flen);
            if(flen > alen && alen > 0)
                flen = alen;
            if(flen > 0)
            {
                register char *where;
                if(alen == ASTRING || alen == AMALLOC)
                    where = ((char**)answ)[rep_no] = (char*)malloc(flen);
                else
                    where = &answ[alen * rep_no];
                bcopy(cp, where, flen);
            }
        }
  }

/*
 * This is where replies to CLIENT processes come back,
 * just collect them into a vector because client processes
 * use a different (incompatible) version of reply
 */
collect_msgs(mode, rmsg, answ, alen, rep_no)
  register message *rmsg;
  register message **answ;
  {
        if(mode == UNCOLLECT)
        {
            while(--rep_no >= 0)
            {
                msg_delete(answ[rep_no]);
                answ[rep_no] = 0;
            }
        }
        else
        {
            msg_increfcount(rmsg);
            answ[rep_no] = rmsg;
        }
  }

/* Note when a new view is defined */
multi_newview()
  {
        register qnode *tqp, *ntqp;

        /* Check new current_view, if a site has failed drop it from remdests */
        if(btst(&current_view.sv_failed))
            for(tqp = tasks->qu_next; tqp != tasks; tqp = ntqp)
            {
                ntqp = tqp->qu_next;
                if(bitv(&tqp->qu_task->task_watching, &current_view.sv_failed))
                {
                    register s;

                    for(s = 0; s < MAX_SITES; s++)
                        if(bit(&current_view.sv_failed, s) && bit(&tqp->qu_task->task_watching, s))
                        {
                            qu_add(tqp->qu_task->task_msgs, MC_ISSNO, s, nullroutine);
                            t_sig(&tqp->qu_task->task_mwant, 0);
                        }
                }
            }
  }

/* Send reply message to the sender of a message */
reply(msg, answ, atype, alen)
  message *msg;
  char *answ;
  {
        address temp;
        temp = *msg_getreplyto(msg);
        REPLY(&temp, (int*)0, (address*)0, msg, answ, atype, alen, 0, (char*)0, 0, 0);
  }

/* Send abort reply message to the sender of a message */
abortreply(msg)
  message *msg;
  {
        address temp;
        temp = *msg_getreplyto(msg);
        REPLY(&temp, (int*)0, (address*)0, msg, (char*)-1, IE_ABORT, 0, 0, (char*)0, 0, 0);
  }

reply_addfield(msg, answ, atype, alen, fn, fp, ft, fl)
  message *msg;
  char *answ, *fp;
  {
        address temp;
        temp = *msg_getreplyto(msg);
        REPLY(&temp, (int*)0, (address*)0, msg, answ, atype, alen, fn, fp, ft, fl);
  }

reply_as(proxy, msg, answ, atype, alen)
  address *proxy;
  message *msg;
  char *answ;
  {
        int msgid = msg_getid(msg);
        address temp;
        if((msgid&1) == 0)
            return;
        temp = *msg_getreplyto(msg);
        REPLY(&temp, &msgid, proxy, msg, answ, atype, alen, 0, (char*)0, 0, 0);
  }

int     cl_errno;

/* Special for replying to the local client who invoked a broadcast */
reply_client(mp, rmsgs, nmsgs)
  message *mp, **rmsgs;
  {
        register message *rmsg;
        static address dests[2], sender;
        register address *act;
        int msgid;

        msgid = msg_getid(mp);
        rmsg = msg_newmsg();
        msg_getid(rmsg) = msgid;
        if(cl_errno)
            msg_insertfield(rmsg, SYSFLD_ERRNO, (char*)&cl_errno, FTYPE_LONG, sizeof(long));
        if(act = msg_getact(mp))
            msg_setact(rmsg, act);
        while(nmsgs--)
            msg_addmsg(rmsg, FLD_ANSW, *rmsgs++);
        msg_addfield(rmsg, FLD_NSENT, &ctp->task_nsent, FTYPE_CHAR, 1);
        dests[0] = *msg_getreplyto(mp);
        dests[0].addr_entry = GENERIC_RCV_REPLY;
        msg_setdests(rmsg, dests);
        sender = ctp->task_addr;
        msg_setsender(rmsg, &sender);
        pr_send(rmsg, dests, nullroutine, (char*)0, (char*)0);
        msg_delete(rmsg);
  }

REPLY(dest, msgid, proxy, msg, answ, atype, alen, fn, fp, ft, fl)
  register address *dest, *proxy;
  register message *msg;
  register char *answ, *fp;
  register int *msgid;
  {
        register message *rmsg;
        static address dests[2], sender;
        register address *act;

        rmsg = msg_newmsg();
	if(msgid)
            msg_getid(rmsg) = *msgid;
	else
            msg_getid(rmsg) = msg_getid(msg);
        if(cl_errno)
            msg_insertfield(rmsg, SYSFLD_ERRNO, (char*)&cl_errno, FTYPE_LONG, sizeof(long));
        if(act = msg_getact(msg))
            msg_setact(rmsg, act);
        if(answ && alen)
        {
            if(atype != FTYPE_MESSAGE)
                msg_addfield(rmsg, FLD_ANSW, answ, atype, alen);
            else
                msg_addmsg(rmsg, FLD_ANSW, (message*)answ);
        }
        else if(answ == 0)
            msg_insertfield(rmsg, FLD_ISNULLREP, 0, 0, 0);
        else if(answ == (char*)-1)
            msg_insertfield(rmsg, FLD_ISABORTREP, (char*)&atype, FTYPE_LONG, sizeof(int));
        else
            panic("REPLY: answ %x alen %d atype %d", answ, alen, atype);
        if(fn && fp)
            msg_insertfield(rmsg, fn, fp, ft, fl);
        dests[0] = *dest;
        dests[0].addr_entry = GENERIC_RCV_REPLY;
        msg_setdests(rmsg, dests);
        if(proxy)
            sender = *proxy;
        else
            sender = ctp->task_addr;
        msg_setsender(rmsg, &sender);
        if(msg_tracemsgs)
        {
            int pno = PN_REPLY;
            msg_replacefield(rmsg, SYSFLD_PROTO, (char*)&pno, FTYPE_LONG, sizeof(int));
        }
        pr_send(rmsg, dests, nullroutine, (char*)0, (char*)0);
        msg_delete(rmsg);
  }


/* Receive a reply; pass it up to collect_replies */
rcv_reply(msg)
  register message *msg;
  {
        register msgid = msg_getid(msg);
        register sys_groupview *pg = (sys_groupview*)msg_getfield(msg, SYSFLD_TRUEVIEW, 1, (int*)0);
        register qnode *qp;
        if(pg)
            pg_gottrueview(pg);
        if(tasks->qu_next == tasks)
            return;
        for(qp = tasks->qu_next; qp != tasks; qp = qp->qu_next)
        {
            register task *tp = qp->qu_task;
            if(tp->task_msgid == msgid)
            {
                msg_increfcount(msg);
                qu_add_mp(tp->task_msgs, MC_ISMSG, msg, MSG_DELETE);
                t_sig(&tp->task_mwant, 0);
                return;
            }
        }
  }

pr_clfailed(msg)
  register message *msg;
  {
        address who;
        register s;
        register qnode *qp;

        who = *msg_getsender(msg);
        s = who.addr_site;
        for(qp = tasks->qu_next; qp != tasks; qp = qp->qu_next)
        {
            register task *tp = qp->qu_task;
            if(bit(&tp->task_watching, s))
            {
                msg_increfcount(msg);
                qu_add_mp(tp->task_msgs, MC_ISMSG, msg, MSG_DELETE);
                t_sig(&tp->task_mwant, 0);
            }
        }
        proc_has_failed(&who);
  }
