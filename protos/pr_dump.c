/*  $RCSfile: pr_dump.c,v $ $Revision: 2.13 $ $Date: 90/08/06 13:47:55 $  */
/*
 *      Various dump routines
 *
 *	Coded by Ken Birman and Tommy Joseph
 *
 *      Interface to most callers:
 *              print(fmt, args): prints to log and flushes
 *              pr_dump("why"): generates a full dump
 *              paddr(addr): prints addr, no linefeed
 *              pmsg(&msg): prints sender/dests/msgid, then linefeed
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
#include "pr_abcast.h"
#include "pr_cbcast.h"
#include "pr_gbcast.h"

int    nalloc, nfree, memused, memalloc, memfree;

extern task scheduler;

pr_dump(level, why, a0, a1, a2, a3)
  char *why;
  {
        register qnode *qp;
        register s;

        print("\nPROTOCOLS PROCESS %d/%d INTERNAL DUMP REQUESTED: ", my_site_no, my_site_incarn);
        print(why, a0, a1, a2, a3);
        print("\n");
        if(level&DUMP_MEM)
        {
            print("Memory mgt: %d allocs, %d frees %d bytes in use\n", nalloc, nfree, memalloc-memfree);
            print("Message counts: %d allocs %d frees (%d in use)\n",
                msg_namsgs, msg_nfmsgs, msg_namsgs-msg_nfmsgs);
        }
        print("\ntasks: scheduler %x ctp %x\n", &scheduler, ctp);
        for(qp = tasks->qu_next; qp != tasks; qp = qp->qu_next)
             dump_task(qp->qu_task);
        print("runqueue: ");
        if(qp = runqueue)
            while((qp = qp->qu_next) != runqueue)
                if(qp->qu_name == TA_ISTASK)
                    print(" [%x]", qp->qu_task);
                else
		{
		    print(" "); 
		    print_rname(qp->qu_freeroutine);
		    print("(%x), ", qp->qu_data);
		}
        print("\n");
        for(s = 0; s < MAX_SITES; s++)
            if(watch_queue[s])
                dump_wqueue(s);
        if(level&DUMP_VIEWS)
        {
            register qnode *cl_root;
            register site_id *sp;
            print("Site view %d/%d:", current_view.sv_viewid&0xFF, current_view.sv_viewid>>8);
            for(sp = current_view.sv_slist; *sp; sp++)
                if(SITE_INCARN(*sp))
                    print(" %d/%d", SITE_NO(*sp), SITE_INCARN(*sp));
                else
                    print(" %d", SITE_NO(*sp));
            print("\n");
            dump_scopes();
            print("\nProcess group views: root %x\n", pg_root);
            for(cl_root = pg_root->qu_next; cl_root != pg_root; cl_root = cl_root->qu_next)
            {
                print("Client"); paddr(&cl_root->qu_pname); print("\n");
                dump_pgqueue(cl_root->qu_queue);
            }
        }
        if(level&DUMP_ASTORE)
        {
            print("\nAssociative store: as_ndelete %d, as_nlocdelete %d\n", as_ndelete, as_nlocdelete);
            for(qp = as_root->qu_next; qp != as_root; qp = qp->qu_next)
                 dump_astore(qp);
            print("\n");
        }
        if(level&DUMP_BCAST)
        {
            dump_abq();
            dump_cbcast();
            dump_gbcast();
            dump_fdect();
        }
        if(level&DUMP_CLIENT)
        {
            print("\nclients:\n");
            dump_clients();
        }
        if(level&DUMP_INTERSITE)
        {
            print("\n");
            dump_intersite();
        }
        if(level&DUMP_MESSAGES)
        {
            print("\n");
            msg_trace_dump();
        }
        fflush(stdout);
  }

typedef struct
{
        int     qi_code;
        char    *qi_name;
        int     (*qi_proutine)();
} qnode_info;

extern qnode_info      qi[];

dump_astore(qp)
  qnode *qp;
  {
        if(qp)
        {
            print("  id= %x", qp->qu_name);
            if(qp->qu_flag&AS_DELETE)
                print("(deletable)");
            if(qp->qu_flag&AS_ADELETE)
                print("(ab-deletable)");
            print("[");
            dump_queue(qp->qu_queue);
            print("]");
            print("\n");
        }
  }

dump_queue(qp)
  qnode *qp;
  {
        register qnode *np;
        if(qp)
        {
            print(" =[");
            np = qp;
            do
                dump_qnode(np);
            while((np = np->qu_next) != qp);
            print("]");
        }
  }

paddrs(addr)
  address *addr;
  {
        if(addr == 0)
        {
            print("( ??? )");
            return;
        }
        while(!aptr_isnull(addr))
            paddr(addr++);
  }

paddr(addr)
  address *addr;
  {
        if(addr_isgid(addr))
        {
            print("(gid %d/%d.%d[%d])", addr->addr_site, addr->addr_incarn, addr->addr_groupid, addr->addr_entry);
            return;
        }
        if(addr_isact(addr))
        {
            print("(act %d/%d.%d[%d])", addr->addr_site, addr->addr_incarn, addr->addr_groupid, addr->addr_entry);
            return;
        }
        else if(!addr_ispid(addr))
        {
            print("(<-- UNKNOWN ADDRESS TYPE -->)");
            return;
        }
        if(addr->addr_incarn != RECOVERY_INCARN)
            print("(%d/%d:", addr->addr_site, addr->addr_incarn);
        else
            print("(%d/recov:", addr->addr_site);
        switch(addr->addr_process)
        {
          case PROTOCOLS:
            print("pr");
            break;
          case REXEC:
            print("rexec");
            break;
          case NEWS:
            print("news");
            break;
          case RMGR:
            print("rmgr");
            break;
          case ISIS:
            print("isis");
            break;
          default:
	    if(addr->addr_process < -10)
		print("remote_%d", addr->addr_process & (short)~PID_REMOTE);
            else 
		print("%d", addr->addr_process);
            break;
        }
	print(".");
        pentry(addr->addr_process, addr->addr_entry);
        print(")");
  }

psid(sid)
  site_id sid;
  {
        print("%d/%d", SITE_NO (sid), SITE_INCARN (sid));
  }

char    *proto_names[]
={
        "cbcast", "iterated cbcast",
        "abcast", "iterated abcast",
        "gbcast", "iterated gbcast",
        "bcast", "iterated bcast",
        "reply",
        "client",
        "fbcast", "iterated fbcast",
};

pmsg(msg)
  register message *msg;
  {
        register msgid = msg_getid(msg), *vp;
        register *proto = (int*)msg_getfield(msg, SYSFLD_PROTO, 1, (int*)0);
        register address *dp = msg_getdests(msg);
        print("MSG %x: ", msg);
        if(proto)
          print("%s ", proto_names[*proto]);
        print("from ");
        paddr(msg_getsender(msg));
        if(dp)
        {
            if(alength(dp) > 2)
                print("\n * to ");
            else
                print("to ");
            paddrs(dp);
        }
        else
            print("to <no dests>");
        print(" size %d", msg_getlen(msg));
        if(msgid)
            print(" sid %d", msgid);
        if(msgid = msg_getid(msg))
	if(vp = (int*)msg_getfield(msg, SYSFLD_VIEWID, 1, (int*)0))
        {
            print("\n * Verify viewid=%d for ", *vp);
            paddr((address*)msg_getfield(msg, SYSFLD_GID, 1, (int*)0));
        }
        print("\n");
  }

dump_pgqueue(pq)
  qnode *pq;
  {
        register sys_groupview *pg;
        register qnode *qp;
        for(qp = pq->qu_next; qp != pq; qp = qp->qu_next)
        {
            print("  ");
            pg = qp->qu_pg;
            print("[%x] ", pg);
            if(addr_cmp(&pg->pg_gid, &qp->qu_pname))
            {
                print("** qu_pname ");
                paddr(&qp->qu_pname);
                print("** ");
            }
            paddr(&pg->pg_gid);
            if(pg->pg_flag || pg->pg_ccount)
	    {
                print("(* ");
                if(pg->pg_flag&PG_CACHED)
                    print("cached ");
                if(pg->pg_ccount)
                    print("%d iterating ", pg->pg_ccount);
                if(pg->pg_flag&PG_REFRESH)
                    print("refreshing ");
                if(pg->pg_flag&PG_DELSENT)
                    print("delsent ");
                if(pg->pg_flag&PG_FAILED)
                    print("failed ");
		print("*) ");
	    }
            print("<%s> =\n", pg->pg_name);
            print("    VID %d = ", pg->pg_viewid, pg->pg_nmemb, pg->pg_nclient);
            paddrs(pg->pg_alist);
            if(pg->pg_nclient)
            {
                print("[");
                paddrs(&pg->pg_alist[pg->pg_nmemb+1]);
                print("]\n");
            }
            else
                print("\n");
        }
  }

dump_wqueue(s)
  {
        register qnode *qp, *fqp;

        fqp = watch_queue[s];
        if(qu_head(fqp) == 0)
            return;
        print("watch_queue[%d/%d]: ", s, fqp->qu_name);
        for(qp = fqp->qu_next; qp != fqp; qp = qp->qu_next)
        {
            print("<%x>= ", qp);
            if(qp->qu_freeroutine != wmagic)
                print("** BAD WATCH NODE! **");
            print ("%d:", qp->qu_name);
            print_rname(qp->qu_proc);
            print("(%x); ", qp->qu_args[1]);
        }
        print("\n");
  }

dump_qnode(qp)
  qnode *qp;
  {
        register n;
        if(qp->qu_name == 0)
            return;
        for(n = 0; qi[n].qi_code; n++)
            if(qi[n].qi_code == qp->qu_name)
            {
                if(qi[n].qi_name)
                {
                    print("<%s: ", qi[n].qi_name);
                    qi[n].qi_proutine(qp);
                    print(">");
                }
                else
                    qi[n].qi_proutine(qp);
                return;
            }
        print("<name=%x: value=%x>",  qp->qu_name, qp->qu_data);
  }

dump_int(qp)
  qnode *qp;
  {
        print("%d", qp->qu_value);
  }

dump_hex(qp)
  qnode *qp;
  {
        print("0x%x", qp->qu_msg);
  }

dump_addr(qp)
  qnode *qp;
  {
        paddr(&qp->qu_address);
  }

dump_bitqp(qp)
  qnode *qp;
  {
        print("bitvec ");
        dump_bitv(&qp->qu_bitvec);
  }

dump_bitv(bits)
  register bitvec *bits;
  {     
        register b;

        print("`");
        for(b = 0; b < ISIS_BVL;b++)
            print("%.8x", bits->bv_data[b]);
        print("'");
  }

dump_taskq(qp)
  register qnode *qp;
  {
        dump_task(qp->qu_task);
  }

dump_task(tp)
   register task *tp;
   {
        print("  TASK[%5x]: ", tp);
        if(tp == &scheduler)
	{
            print("scheduler\n");
	    return;
	}
        else
        {
            print_rname(tp->task_routine);
            print("(%x), ", tp->task_arg0);
        }
        if(tp->task_queue)
        {
            int pr_cbcast(), pr_abcast(), pr_gbcast(), pr_bcast();
            if(tp->task_queue == &tp->task_mwant)
            {
                print("wants %d of %d replies, got %d+%d null, msgid=%d\n", tp->task_nwant, tp->task_nsent, tp->task_nreplies, tp->task_nullreps, tp->task_msgid);
                print("        Dests: "); paddrs(tp->task_dests); print("; stat <%s>", tp->task_done);
            }
            else if(tp->task_queue == &runqueue)
                print("** runnable **");
            else if(check_glock(tp->task_queue) == 0)
                if(strcmp(tp->task_waitingfor, "W_WAIT"))
                    print("wait(%s:%x)", tp->task_waitingfor, tp->task_queue);
                else
		    print("doing cbcast internal flush");
	    if((tp->task_queue != &tp->task_mwant) &&
              (tp->task_routine == pr_cbcast || tp->task_routine == pr_abcast ||
                    tp->task_routine == pr_gbcast || tp->task_routine == pr_bcast))
            {
                message *mp = (message*)tp->task_arg0;
		print(", msgid=%d\n        Dests: ", msg_getid(mp));
                paddrs(msg_getdests(mp));
            }
        }
        else if(tp != ctp)
            print("** suspended, not on a condition queue **");
        else
            print("** running **");
        print("\n");
        if(tp->task_msgs->qu_next != tp->task_msgs)
        {
            register qnode *mp;
            for(mp = tp->task_msgs->qu_next; mp != tp->task_msgs; mp = mp->qu_next)
                if(mp->qu_name == MC_ISMSG)
                {
                    print("  ...");
                    pmsg(mp->qu_msg);
                }
        }
  }

dump_str(qp)
  qnode *qp;
  {
       print("%s", qp->qu_data);
  }

dump_sid(qp)
  qnode *qp;
  {
        print("%d/%d", SITE_NO(qp->qu_sid), SITE_INCARN(qp->qu_sid));
  }

dump_cond(qp)
  register qnode *qp;
  {
        print("%x", qp->qu_cond);
  }

print(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
  char *fmt;
  {
	extern ISIS_TIME;
	static PREV_TIME, newline = 1;
	register delta = ISIS_TIME-PREV_TIME;
	if(newline && delta > 60000)
	{
            struct timeval rtime;
            gettimeofday(&rtime, (struct timezone*)0);
            printf("... Time is now %s", ctime(&rtime.tv_sec));
            PREV_TIME = ISIS_TIME;
	}
        printf(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
        fflush(stdout);
	newline = (fmt[strlen(fmt)-1] == '\n');
  }

#define SI(x)   SITE_NO(x),SITE_INCARN(x)

dump_fdect()
  {
        register site_id    *s_id;
        register qnode      *qp;
        register int        i;

        print ("\nFailure detector: current ");
        dump_sview (&current_view);
        print ("  %scoord, %sfork, %sfail, %sprop, %soprop, %ssent_oprop\n",
            (fd_coordinator ? "" : "not "), (fd_forked ? "" : "no "),
            (participant_failed ? "" : "no "), (exists_proposal ? "" : "no "),
            (exists_old_proposal ? "" : "no "), (sent_old_proposal ? "" : "not "));
                                          
        if (exists_old_proposal)
        {
            print ("  Old proposed viewid %d:\n      slist:",
                                                             old_proposed_viewid);
            for (s_id = old_proposed_slist; *s_id; s_id++)
                print (" %d/%d", SI(*s_id));
            print ("\n    failed: ");
            dump_bitv (&old_proposed_failed);
            print (", recovered: ");
            dump_bitv (&old_proposed_recovered);
            print ("\n");
        }
        if (exists_proposal)
        {
            print ("  Proposed viewid %d:\n      slist:", proposed_viewid);
            for (s_id = proposed_slist; *s_id; s_id++)
                print (" %d/%d", SI(*s_id));
            print ("\n    failed: ");
            dump_bitv (&proposed_failed);
            print (", recovered: ");
            dump_bitv (&proposed_recovered);
            print ("\n");
        }
        print ("  Pending failures:");
        for (qp = pending_failures->qu_next; qp != pending_failures;
                                                               qp = qp->qu_next)
            print (" %d/%d", SI(qp->qu_name));
        print ("\n  Pending recoveries:");
        for (qp = pending_recoveries->qu_next; qp != pending_recoveries;
                                                               qp = qp->qu_next)
            print (" %d", qp->qu_name);
        print ("\n  Replies wanted:");
        for (qp = replies_wanted->qu_next; qp != replies_wanted;
                                                               qp = qp->qu_next)
            print (" %d/%d", SI(qp->qu_name));
        print ("\n  View r_locks: ");
        dump_bitv (&sview_rlocks);
        for (i = 0; i < MAX_SITES; i++)
            if (sview_nlocks[i])
                print (" %d:%d", i, sview_nlocks[i]);
        print ("\n  View w_locks: ");
        dump_bitv (&sview_wlocks);
        print ("\n  View want_w_locks: ");
        dump_bitv (&sview_want_wlocks);
        print ("\n");
  }

dump_abq()
 {
        register qnode    *abqp, *qp, *subq;
        register abq_item *item;
     
        print("abq:\n");
        print ("  max_priority = %x\n", ab_priority);
        for (abqp = abq->qu_next; abqp != abq; abqp = abqp->qu_next)
        {
            print ("  process %d message queue:\n", abqp->qu_name);
            subq = abqp->qu_queues[0];
            for (qp = subq->qu_next; qp != subq; qp = qp->qu_next)
            {
                item = (abq_item *) qp->qu_data;
                print ("    msg_id=%x, data=[%x]%x/%x/", qp->qu_name, item,
                                                  item->msg_id, item->priority);
                switch (item->tag)
                {
                  case AB_DELIVERABLE:
                    print ("deliverable");
                    break;
                  case AB_UNDELIVERABLE:
                    print ("undeliverable");
                    break;
                  case AB_DELIV_GBCAST:
                    print ("deliverable gbcast");
                    break;
                  case AB_UNDELIV_GBCAST:
                    print ("undeliverable gbcast");
                    break;
                  default:
                    print ("?%d?", item->tag);
                    break;
                }
                print ("/%x\n", item->msg);
            }
            
            print ("  process %d priority queue:\n   ", abqp->qu_name);
            subq = abqp->qu_queues[1];
            dump_hexaddrq (subq);
            print ("\n");
        }
 }
 
dump_array (name, n)
 int  *name, n;
 {
        register int i;
        for (i = 0; i < n; i++)
            print (" %d", name[i]);
 }
 
 
dump_list (name)
 int  *name;
 {
        register int i;
        for (i = 0; name[i]; i++)
            print (" %d", name[i]);
 }


dump_hexlist (name)
 int  *name;
 {
        register int i;
        for (i = 0; name[i]; i++)
            print (" %x", name[i]);
 }


dump_shorthexlist (name)
 short  *name;
 {
        register short i;
        for (i = 0; name[i]; i++)
            print (" %x", name[i]);
 }


dump_qnames (qp)
 qnode  *qp;
 {
        register qnode  *qqp;
        for (qqp = qp->qu_next; qqp != qp; qqp = qqp->qu_next)
            print (" %d", qqp->qu_name);
 }
 
dump_qhexnames (qp)
 qnode  *qp;
 {
        register qnode  *qqp;
        for (qqp = qp->qu_next; qqp != qp; qqp = qqp->qu_next)
            print (" %x", qqp->qu_name);
 }
 
dump_qnodes (qp)
 qnode  *qp;
 {
        register qnode  *qqp;
        for (qqp = qp->qu_next; qqp != qp; qqp = qqp->qu_next)
            print (" %d:%d", qqp->qu_name, qqp->qu_value);
 }
 
vdump_addrq (qp)
 qnode  *qp;
 
 {
        register qnode  *qqp;
        print ("header[%x]:\n", qp);
        qqp = qp;
        do
        {
            print ("   [%x]name:%d, data:%x, next:%x, last:%x\n", qqp,
                         qqp->qu_name, qqp->qu_data, qqp->qu_next, qqp->qu_last);
            qqp = qqp->qu_next;
        }
        while (qqp != qp);
 }
 
dump_addrq (qp)
 qnode  *qp;
 {
        register qnode  *qqp;
        for (qqp = qp->qu_next; qqp != qp; qqp = qqp->qu_next)
            print (" %d:%x", qqp->qu_name, qqp->qu_data);
 }   
 
dump_hexaddrq (qp)
 qnode  *qp;
 {
        register qnode  *qqp;
        for (qqp = qp->qu_next; qqp != qp; qqp = qqp->qu_next)
            print (" %x:%x", qqp->qu_name, qqp->qu_data);
 }   
 
dump_idq (qp)
 qnode  *qp;
 {
        register qnode  *qqp;
        for (qqp = qp->qu_next; qqp != qp; qqp = qqp->qu_next)
        {
            print (" ");
            psid (qqp->qu_name);
        }
 }
 
dump_cbcast()
 {
        register qnode       *pbuf, *qp;
        register pbuf_item   *item;
        print ("cbcast data structures:\n");
        print ("  pbufs:");
        dump_addrq (pbufs);
        print ("\n");
        for (qp = pbufs->qu_next; qp != pbufs; qp = qp->qu_next)
        {
            pbuf = qp->qu_queue;
            print ("    [%x]process %d:", pbuf, qp->qu_name);
            dump_hexaddrq (pbuf);
            print ("\n");
        }
        print ("  pb_itemlist:\n");
        for (qp = pb_itemlist->qu_next; qp != pb_itemlist; qp = qp->qu_next)
        {
            print ("    [%x]%x:", qp, qp->qu_name);
            item = (pbuf_item *) qp->qu_data;
            dump_pbitem (item);
            print ("\n");
        }
        print ("  idlists:\n");
        for (qp = idlists->qu_next; qp != idlists; qp = qp->qu_next)
        {
            print ("    process %d:", qp->qu_name);
            dump_qhexnames (qp->qu_queue);
            print ("\n");
        }
        print ("  piggylists:\n");
        for (qp = piggylists->qu_next; qp != piggylists; qp = qp->qu_next)
        {
            print ("    cb_msg %x:", qp->qu_name);
            dump_qhexnames (qp->qu_queue);
            print ("\n");
        }
 }
 
dump_pbitem (item)
  pbuf_item  *item;
  {
        print (" [%x]%x,", item, item->msg);
        dump_idq (item->rem_dests);
        print (",");
        dump_bitv (&item->sent);
        print (",");
        dump_bitv (&item->received);
        print (",");
        print ("%spf,%d,", (item->piggy_flag ? "" : "no "), item->refcount);
        if (item->wakeup)
            dump_qhexnames (item->wakeup);
        else
            print ("0");
  }
     
dump_gbcast()
  {
        register qnode       *qp, *qqp, *w_queue;
        print ("gbcast data structures:\n");
        print ("  wait1:");
        dump_qhexnames (wait1);
        print ("\n  wait queues:");
        dump_addrq (wait_queues);
        print ("\n");
        for (qp = wait_queues->qu_next; qp != wait_queues; qp = qp->qu_next)
        {
            w_queue = qp->qu_queue;
            print ("    [%x->%x]process %d:", qp, w_queue, qp->qu_name);
            for (qqp = w_queue->qu_next; qqp != w_queue; qqp = qqp->qu_next)
                print (" [%x]%x:%x%s", qqp, qqp->qu_name, (qqp->qu_witem).msg,
                             (qqp->qu_witem).deleted ? "(deleted)" : "");
            print ("\n");
        }
        print ("  ");
        dump_glocks();
  }


dump_sview (view)
  register sview *view;
  {
        register int    i;
        print ("view %d/%d:\n", view->sv_viewid&0xFF, view->sv_viewid>>8);
        print ("  slist:");
        for (i = 0; view->sv_slist[i]; i++)
            print (" %x", view->sv_slist[i]);
        print ("\n  incarn:");
        for (i = 0; i <= MAX_SITES; i++)
            if (view->sv_incarn[i] != DOWN_INCARN)
                print (" %d/%d", i, view->sv_incarn[i]);
        print ("\n  failed: ");
        dump_bitv (&view->sv_failed);
        print ("\n  recovered: ");
        dump_bitv (&view->sv_recovered);
        print ("\n");
  }

dump_glocks ()
  {
        register qnode  *node;
        register glock  *lock;
    
        print ("glocks:\n");
        for (node = glocks->qu_next; node != glocks; node = node->qu_next)
        {
            lock = (glock *) node->qu_data;
            print ("    process %d:\n", node->qu_name);
            if (lock->shrlock)
            {
                print ("      shrlock:");
                dump_qhexnames (lock->shrlock);
                print ("\n");
            }
            if (lock->exlock)
                print ("      exlock: %x\n", lock->exlock);
            if (lock->want_shrlock)
            {
                print ("      want_shrlock:");
                dump_qhexnames (lock->want_shrlock);
                print ("\n");
            }
            if (lock->want_exlock)
            {
                print ("      want_exlock:");
                dump_qhexnames (lock->want_exlock);
                print ("\n");
            }
        }
  }


check_glock (cond)
  condition *cond;
  {
        register qnode  *node, *qp;
        register glock  *lock;

        for (node = glocks->qu_next; node != glocks; node = node->qu_next)
        {
            lock = (glock *) node->qu_data;
            if (lock->want_shrlock)
                for (qp = lock->want_shrlock->qu_next; qp != lock->want_shrlock;
                                                                 qp = qp->qu_next)
                    if(cond == &qp->qu_cond)
                    {
                        print("wait(want_shrlock: %x)", node->qu_name);
                        return(1);
                    }
            if (lock->want_exlock)
                for (qp = lock->want_exlock->qu_next; qp != lock->want_exlock;
                                                                 qp = qp->qu_next)
                    if(cond == &qp->qu_cond)
                    {
                        print("wait(want_exlock: %x)", node->qu_name);
                        return(1);
                    }
        }
        return(0);
  }

qnode_info      qi[]
={
        QU_PHASE,       "phase",                dump_int,
        QU_WATCH,       "watch_id",             dump_int,
        QU_MSG,         "qu_message",           dump_hex,
        QU_PRIORITY,    "priority",             dump_hex,
        QU_MSGQ,        "message queue",        dump_queue,
        QU_PRIORITYQ,   "priority queue",       dump_queue,
        QU_CBCLEANUP,   "cb_cleanup",           dump_hex,
        QU_FAILEDSITE,  "failed site",          dump_sid,
        QU_GBANSW,      "gb_answ",              dump_hex,
        QU_GBANSW1A,    "gb_answ1a",            dump_int,
        AS_IGNORE,      "as_ignore",            dump_bitqp,
        AS_SENT,        "as_sent",              dump_bitqp,
        AS_SCOPE,       "as_scope",             dump_bitqp,
        AS_FREED,       "as_freed",             dump_bitqp,
        MC_ISMSG,       "mc_message",           dump_hex,
        TA_ISTASK,      "task",                 dump_taskq,
        0,              0,                      0,
};

static  char *rnames[MAXENTRIES];
static  (*routines[MAXENTRIES])();
static  rptr;

isis_rname(routine, rname)
  int (*routine)();
  char *rname;
  {
        if(rptr == MAXENTRIES)
            panic("isis_rname");
        routines[rptr] = routine;
        rnames[rptr++] = rname;
  }

print_rname(routine)
  int (*routine)();
  {
        register r;

        for(r = 0; r < rptr; r++)
            if(routines[r] == routine)
            {
                print(rnames[r]);
                return;
            }
        print("%x", routine);
  }

msg_trace_dump()
{
    msg_dumpmsgs ();
    print ("\n");
}

/*
 * This routine is not really necessary, but helpful for us when debugging 
 * It should return 0 on machines where you don't know how to figure
 * out who the caller <n> levels up was
 */
/* HP 9000/300 doesn't like optimizing asm statements. */
#ifdef          hp9000s300
#    include "OPT_LEVEL_1.h"
#endif

char    *db_caller;
char *
callername (n_levels)
  int   n_levels;

{
#   if(SUN3)
    {
        asm ("movl a6, a0");
        while (n_levels-- > 0)
            asm ("movl a0@, a0");
        asm ("movl a0@(0x4), _db_caller");
    }
#   endif
#   if(AUX)
    {
        asm ("mov.l %a6, %a0");
        while (n_levels-- > 0)
            asm ("mov.l (%a0), %a0");
        asm ("mov.l 0x4(%a0), db_caller");
    }
#   endif
#   if(HPUX)
    {
#ifdef hp9000s300
        asm ("mov.l %a6, %a0");
        while (n_levels-- > 0)
            asm ("mov.l (%a0), %a0");
        asm ("mov.l 0x4(%a0), _db_caller");
#endif hp9000s300
#ifdef hp9000s800
        db_caller = get_sp();
#endif hp9000s800
    }
#   endif
    return (db_caller);
}
