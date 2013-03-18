/*  $RCSfile: cl_dump.c,v $ $Revision: 2.95 $ $Date: 90/09/07 16:34:13 $  */
/*
 *	Originally coded by Ken Birman
 *
 *      Various dump routines
 *
 *      Interface to most callers:
 *              print(fmt, args): prints to log and flushes
 *              cl_dump(level, "why"): generates a dump at requested level
 *              paddr(addr): prints address, no linefeed
 *              pmsg(&msg): prints sender/dests/msgid, then linefeed
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
 */

#define  ISIS_SNAMES
#include "isis.h"

static	qnode *cl_dumpers;
void    dump_tasks();
int     isis_ignored_replies;
int     isis_ignored_nullreplies;
extern  isis_msgs_enqueued;

void
isis_dump(routine)
  VOID *routine;
  {
	if(cl_dumpers == (qnode*)0)
	    cl_dumpers = qu_null();
	qu_add_proc(cl_dumpers, 0, (int)routine, NULLROUTINE);
  }

void
cl_dump(level, why, a0, a1, a2, a3)
  int level;
  char *why;
  int a0, a1, a2, a3;
  {
        register qnode *qp;
        register s;
        extern char *isis_joining;

	if(my_process_id < -10)
            print("\nCLIENT PROCESS remote_%d INTERNAL DUMP REQUESTED: ", my_process_id & (short)~PID_REMOTE);
	else
            print("\nCLIENT PROCESS %d INTERNAL DUMP REQUESTED: ", my_process_id);
        print(why, a0, a1, a2, a3);
        print("\n");
	if(isis_state)
	{
	    register state = isis_state, n = 0, printed = 0;
	    print("isis internal state vector: [");
	    for(s = 1; state; ++n, s <<= 1)
	        if(state&s)
		{
		    state &= ~s;
		    if(isis_snames[n])
		    {
                        if(printed++)
		            print("|");
	                print("%s", isis_snames[n]);
	            }
		}
#           ifdef BYPASS
	    print("], isis_nblock %d\n", isis_nblock);
#           else
	    print("]\n");
#           endif
	}
        if(isis_joining)
            print("This client is joining group <%s>\n", isis_joining);
        if(level&DUMP_MEM)
        {
            print("Memory mgt: %d allocs, %d frees, %d bytes in use\n", nalloc, nfree, memalloc-memfree);
            print("Message counts: %d allocs %d frees (%d in use), %d enqueued by MSG_ENQUEUE\n",
                msg_namsgs, msg_nfmsgs, msg_namsgs-msg_nfmsgs, isis_msgs_enqueued);
            if(isis_ignored_replies)
		print("*** Ignored %d replies, %d were nullreplies (no bcast-task was waiting)\n", isis_ignored_replies, isis_ignored_nullreplies);
        }
        dump_tasks();
        if((isis_state&ISIS_INIT) == 0)
        {
            print("This client has not (re)initialized ISIS\n");
            return;
        }
        dump_act();
        dump_trans();
        if(qu_head(isis_pgmon) || qu_head(isis_wlist) || qu_head(isis_swlist))
        {
            print("\nMonitoring / watching:\n");
            site_monitor_dump();
            pg_monitor_dump();
            proc_monitor_dump();
        }
        dump_tokens();
        print("\nSite view %d/%d:\n", isis_sv.sv_viewid&0xFF, isis_sv.sv_viewid>>8);
        begin
        {
            register site_id *s;
            for(s = isis_sv.sv_slist; *s; s++)
                print("    %s\t[site_no %d  site_incarn %d]\n", site_names[SITE_NO(*s)],
                        SITE_NO(*s), SITE_INCARN(*s));
        }
        dump_interclient();
        pgroups_dump();
        msg_dumpmsgs();
	if(cl_dumpers)
	    for(qp = cl_dumpers->qu_next; qp != cl_dumpers; qp = qp->qu_next)
	        qp->qu_proc();
  }

void
paddrs(addr)
  address *addr;
  {
        if(addr == 0)
        {
            print("( ??? )");
            return;
        }
        while(addr->addr_site)
            paddr(addr++);
  }

void
paddr(addr)
  address *addr;
  {
        if(addr == 0)
	{
            print("(null addr ptr)");
            return;
	}
        print("(");
        switch(addr->addr_portno)
        {
          case ISAGID:
            print("gaddr=%d/%d.%d[",
                  addr->addr_site, addr->addr_incarn, addr->addr_groupid);
            pentry(addr, addr->addr_entry);
            print("])");
            return;
          case ISALGID:
            print("lgaddr=%d/%d.%d[",
                  addr->addr_site, addr->addr_incarn, addr->addr_groupid);
            pentry(addr, addr->addr_entry);
            print("])");
            return;
          case ISPLIST:
            print("plist=%d/%d:%d)",
                  addr->addr_process, addr->addr_site, addr->addr_incarn);
	    return;
          case ISACT:
            print("act=");
          default:
            print("%d/%d:", addr->addr_site, addr->addr_incarn);
            switch(addr->addr_process)
            {
              case PROTOCOLS:
                print("protos.");
                break;
              case REXEC:
                print("rexec.");
                break;
              case RMGR:
                print("rmgr.");
                break;
              case ISIS:
                print("isis.");
                break;
              case NEWS:
                print("news.");
                break;
              case XMGR:
                print("xmgr.");
                break;
              case LMGR:
                print("lmgr.");
                break;
              default:
		if(addr->addr_process&PID_REMOTE)
                    print("remote_%d.", addr->addr_process & (short)~PID_REMOTE);
		else
                    print("%d.", addr->addr_process);
                break;
            }
            pentry(addr, addr->addr_entry);
            print(")");
            return;
        }
  }

void
peid(eid)
  event_id eid;
  {
        char *cl_getrname();
        print("eid=<e_pname "); 
        if(aptr_isnull(&eid.e_pname))
            print("*");
        else
            paddr(&eid.e_pname);
        print("; e_op %s; e_msgid", eid.e_op? cl_getrname(eid.e_op): "*");
        if(eid.e_msgid)
            print(" %d>", eid.e_msgid);
        else
            print("*>");
  } 

address
atoaddr(str)
  char *str;
  {
        int site, incarn, gaddr, entry;
        address addr;

        bzero(&addr, sizeof(addr));
        if(*str == '(')
            ++str;
        switch(*str)
        {
          case 'g':
            sscanf(str, "gaddr %d/%d.%d[%d]", &site, &incarn, &gaddr, &entry);
            addr.addr_site = site;
            addr.addr_incarn = incarn;
            addr.addr_groupid = gaddr;
            addr.addr_entry = entry;
            addr.addr_portno = ISAGID;
            break;

          default:
            sscanf(str, "%d/%d:", &site, &incarn);
            addr.addr_site = site;
            addr.addr_incarn = incarn;
            addr.addr_portno = 0;
            while(*str && *str != ':') ++str;
            if(*str++ == ':')
            {
                if(*str == 'p')
                    addr.addr_process = PROTOCOLS;
                else if(*str == 'r')
                    addr.addr_process = REXEC;
                else
                    addr.addr_process = atoi(str);
            }
        }
        return(addr);
  }

void
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
};

void
pmsg(msg)
  register message *msg;
  {
        register *proto = (int*)msg_getfield(msg, SYSFLD_PROTO, 1, NULLIARG);
        register address *dp = msg_getdests(msg), *sp, *rp;
        if(dp && dp->addr_entry == GENERIC_NEW_VIEW)
	{
            register sys_groupview *pg;
	    pg = (sys_groupview*)msg_getfield(msg, CL_NEWVIEW, 1, NULLIARG);
            print("NEW VIEW will be %d.0 of  ", pg->pg_viewid);
	    paddr(&pg->pg_gid);
	    print(" =[");
	    paddrs(pg->pg_alist);
	    print(" ]\n");
	}
	else
	{
            print("MSG %x ", msg);
            if(proto)
                print("%s", proto_names[*proto]);
            print(" from ");
	    if(sp = msg_getsender(msg))
                paddr(sp);
	    else
		print(" (no sender) ");
	    if((rp = msg_getsender(msg)) && !addr_isequal(sp, rp))
	    {
	        print(" reply to ");
                paddrs(rp);
	    }
	    if(dp)
	    {
                print(" to ");
                paddrs(dp);
	    }
	    else
		print(" (no dests) ");
	    if(msg_getid(msg))
	        print(" msgid %d", msg_getid(msg));
            print(" size %d\n", msg_getlen(msg));
	}
  }

FILE    *isis_outfile;

void
isis_logging(flag)
  int flag;
  {
        static FILE *file;
        char buf[30];
        if(file == 0)
        {
            sprintf(buf, "%d.log", getpid());
            file = fopen(buf, "w");
        }
        if(flag)
            isis_outfile = file;
        else
            isis_outfile = 0;
  }

void
dump_tasks()
{
    register qnode *qp;
    if(qu_head(isis_tasks))
    {
        print("\nTasks: %d created, %d light-weight context switches\n", isis_created, isis_switched);
        for(qp = isis_tasks->qu_next; qp != isis_tasks; qp = qp->qu_next)
            dump_task(qp);
    }
}

void
dump_task(qp)
  qnode *qp;
   {
        register task *tp = qp->qu_task;
	if(tp->task_flag&TASK_ZOMBIE)
	    return;

        print("  TASK [%x", tp);
#       ifndef THREADS
#          if SUN3 || AUX
           if(isis_ctp != tp)
               print(", pc=0x%x sp=0x%x fp=0x%x",
                     tp-> task_env[0], tp-> task_env[14], tp-> task_env[13]);
#          endif
#          if SUN4
#              define sparc_sp (tp-> task_env[1])
           if(isis_ctp != tp)
               print(", pc=0x%x sp=0x%x",
                     tp-> task_env[0], sparc_sp);
#          endif
#       endif
        print("]: ");
	if(tp == isis_scheduler)
	     print("<ISIS SCHEDULER> ");
	if(tp->task_routine)
	{
	    if(tp->task_flag&TASK_PRNAME)
		print("%s", tp->task_routine);
	    else
               cl_rname(tp->task_routine);
            print("(0x%x), ", tp->task_arg0);
	}
#ifdef CTHREADS
	if(tp->task_name)
            print("Cthread-Name <%s>, ", tp->task_name);
#endif CTHREADS
        if(tp->task_queue)
        {
            if(tp->task_sleep)
            {
                register ms = tp->task_sleep%1000;
                print("sleep(%d", tp->task_sleep/1000);
                if(ms) print(".%.3d secs", ms);
                print(")");
            }
            else if(tp->task_queue == &tp->task_mwant)
	    {
	        print("sent to ");
		paddr(&tp->task_sentto);
		if(tp->task_dests)
		{
                    print("\n   Wants %d of %d replies, got %d+%d null, msgid=%d\n", tp->task_nwant, tp->task_nsent, tp->task_nreplies, tp->task_nullreps, tp->task_msgid);
                    print("   Dests: "); paddrs(tp->task_dests); print("; stat <%s>", tp->task_done);
                }
		else if(tp->task_msgid)
		    print(", msgid=%d", tp->task_msgid);
	    }
            else if(tp->task_waitingfor)
                print("wait (%s)", tp->task_waitingfor);
            else if(tp->task_queue != &isis_runqueue)
                print("wait on user-defined condition (%5x)", tp->task_queue);
	    else
	        print("** runnable **", tp->task_queue);
        }
        else if(tp != isis_ctp)
            print("** suspended, not on a condition queue **");
        else if((tp->task_flag&TASK_CONGESTED) == 0)
            print("** running **");
        else
            print("waiting for ISIS to congestion to subside");
        if(tp->task_flag&TASK_START)
            print(", startup task");
        if(tp->task_flag&TASK_LOGGED)
            print(", logged");
        if(tp->task_flag&TASK_INHIBIT)
            print(", inhibiting joins");
        if(tp->task_flag&TASK_XBYREF)
            print(", fortran mode");
        print(", activity=%d", tp->task_act);
        print("\n");
  }

void
dump_sid(qp)
  qnode *qp;
  {
        print("%d/%d", SITE_NO(qp->qu_sid), SITE_INCARN(qp->qu_sid));
  }

void
dump_cond(qp)
  register qnode *qp;
  {
        print("%x", qp->qu_cond);
  }

void
dump_sview(sv)
  register sview *sv;
  {
        register i;
        print("Got new sview!\n");
        print("sv->viewid %d/%d ", sv->sv_viewid&0xFF, sv->sv_viewid>>8);
        for(i = 0; sv->sv_slist[i]; i++)
            print("<%d/%d> ", SITE_NO(sv->sv_slist[i]), SITE_INCARN(sv->sv_slist[i]));
        print("\n");
        for(i = 0; i < MAX_SITES+1; i++)
            if(sv->sv_incarn[i] != DOWN_INCARN)
                print("sv_incarn[%d] = %d\n", i, sv->sv_incarn[i]);
        for(i = 0; i < MAX_SITES+1; i++)
            if(bit(&sv->sv_failed, i))
                print("site %d has failed since the last view\n", i);
        for(i = 0; i < MAX_SITES+1; i++)
            if(bit(&sv->sv_recovered, i))
                print("site %d has recovered since the last view\n", i);
  }


/*
 *  Note: it is perfectly reasonable for this to return 0 if you don't
 *  have an easy way to compute the <n'th> level caller's address on your
 *  machine.  We only use this for debugging in the message-trace routines
 */
/* HP 9000/300 doesn't like optimizing asm statements. */
#ifdef          hp9000s300
#    include    "OPT_LEVEL_1.h"
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
        asm ("mov.l 0x4(%a0), _db_caller");
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
