/*  $RCSfile: pr_local.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:49 $  */
/*
 *	Originally coded by Ken Birman
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
#define  GENERIC_ENTRIES

#include "pr.h"

static  (*entries[MAXENTRIES])();
int     inhibit_swtch;

/* Do local deliveries of a message with some dests at this site */
pr_local_delivery_urgent(msg, plist)
  register message *msg;
  register int *plist;
  {
        int t_fork_urgent();
        do_local_delivery(t_fork_urgent, msg, plist);
  }

pr_local_delivery_immed(msg, plist)
  register message *msg;
  register int *plist;
  {
        int t_fork_immed();
        do_local_delivery(t_fork_immed, msg, plist);
  }

pr_local_delivery(msg, plist)
  register message *msg;
  register int *plist;
  {
        int t_fork();
        ++inhibit_swtch;
        do_local_delivery(t_fork, msg, plist);
        --inhibit_swtch;
  }

do_local_delivery(forker, msg, plist)
  int (*forker)();
  register message *msg;
  register int *plist;
  {
        register cnt = 0, send_remote = 0;
        register address *alist, *ap, *bp;
        int internal_invoke;
        register *pp;
        alist = msg_getdests(msg);
        if(pp = plist)
        {
            internal_invoke = 0;
            while(*pp)
                if(*pp == my_process_id)
                {
                    ++pp;
                    ++internal_invoke;
                }
                else
                {
                    ++cnt;
		    if(*pp > -10 || send_remote++ == 0)
                        pr_local_send(*pp++, msg);
		    else
			++pp;
                }
        }
        else
        {
            internal_invoke = 1;
            if(alist == 0) 
                return; 
            for(ap = alist; !aptr_isnull(ap); ap++)
                if(ap->addr_site == my_site_no && (ap->addr_incarn == RECOVERY_INCARN || ap->addr_incarn == my_site_incarn || my_site_incarn == RECOVERY_INCARN) && ap->addr_process != my_process_id)
                 {
                    for(bp = alist; bp != ap; bp++)
                        if(bp->addr_process == ap->addr_process && bp->addr_site == ap->addr_site)
                            break;
                    if(bp == ap)
                    {
                        ++cnt;
		        if(ap->addr_process > -10 || send_remote++ == 0)
                            pr_local_send(ap->addr_process, msg);
                    }
                 }
        }
        /* Loop to do internal invocations, if any */
        if(internal_invoke)
            while(alist && !aptr_isnull(alist))
            {
                if(alist->addr_site == my_site_no && (alist->addr_incarn == my_site_incarn || alist->addr_incarn == RECOVERY_INCARN || my_site_incarn == RECOVERY_INCARN))
                {
                    if(alist->addr_process == PROTOCOLS)
                    {
                        register e = alist->addr_entry;
                        ++cnt;
                        if(e >= 0 && e < MAXENTRIES && entries[e])
                        {
                            if(e == GENERIC_RCV_REPLY)
                                rcv_reply(msg);
                            else
                            {
                                my_entry = e;
                                (*forker)(entries[e], (char*)msg, msg);
                                my_entry = 0;
                            }
                        }
                        else
                        {
                            print("Address ");
                            paddr(alist);
                            print(" unknown\n");
                        }
                    }
                }
                ++alist;
            }
        if(cnt == 0)
        {
            print("Site %d/%d found no local dests for ", my_site_no, my_site_incarn);
            pmsg(msg);
        }
  }

/* Define mapping from entry codes to routines */
isis_entry(code, routine, rname)
  register code, (*routine)();
  char *rname;
  {
        if(code < 0 || code >= MAXENTRIES)
            panic("entry code %d", code);
        if(entries[code] && entries[code] != routine)
        {
            print("WARNING: Refinition of entry %d as %s, was ", code, rname);
            print_rname(entries[code]);
            print("\n");
        }
        entries[code] = routine;
        isis_rname(routine, rname);
  }

pentry(who, code)
  {
        if(code >= SYS_BASE && code <= MAXENTRIES)
            print(generic_entries[code-SYS_BASE]);
        else if(who == PROTOCOLS)
            print_rname(entries[code]);
        else
            print("%d", code);
  }
