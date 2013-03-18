/*  $RCSfile: tk_rexec.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:05 $  */
/*
 *	Originally coded by Ken Birman
 *      Toolkit interface to rexec facility
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
 */

#include "isis.h"
#include "rexec.h"


static void
joined(gaddr, addr, event, cond)
  address *gaddr, *addr;
  int event;
  char *cond;
  {
        t_sig((condition *) cond, (void *) ((event == W_FAIL)? -1: 0));
  }

int
isis_rexec(nwanted, gid, sites, prog, args, env, user, passwd, addrs)
  int nwanted;
  address *gid, *addrs;
  register site_id *sites;
  char *prog, **args, **env, *user, *passwd;   
  {
        register message *mp;
        register nrep;
        static firsttime = 0;

	ISIS_ENTER();
	mp = msg_newmsg();
        if(firsttime++ == 0)
            isis_task(joined, "isis_rexec:joined");

        msg_addfield(mp, RE_PROG, prog, FTYPE_CHAR, strlen(prog)+1);
        while(args && *args)
        {
            msg_addfield(mp, RE_ARGS, *args, FTYPE_CHAR, strlen(*args)+1);
            ++args;
        }
        while(env && *env)
        {
            msg_addfield(mp, RE_ENV, *env, FTYPE_CHAR, strlen(*env)+1);
            ++env;
        }
        msg_addfield(mp, RE_USER, user, FTYPE_CHAR, strlen(user)+1);
        msg_addfield(mp, RE_PASSWD, passwd, FTYPE_CHAR, strlen(passwd)+1);
        nrep = 0;
        while(nrep < nwanted && *sites)
        {
            address where, addr;
            where = ADDRESS(SITE_NO(*sites), SITE_INCARN(*sites), REXEC, 0);
            if(cbcast_l("s", &where, REXEC_REQ, mp, ALL, "%a", &addr) > 0)
                if(addr.addr_entry == 0 && !addr_isnull(&addr))
                {
                    if(!addr_isnull(gid))
                    {
                        switch(pg_watch(gid, &addr, W_JOIN, joined, (void *) &isis_ctp->task_cond))
                        {
                          case -1:
                          case 0:
                                break;
                          default:
                                if((int)t_wait_l(&isis_ctp->task_cond,
                                                 "isis system: waiting for join or failure of rexec-ed process")
                                   == -1) 
                                    continue; 
                        }
                    }
                    addrs[nrep++] = addr;
                }
            ++sites;
        }
        addrs[nrep] = NULLADDRESS;
        ISIS_RETURN(nrep);
  }
