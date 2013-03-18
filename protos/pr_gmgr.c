/*  $RCSfile: pr_gmgr.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:34 $  */
/*
 *	Originally coded by Ken Birman
 *      Process group structure management
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

adesc   pg_adesc ={ sizeof(sys_groupview), sizeof(sys_groupview), 16 };

#define pg_alloc()      (sys_groupview*)mallocate(&pg_adesc)

pg_free(pg)
  register sys_groupview *pg;
  {
        if(pg)
	{
            if(pg->pg_hipri || pg->pg_lopri || pg->pg_newview || pg->pg_bwait || pg->pg_rwait)
	        panic("pg_free %x: wasn't fully signalled!", pg);
            mdeallocate((char*)pg, &pg_adesc);
	}
  }

address make_gid()
  {
        static gids;
        address gid;
        gid = ADDRESS(my_site_no, my_site_incarn, 0, 0);
        gid.addr_portno = ISAGID;
        gid.addr_groupid = ++gids;
        return(gid);
  }

/*
 * If this message mentions a viewid then it was sent using the
 * cache iteration policy.  Returns -1 if the message should NOT be
 * delivered, 0 if it can be delivered
 */
pg_verify(dest, mp)
  register message *mp;
  address dest;
  {
        register *viewid = (int*)msg_getfield(mp, SYSFLD_VIEWID, 1, (int*)0);
	register address *to, *ap;

	if(ap = (address*)msg_getfield(mp, SYSFLD_EXCLUDE, 1, (int*)0))
	    if(addr_isequal(ap, &dest))
	    {
		REPLY(msg_getreplyto(mp), &msg_getid(mp), &dest, mp, 0, 0, 0, 0, 0, 0, 0);
		return(-1);
	    }
        if(viewid)
        {
            address gid;
            address from;
            register qnode *cl_root, *qp = 0;
            register dont_reply = 0;
            register sys_groupview *pg = 0;
            to = msg_getdests(mp);
            from = *msg_getreplyto(mp);
            while(!aptr_isnull(to))
                if(to->addr_site == from.addr_site && to->addr_incarn == from.addr_incarn)
                    break;
                else
                    ++to;
            if(!aptr_isnull(to) && addr_cmp(to, &dest))
                ++dont_reply;
            gid = *(address*)msg_getfield(mp, SYSFLD_GID, 1, (int*)0);
            gid.addr_entry = 0;
            cl_root = pg_find(pg_root, &dest);
            if(cl_root)
                qp = pg_find(cl_root->qu_queue, &gid);
            if(qp && ((pg = qp->qu_pg)->pg_flag&PG_CACHED))
                pg = 0;
            if(pg && pg->pg_viewid == *viewid)
            {
                if(dont_reply == 0 && (msg_getid(mp)&1) == 0)
                    REPLY(&from, &msg_getid(mp), &dest, mp, (char*)-1, 0, 0, 0, 0, 0, 0);
                return(0);
            }
            if(dont_reply == 0)
            {
                if(from.addr_site == my_site_no)
                    pg = 0;
                REPLY(&from, &msg_getid(mp), &dest, mp, (char*)-1, IE_AGAIN, 0, SYSFLD_TRUEVIEW, (char*)pg, FTYPE_PGROUP, pg? pglength(pg): 0);
            }
            return(-1);
        }
        return(0);
  }

#define VLEN    20

pg_verify_view(mp)
  register message *mp;
  {
        register verify **vi;
        verify *vis[VLEN];
        register address *ap, *dests = msg_getdests(mp);
        register qnode *qp;
        register nv = msg_getfields(mp, SYSFLD_VERIFY, (char**)vis, (int*)0, VLEN);
        vi = vis;
#       if (GB_DEBUG)
            print("pg_verify_view: found %d SYSFLD_VERIFY fields in message ", nv);
            pmsg(mp);
#       endif
        while(nv--)
        {
            /* Check members at this site */
            (*vi)->vi_gid.addr_entry = 0;
#           if (GB_DEBUG)
                print("pg_verify_view: viewid %d of ", (*vi)->vi_viewid);
                paddr(&(*vi)->vi_gid);
                print("at sites ");
                dump_bitv(&(*vi)->vi_sites);
                print("\n");
#           endif
            if(bit(&(*vi)->vi_sites, my_site_no))
            {
                register fnd = 0;
                for(ap = dests; !aptr_isnull(ap); ap++)
                    if(ap->addr_site == my_site_no)
                    {
                        if((qp = pg_find(pg_root, ap)) == 0)
                            continue;
                        if((qp = pg_find(qp->qu_queue, &(*vi)->vi_gid)) == 0)
                           continue;
                        if(qp->qu_pg->pg_flag&PG_CACHED)
                           continue;
                        ++fnd;
#                       if (GB_DEBUG)
		            print("... pg_verify_view: found viewid %d of ", qp->qu_pg->pg_viewid);
                            paddr(&qp->qu_pname);
                            print("\n");
#                       endif
                        if(qp->qu_pg->pg_viewid != (*vi)->vi_viewid)
                            return(GB_ABORT);
                        break;
                     }
                if(!fnd)
                {
#                   if (GB_DEBUG)
	                print("... pg_verify_view: didn't find group\n");
#                   endif
                    return(GB_ABORT);
                }
            }
#           if (GB_DEBUG)
            else
                print("... not at my site\n");
#           endif(GB_DEBUG)
            ++vi;
        }
#       if (GB_DEBUG)
	    print("... pg_verify_view: verified!\n");
#       endif
        return(0);
  }

pglist_rcv(req)
  register message *req;
  {
        register char *gname;
        register qnode *lroot, *pqp;
        register len = 0;
        
        gname = msg_getfield(req, GMGR_GNAME, 1, (int*)0);
        lroot = qu_null();
        for(pqp = pg_root->qu_next; pqp != pg_root; pqp = pqp->qu_next)
        {
            register qnode *cl_root = pqp->qu_queue, *qp, *lp;
            for(qp = cl_root->qu_next; qp != cl_root; qp = qp->qu_next)
            {
                register sys_groupview *pg = qp->qu_pg;
                if(pg->pg_flag&(PG_CACHED|PG_DELSENT))
                    continue;
                if(gname && *gname)
                {
                     register char *gp, *sp;
                     sp = pg->pg_name;
                     gp = gname;
                     while(*gp && *sp && *gp == *sp)
                        ++gp, ++sp;
                     if(*gp || *sp)
                        continue;
                }
                for(lp = lroot->qu_next; lp != lroot; lp = lp->qu_next)
                    if(addr_isequal(&lp->qu_pname, &pg->pg_gid))
                        break;
                if(lp == lroot)
                {
                    pg_add_sys_groupview(lroot, &pg->pg_gid, pg, nullroutine);
                    len += sizeof(gl_desc);
                }
            }
        }
        if(len == 0)
             reply(req, 0, 0, 0);
        else
        {
            gl_desc *rep;
            register gl_desc *rp;
            register qnode *lp;
            rep = rp = (gl_desc*)malloc(len += sizeof(gl_desc));
            for(lp = lroot->qu_next; lp != lroot; lp = lp->qu_next)
            {
                register sys_groupview *pg = lp->qu_pg;
                register address *ap;
                bcopy(pg->pg_name, rp->gl_name, PG_GLEN);
                rp->gl_viewid = pg->pg_viewid;
                rp->gl_nmembers = pg->pg_nmemb;
                rp->gl_nclients = pg->pg_nclient;
                rp->gl_addr = pg->pg_gid;
                bclr(&rp->gl_sites);
                for(ap = pg->pg_alist; !aptr_isnull(ap); ap++)
                    bis(&rp->gl_sites, ap->addr_site);
                ++rp;
            }
            rp->gl_addr = NULLADDRESS;
            reply(req, (char*)rep, FTYPE_GLDESC, len);
            free((char*)rep);
        }
        qu_freeall(lroot);
  }

#define CR_ABORTED      -1              /* Create was aborted */
#define CR_NULL         0               /* No create underway */
#define CR_CHECKING     1               /* Checking for other creates */
#define CR_WILLCREATE   2               /* Create will succeed */

static  pg_creating = CR_NULL;          /* Set if I am creating */
static  condition pg_waiting;           /* Waiting for creates to terminate */

address
pg_create(gname, alist, incarn)
  char *gname;
  address *alist;
  {
        register sys_groupview *pg;
        register message *mp;
        register nrep, glen;
        char answ[MAX_SITES];
        site_id slist[MAX_SITES];
        register char *ap;
        address gid, pg_local_lookup();
        char *scope;

        if(*gname == '@')
        {
            scope = ++gname;
            while(*gname && *gname != ':')
                ++gname;
            if(*gname)
                *gname++ = 0;
        }
        else
            scope = 0;
        if((glen = strlen(gname)) >= PG_GLEN)
            glen = PG_GLEN-1;
        gname[glen++] = 0;

        if(sl_scope(scope, slist) == 0)
            /* Doesn't include local site!  Illegal create request */
            return(NULLADDRESS);

        gid = pg_local_lookup(gname);
        if(gid.addr_site || *slist == 0)
            return(NULLADDRESS);

        while(pg_creating)
            t_wait(&pg_waiting, "pg_create");
        pg_creating = CR_CHECKING;
        mp = msg_genmsg(GMGR_GNAME, gname, FTYPE_CHAR, glen, 0);
        nrep = BCAST_SL(slist, PROTOCOLS, PR_CREATE_CHK, mp, ALL, collect_answ, answ, 1);
        msg_delete(mp);
        if(pg_creating == CR_ABORTED)
        {
            pg_creating = CR_NULL;
            t_sig_all(&pg_waiting, 0);
            return(NULLADDRESS);
        }
        for(ap = answ; ap < &answ[nrep]; ap++)
            if(*ap == '-')
            {
                pg_creating = CR_NULL;
                t_sig_all(&pg_waiting, 0);
                return(NULLADDRESS);
            }
        pg_creating = CR_WILLCREATE;
        pg = pg_alloc();
        gid = pg->pg_gid = make_gid();
        pg->pg_viewid = 1;
        pg->pg_incarn = incarn;
        begin
        {
            register n, in_clist = 0;
            for(n = 0; !aptr_isnull(&alist[n]) || in_clist++ == 0; n++)
            {
                pg->pg_alist[n] = alist[n];
                pg->pg_alist[n].addr_entry = 0;
                if(in_clist == 0)
                    pg->pg_nmemb++;
                else if(!aptr_isnull(&alist[n]))
                    pg->pg_nclient++;
            }
            pg->pg_alist[n] = NULLADDRESS;
        }
        if(gname)
            strncpy(pg->pg_name, gname, glen);
#       ifdef PG_DEBUG
        print("Creating group %s: ", pg->pg_name);
	paddr(&gid);
        print(", Members = "); paddrs(pg->pg_alist);
        if(pg->pg_nclient)
	    { print("["); paddrs(pg->pg_alist); print("]"); }
        print("\n");
#       endif PG_DEBUG
        mp = msg_genmsg(CL_NEWVIEW, (char*)pg, FTYPE_PGROUP, pglength(pg), 0);
        BCAST_SL(slist, PROTOCOLS, PR_ADD_GROUPVIEW, mp, ALL, collect_answ, answ, 1);
        pg_free(pg);
        pg_creating = CR_NULL;
        t_sig_all(&pg_waiting, 0);
        msg_delete(mp);
        return(gid);
  }

#define MAXTRUEVIEW     8

static  ntrueview;
static  qnode   *trueviews;

/* Used to tell a site that may not have heard what the "true" view is for a group */
pg_gottrueview(tpg)
  register sys_groupview *tpg;
  {
        register qnode *qp;
        register sys_groupview *pg;
        if(trueviews == 0)
            trueviews = qu_null();
        qp = pg_find(trueviews, &tpg->pg_gid);
        if(qp)
        {
            if(tpg->pg_viewid <= qp->qu_pg->pg_viewid)
                return;
            bcopy(tpg, qp->qu_pg, pglength(tpg));
            return;
        }
        else if(ntrueview == MAXTRUEVIEW)
        {
            register qnode *tq = qu_head(trueviews);
            qu_free(tq);
        }
        else
            ++ntrueview;
        pg = pg_alloc();
        bcopy(tpg, pg, pglength(tpg));
        pg_add_sys_groupview(trueviews, &pg->pg_gid, pg, pg_free);
  }

pg_add_groupview(mp)
  message *mp;
  {
        register sys_groupview *npg = (sys_groupview*)msg_getfield(mp, CL_NEWVIEW, 1, 0), *pg;
        register address *pp;
        for(pp = npg->pg_alist; pp != &npg->pg_alist[npg->pg_nmemb+npg->pg_nclient+1]; pp++)
        {
            register message *msg;
            register qnode *cl_root;
            int pg_failed();
            if(pp->addr_site != my_site_no || pp->addr_incarn != my_site_incarn)
                continue;
            if((cl_root = pg_find(pg_root, pp)) == 0)
                cl_root = pg_add_qu(pg_root, pp, qu_null());
            pg = pg_alloc();
            bcopy(npg, pg, pglength(npg));
            pg_add_sys_groupview(cl_root->qu_queue, &pg->pg_gid, pg, pg_free);
            if(check_proc_status(pp->addr_process, 0))
            {
                msg = msg_genmsg(CL_NEWVIEW, (char*)pg, FTYPE_PGROUP, pglength(pg), 0);
                pp->addr_entry = GENERIC_NEW_VIEW;
                msg_setdest(msg, pp);
                pp->addr_entry = 0;
		pr_local_send(pp->addr_process, msg);
		msg_delete(msg);
            }
            else
                t_fork(pg_failed, pp->addr_process, (message*)0);
        }
        reply(mp, "+", FTYPE_CHAR, 1);
  }

pgcreate_chk(mp)
  register message *mp;
  {
        register char *gname = msg_getfield(mp, GMGR_GNAME, 1, (int*)0);
        register cnt = 0;
        address addr, sender, pg_local_lookup();
        sender = *msg_getsender(mp);
        forever
        {
            addr = pg_local_lookup(gname);
            if(addr.addr_site || pg_creating == CR_WILLCREATE || ++cnt == 4)
            {
                reply(mp, "-", FTYPE_CHAR, 1);
                return;
            }
            if(pg_creating <= CR_NULL || (pg_creating == CR_CHECKING && sender.addr_site <= my_site_no))
            {
                if(pg_creating == CR_CHECKING && sender.addr_site != my_site_no)
                    pg_creating = CR_ABORTED;
                reply(mp, "+", FTYPE_CHAR, 1);
                return;
            }
            t_wait(&pg_waiting, "pg_create_chk");
        }
  }

address pg_lookup(gname)
  char *gname;
  {
        address addr, pg_local_lookup();
        site_id slist[MAX_SITES+1];
        message *mp;
        register nrep, glen;
        char *scope;

        EVENT(S_LOOKUP);
        if(*gname == '@')
        {
            scope = ++gname;
            while(*gname && *gname != ':')
                ++gname;
            if(*gname)
                *gname++ = 0;
        }
        else
            scope = 0;

        if((glen = strlen(gname)) >= PG_GLEN)
            glen = PG_GLEN-1;
        gname[glen++] = 0;
        if(sl_scope(scope, slist))
        {
            addr = pg_local_lookup(gname);
            if(addr.addr_site || current_view.sv_slist[1] == 0)
                return(addr);
        }
        if(*slist == 0)
            return(NULLADDRESS);
        mp = msg_genmsg(GMGR_GNAME, gname, FTYPE_CHAR, glen, 0);
        nrep = BCAST_SL(slist, PROTOCOLS, PR_PGLOOKUP, mp, 1, collect_answ, &addr, sizeof(address));
        msg_delete(mp);
        if(nrep && addr.addr_site)
            return(addr);
        return(NULLADDRESS);
  }

pglookup_rcv(mp)
  register message *mp;
  {
        register char *gname = msg_getfield(mp, GMGR_GNAME, 1, (int*)0);
        address addr, pg_local_lookup();
        
        addr = *msg_getsender(mp);
        addr = pg_local_lookup(gname);
        if(addr.addr_site)
            reply(mp, (char*)&addr, FTYPE_ADDRESS, sizeof(address));
        else 
            reply(mp, (char*)0, 0, 0);
  }

address pg_local_lookup(gname)
  char *gname;
  {
        register qnode *cl_root, *gp;

        for(cl_root = pg_root->qu_next; cl_root != pg_root; cl_root = cl_root->qu_next)
            for(gp = cl_root->qu_queue->qu_next; gp != cl_root->qu_queue; gp = gp->qu_next)
            {
                register sys_groupview *pg = gp->qu_pg;
                if((pg->pg_flag&PG_CACHED) == 0 && strcmp(pg->pg_name, gname) == 0)
                    return(gp->qu_pg->pg_gid);
            }
        return(NULLADDRESS);
  }

/* Case where a client failed: leave all groups */
pg_failed(pid)
  int pid;
  {
        address pname, addrs[2], died[2];
        register qnode *cl_root, *qp, *cp, *nqp;
        register message *mp;

        pname = ADDRESS(my_site_no, my_site_incarn, pid, 0);
  again:
        if((cl_root = pg_find(pg_root, &pname)) == 0)
            return;
        cp = cl_root->qu_queue;
        for(qp = cp->qu_next; qp != cp; qp = nqp)
        {
            register sys_groupview *pg = qp->qu_pg;
            nqp = qp->qu_next;
            if(pg->pg_flag&PG_CACHED)
            {
                if(pg->pg_ccount)
		{
                    pg->pg_flag |= PG_FAILED;
		    t_wait(&pg->pg_newview, "pg_failed");
		    goto again;
		}
#ifdef  PG_DEBUG
                print("cl_failed %d: delete cached view of ", pid);
	        paddr(&pg->pg_gid);
	        print("\n");
#endif
                qu_free(qp);
            }
            else if((pg->pg_flag&PG_DELSENT) == 0)
            {
                char answ[MAX_PROCS];
                if(pg->pg_ccount)
                    /* Wait until the iteration terminates */
                    if(t_wait(&pg->pg_bwait, "bwait") < 0)
                        goto again;
                pg->pg_flag |= PG_DELSENT;
                addrs[0] = pg->pg_gid;
                addrs[0].addr_entry = GENERIC_DELETE;
                addrs[1] = NULLADDRESS;
                died[0] = pname;
                died[1] = NULLADDRESS;
                mp = msg_genmsg(CL_PNAME, (char*)died, FTYPE_ADDRESS, sizeof(died), 0);
		msg_insertfield(mp, CL_GID, (char*)&pg->pg_gid, FTYPE_ADDRESS, sizeof(address));
                msg_insertfield(mp, SYSFLD_VCHANGE, (char*)0, FTYPE_CHAR, 0);
                msg_setsender(mp, &pname);
                msg_setreplyto(mp, &my_address);
#ifdef  PG_DEBUG
	        print("cl_failed %d: gbcast delete for ", pid);
	        paddr(&pg->pg_gid);
	        print("\n");
#endif
                GBCAST(addrs, mp, ALL, collect_answ, answ, 1);
                msg_delete(mp);
                goto again;
            }
        }
        if(cl_root->qu_queue->qu_next == cl_root->qu_queue)
	{
#ifdef  PG_DEBUG
            print("cl_failed %d: delete root node\n", pid);
#endif
            qu_free(cl_root);
	}
  }

static  update_is_running;

/* Case where a whole site failed */
pg_newview()
  {
        int pg_update_pgviews();
        if(update_is_running++)
            return;
        t_fork(pg_update_pgviews, 0, (message*)0);
  }

pg_update_pgviews()
  {
        static address addrs[2], died[2];
        register qnode *cl_root, *qp;
        register message *mp;
        register sys_groupview *pg;
        register address *ap, *sp;

  again:
        for(cl_root = pg_root->qu_next; cl_root != pg_root; cl_root = cl_root->qu_next)
            /* For each client, look at views to which it belongs */
            for(qp = cl_root->qu_queue->qu_next; qp != cl_root->qu_queue; qp = qp->qu_next)
            {
                char answ[MAX_PROCS];

                pg = qp->qu_pg;

                if(pg->pg_ccount)
                    if(t_wait(&pg->pg_rwait, "pg_update") < 0)
                        goto again;
                if(pg->pg_flag&PG_CACHED)
                    continue;

                /* Find the oldest operational member or client */
                for(sp = pg->pg_alist; sp != &pg->pg_alist[pg->pg_nmemb+pg->pg_nclient+1]; sp++)
                    if(sp->addr_site && SITE_IS_UP(sp->addr_site, sp->addr_incarn))
                        break;
                /* Is this client the the oldest member? */
                if(addr_cmp(sp, &cl_root->qu_pname))
                    continue;
                /* Yes, send messages on behalf of dead members and clients */
                for(ap = pg->pg_alist; ap != &pg->pg_alist[pg->pg_nmemb+pg->pg_nclient+1]; ap++)
                    if(ap->addr_site && SITE_IS_UP(ap->addr_site, ap->addr_incarn) == 0)
                    {
                        died[0] = *ap;
                        addrs[0] = pg->pg_gid;
                        addrs[0].addr_entry = GENERIC_DELETE;
                        mp = msg_genmsg(CL_PNAME, (char*)died, FTYPE_ADDRESS, sizeof(address)*2, 0);
			msg_insertfield(mp, CL_GID, (char*)&pg->pg_gid, FTYPE_ADDRESS, sizeof(address));
                        msg_setsender(mp, sp);
                        msg_setreplyto(mp, &my_address);
                        msg_insertfield(mp, SYSFLD_VCHANGE, (char*)0, FTYPE_CHAR, 0);
                        GBCAST(addrs, mp, ALL, collect_answ, answ, 1);
                        msg_delete(mp);
                        goto again;
                    }
        }
        if(update_is_running)
	{
	    update_is_running = 0;
            goto again;
	}
  }

/* Client wants to know the current view of some group */
cl_getview(mp)
  message *mp;
  {
        register qnode *cl_root, *qp = 0;
        register sys_groupview *pg;
        qnode *cview_refresh();
        address gid, sender;

        gid = *(address*)msg_getfield(mp, CL_GID, 1, (int*)0);
        if(aptr_isnull(&gid))
            goto done;
        /* Check first for a non-cached copy */
        sender = *msg_getsender(mp);
        if(cl_root = pg_find(pg_root, &sender))
            if(qp = pg_find(cl_root->qu_queue, &gid))
                if((qp->qu_pg->pg_flag&PG_CACHED) == 0)
                {
                    reply(mp, (char*)qp->qu_pg, FTYPE_PGROUP, sizeof(sys_groupview));
                    return;
                }
        /* Get a fresh cached copy */
        if(cl_root == 0)
            cl_root = pg_add_qu(pg_root, &sender, qu_null());
  again:
        if(qp = pg_find(cl_root->qu_queue, &gid))
            pg = qp->qu_pg;
        else
            pg = 0;
        if((pg && pg->pg_ccount) || (qp = cview_refresh(sender, cl_root, gid, 0)))
        {
	    pg = qp->qu_pg;
            if(pg->pg_ccount)
                /* Wait until the iteration terminates */
                if(t_wait(&pg->pg_bwait, "bwait") < 0)
                    goto again;
            reply(mp, (char*)qp->qu_pg, FTYPE_PGROUP, sizeof(sys_groupview));
            return;
        }
  done:
        reply(mp, (char*)0, 0, 0);
  }

/*
 * Wait for the processes that died to drop out of the groups to which this process belongs
 * Called from collect_replies
 */
pg_wait(client, alist, died)
  address client;
  register address *alist;
  register char *died;
  {
        register n;
        register qnode *cl_root;

        for(n = 0; n < ADDR_LEN && !aptr_isnull(&alist[n]); n++)
            if(died[n])
                break;
        if(died[n] == 0)
            return;
        while(n < ADDR_LEN && !aptr_isnull(&alist[n]))
        {
            if(died[n])
            {
                register qnode *ap;
                /* process alist[n] died during protocol */
            again:
                if((cl_root = pg_find(pg_root, &client)) == 0)
                    return;
                for(ap = cl_root->qu_queue->qu_next; ap != cl_root->qu_queue; ap = ap->qu_next)
                {
                    register sys_groupview *pg;
                    register address *mp;
                    if(((pg = ap->qu_pg)->pg_flag&PG_CACHED) == 0)
                    {
                        /* Accurate view */
                        for(mp = pg->pg_alist; !aptr_isnull(mp); mp++)
                            if(addr_cmp(mp, &alist[n]) == 0)
                            {
                                /* Wait for a new view */
                                t_wait(&pg->pg_newview, "pg_newview");
                                if((cl_root = pg_find(pg_root, &client)) == 0)
                                    return;
                                goto again;
                            }
                    }
                }
            }
            n++;
        }
  }

/*
 * Spy on membership related messages to the process
 * Returns a freshly allocated message to substitute
 * for the one it grabbed.
 */
message *
gmgr_spy(pname, mp, cl_known)
  register message *mp;
  address pname;
  {
        register entry, nmemb, nclient;
        address gid, addr, atemp;
        register address *ap, *bp, *memb;
        register qnode *cl_root, *gp = 0;
        register sys_groupview *pg, *pgcont;
        int pglen;

        entry = pname.addr_entry;
        pname.addr_entry = 0;

        gid = *(address*)msg_getfield(mp, CL_GID, 1, (int*)0);
        if(cl_root = pg_find(pg_root, &pname))
            gp = pg_find(cl_root->qu_queue, &gid);
        if(entry == GENERIC_SIGNAL)
        {
            char *answ = "-";
            addr = *msg_getsender(mp);
            if(gp)
               for(ap = gp->qu_pg->pg_alist; !aptr_isnull(ap); ap++)
                    if(addr_cmp(ap, &addr) == 0)
                    {
                        register signo = *(int*)msg_getfield(mp, CL_SIGNO, 1, (int*)0);
                        kill(pname.addr_process, signo);
                        answ = "+";
                        break;
                    }
            reply_as(&pname, mp, answ, FTYPE_CHAR, 1);
            return((message*)0);
        }
        memb = (address*)msg_getfield(mp, CL_PNAME, 1, (int*)0);
#ifdef  PG_DEBUG
        print("gmgr_spy: "); pmsg(mp);
#endif
        if(entry == GENERIC_DELETE && gp == 0)
        {
            reply_as(&pname, mp, (char*)0, 0, 0);
            gb_unlock(mp, 0, pname.addr_process);
            return((message*)0);
        }
        pgcont = (sys_groupview*)msg_getfield(mp, SYSFLD_PGVIEW, 1, &pglen);
        if(gp)
        {
            pg = gp->qu_pg;
            if((pg->pg_flag&PG_CACHED) && pgcont)
            {
                /*
                 *  This is a little tricky.  Can't wait for iterated protocols to
                 *  finish or a deadlock can result.  Cross fingers and hope for best...
                 *  In principle, can infer that all succeeded because, after all, the
                 *  join obviously did.  But, application-level multitasking could
                 *  presumably result in extra broadcasts after the join and those
                 *  will screw up now (fifo or cbcast ordering could be violated)
                 */
                if(entry != GENERIC_ADDMEMB && entry != GENERIC_ADDCLIENT)
                     panic("entry %d in spy cached case", entry);
                bcopy(pgcont, pg, pglen);
                pg->pg_flag &= ~PG_CACHED;
            }
            else switch(entry)
            {
              case GENERIC_DELETE:
                if(memb->addr_site != 0xFF)
                {
                     for(ap = pg->pg_alist; ap != &pg->pg_alist[pg->pg_nmemb+pg->pg_nclient]; ap++)
                     {
                         register address *dp;
                         for(dp = memb; !aptr_isnull(dp); dp++)
                            if(addr_isequal(ap, dp))
                                break;
                        if(!aptr_isnull(dp))
                            break;
                     }
                     if(aptr_isnull(ap))
                        goto no_change;
                }
                break;
              case GENERIC_ADDMEMB:
              case GENERIC_ADDCLIENT:
                for(ap = pg->pg_alist; !aptr_isnull(ap); ap++)
                    if(addr_isequal(ap, memb))
                        break;
                if(!aptr_isnull(ap))
                    goto no_change;
                if(entry == GENERIC_ADDMEMB)
                    break;
                for(++ap; !aptr_isnull(ap); ap++)
                    if(addr_isequal(ap, memb))
                        break;
                if(aptr_isnull(ap))
                    break;
              no_change:
#		if (PG_DEBUG)
                    print("gmgr_spy: IGNORING a view change that had no effect!\n");
                    print("membs = "); paddrs(pg->pg_alist);
                    print("proc = "); paddr(memb);
                    print("msg_id = %x, ", *(int*)msg_getfield(mp, SYSFLD_PROTID, 1, (int*)0));
                    pmsg(mp);
#               endif
                reply_as(&pname, mp, "+", FTYPE_CHAR, 1);
                gb_unlock(mp, 0, pname.addr_process);
                return((message*)0);
            }
        }
        else
        {
            /* Copy view from piggybacked one, if it was unknown */
            if(addr_cmp(&pname, memb) && entry != GENERIC_DELETE)
            {
                message *msg = msg_newmsg(); char answ[MAX_SITES];
                print("GMGR spy: group was unknown to pid %d...  ", pname.addr_process);
                print("req %s ", entry == GENERIC_ADDMEMB? "add": "del"); paddr(memb);
                print("\n");
                pmsg(mp);
                panic("gmgr_spy: group should have been known");
            }
            if(cl_root == 0)
                cl_root = pg_add_qu(pg_root, &pname, qu_null());
            pg = pg_alloc();
            bcopy(pgcont, pg, pglen);
            gp = pg_add_sys_groupview(cl_root->qu_queue, &gid, pg, pg_free);
        }

        /* If <memb> is already listed, delete duplicated entry */
        bp = pg->pg_alist;
        if(memb->addr_site == 0xFF)
            nmemb = nclient = 0;
        else
        {
            /* Delete might have sent a vector of addresses */
            nmemb = pg->pg_nmemb;
            nclient = pg->pg_nclient;
            /* Copy members */
            for(ap = pg->pg_alist; !aptr_isnull(ap); ap++)
            {
                register address *dp;
                for(dp = memb; !aptr_isnull(dp); dp++)
                    if(addr_cmp(ap, dp) == 0)
                        break;
                if(aptr_isnull(dp))
                    *bp++ = *ap;
                else 
                    --nmemb;
            }
            /* Copy clients */
            for(*bp++ = *ap++; !aptr_isnull(ap); ap++)
            {
                register address *dp;
                for(dp = memb; !aptr_isnull(dp); dp++)
                    if(addr_cmp(ap, dp) == 0)
                        break;
                if(aptr_isnull(dp))
                    *bp++ = *ap;
                else
                    --nclient;
            }
            *bp = NULLADDRESS;

            /* Add a new member */
            if(entry == GENERIC_ADDMEMB)
            {
                ap = &pg->pg_alist[nmemb];
                addr = *memb;
                do
                {
                    atemp = *ap;
                    *ap++ = addr;
                    addr = atemp;
                }
                while(!aptr_isnull(ap));
                *ap++ = addr;
                *ap = NULLADDRESS;
                ++nmemb;
            }
            else if(entry == GENERIC_ADDCLIENT)
            {
                ap = &pg->pg_alist[nmemb+nclient+1];
                *ap++ = *memb;
                *ap = NULLADDRESS;
                ++nclient;
            }
        }
        EVENT(S_VCHANGE);
        ++pg->pg_viewid;
        pg->pg_nclient = nclient;
        pg->pg_nmemb = nmemb;
        if(nmemb && entry == GENERIC_DELETE)
        {
            register address *dp;
            for(dp = memb; !aptr_isnull(dp); dp++)
                if(addr_cmp(&pname, dp) == 0)
                    nmemb = 0;
        }
        if(nmemb == 0)
        {
            /* Deleted entire sys_groupview */
            pg_sig_all(pg, -1);
            qu_free(gp);
            pg = 0;
            if(cl_root->qu_queue->qu_next == cl_root->qu_queue)
                qu_free(cl_root);
        }
        else if(pg->pg_nmemb+pg->pg_nclient >= PG_ALEN-2)
            panic("clist overflowed");
        else
            while(pg->pg_newview)
                t_sig(&pg->pg_newview, 0);
        reply_as(&pname, mp, "+", FTYPE_CHAR, 1);
#ifdef  PG_DEBUG
        print("Client "); paddr(&pname);
        print(", Group "); paddr(&gid);
        if(nmemb)
        {
            print(", Members = "); paddrs(pg->pg_alist);
            if(nclient)
                { print("["); paddrs(pg->pg_alist[nmemb]); print("]"); }
            print("\n");
        }
        else
            print(", *** deleted ***\n");
#endif  PG_DEBUG
        gb_unlock(mp, 0, pname.addr_process);
        if(cl_known != -1)
        {
            /* Pass the local process a copy of the view */
            if(pg)
            {
                mp = msg_genmsg(CL_NEWVIEW, (char*)pg, FTYPE_PGROUP, pglength(pg), 0);
                pname.addr_entry = GENERIC_NEW_VIEW;
            }
            else
            {
                mp = msg_genmsg(CL_GID, (char*)&gid, FTYPE_ADDRESS, sizeof(address), 0);
                pname.addr_entry = GENERIC_DEL_PGROUP;
            }
            msg_setdest(mp, &pname);
            if(entry != GENERIC_DELETE && SITE_IS_UP(memb->addr_site, memb->addr_incarn) == 0 && update_is_running++ == 0)
                /* Late termination of an ADDMEMB protocol */
                t_fork(pg_update_pgviews, 0, (message*)0);
            return(mp);
        }

        /* Falls through if the process has failed */
        if(entry != GENERIC_DELETE && addr_cmp(&pname, memb) == 0)
	    /* Added self -> initiate delete action */
	    t_fork(pg_failed, (int)pname.addr_process, (message*)0);
        return((message*)0);
  }

pg_sig_all(pg, val)
  register sys_groupview *pg;
  {
        t_sig_all(&pg->pg_hipri, val);
        t_sig_all(&pg->pg_lopri, val);
        t_sig_all(&pg->pg_newview, val);
        t_sig_all(&pg->pg_bwait, val);
        t_sig_all(&pg->pg_rwait, val);
  }

/* Look up the desired sys_groupview view */
sys_groupview *
view_get(qp, gid)
  register qnode *qp;
  address gid;
  {
        register sys_groupview *pg;
        gid.addr_entry = 0;
  again:
        if(qp = pg_find(qp, &gid))
        {
            pg = qp->qu_pg;
            if((pg->pg_flag&PG_CACHED) == 0 && pg->pg_ccount == 0)
            {
                if(pg->pg_ccount)
                    /* Wait until the iterated protocols finish */
                    if(t_wait(&pg->pg_bwait, "pg_bwait") < 0)
                        goto again;
                return(pg);
            }
        }
        return((sys_groupview*)0);
  }

/*
 *  This client is not a member, so used a cached sys_groupview view instead
 *  If called at all, always called by a process that holds no lock.
 */
sys_groupview *
cache_get(sender, gid, msg)
  address sender, gid;
  message *msg;
  {
        register qnode *cp, *cl_root;
        qnode *cview_refresh();
        register sys_groupview *pg;

        gid.addr_entry = 0;
  again:
        /* Make sure the client is listed */
        if((cl_root = pg_find(pg_root, &sender)) == 0)
            cl_root = pg_add_qu(pg_root, &sender, qu_null());
        if((cp = pg_find(cl_root->qu_queue, &gid)) == 0 && (cp = cview_refresh(sender, cl_root, gid, 0)) == 0)
            return((sys_groupview*)0);

        /* cp and pg now point to the cache entry.  Wait if it is being refreshed */
	pg = cp->qu_pg;
        while(pg->pg_flag&PG_REFRESH)
        {
            switch(t_wait(&pg->pg_lopri, "lopri"))
            {
              case -1:
                /* Got deleted because nmembs dropped to 0 */
                return((sys_groupview*)0);

              case -2:
                /* Must switch to a different copy */
                goto again;
            }
        }
        if(pg->pg_flag&PG_CACHED)
        {
            msg_replacefield(msg, SYSFLD_VIEWID, (char*)&pg->pg_viewid, FTYPE_LONG, sizeof(int));
            msg_replacefield(msg, SYSFLD_GID, (char*)&gid, FTYPE_ADDRESS, sizeof(address));
            ++pg->pg_ccount;
        }
	else
	    if(pg->pg_ccount)
		t_wait(&pg->pg_bwait, "get view: waiting for iteration to finish");
        return(pg);
  }


/* Get a cached view, replacing the one thats there if any */
qnode *
cview_refresh(sender, cl_root, gid, oldviewid)
  register qnode *cl_root;
  address sender, gid;
  {
        message *msg;
        register count;
        register sys_groupview *pg;
        register qnode *cp, *cqp, *qp, *nqp;
        for(cqp = pg_root->qu_next; cqp != pg_root; cqp = cqp->qu_next)
            if((qp = pg_find(cqp->qu_queue, &gid)) && ((pg = qp->qu_pg)->pg_flag&PG_CACHED) == 0)
            {
                /* Found a presumably accurate copy */
                cqp = cl_root->qu_queue;
                cp = pg_find(cl_root->qu_queue, &gid);
                if(cp == 0)
                {
                    cp = pg_add_sys_groupview(cqp, &pg->pg_gid, pg_alloc(), pg_free);
                    bcopy((char*)pg, (char*)cp->qu_pg, pglength(pg));
                    cp->qu_pg->pg_flag |= PG_CACHED;
                }
                else if(cp->qu_pg->pg_flag&PG_CACHED)
                    bcopy((char*)pg, (char*)cp->qu_pg, pglength(pg));
                /* Check for entries with same gname but old gid */
                for(qp = cqp->qu_next; qp != cqp; qp = nqp)
                {
                    nqp = qp->qu_next;
                    if(qp != cp && strcmp(qp->qu_pg->pg_name, pg->pg_name) == 0 && qp->qu_pg->pg_ccount == 0)
                        qu_free(qp);
                }
                return(cp);
            }

        EVENT(S_CFAULT);

        /* Next idea... check ``trueviews'' cache for something with a fresher viewid */
        qp = trueviews? pg_find(trueviews, &gid): 0;
        if(qp && qp->qu_pg->pg_viewid > oldviewid)
        {
            /* Pretend I just got this when doing a BCAST */
            pg = qp->qu_pg;
            qp->qu_pg = 0;
            qu_free(qp);
            --ntrueview;
            count = 1;
            goto found;
        }

        /* Didn't find anything locally.  Request it from abroad */
        msg = msg_genmsg(ADDR_GID, (char*)&gid, FTYPE_ADDRESS, sizeof(gid), 0);
        pg = pg_alloc();
        count = 0;
        cp = pg_find(cl_root->qu_queue, &gid);
        if(cp)
        {
            address alist[8];
            site_id slist[2];
            register a, n;
            register sys_groupview *spg = cp->qu_pg;
            a = n = 0;
            while(n < spg->pg_nmemb && n < 8 && a < spg->pg_nmemb)
                if(spg->pg_alist[a++].addr_site == my_site_no)
                    alist[n++] = spg->pg_alist[a-1];
            a = n = 0;
            while(n < spg->pg_nmemb && n < 8 && a < spg->pg_nmemb)
                if(spg->pg_alist[a++].addr_site != my_site_no)
                    alist[n++] = spg->pg_alist[a-1];
            slist[1] = 0;
            while(n--)
            {
                slist[0] = MAKE_SITE_ID(alist[n].addr_site, alist[n].addr_incarn);
                if((count = BCAST_SL(slist, PROTOCOLS, PR_PGWANTVIEW, msg, 1, collect_answ, (char*)pg, sizeof(sys_groupview))) == 1)
                    break;
            }
        }
        if(count == 0)
            /* As a last resort try everyone */
            count = BCAST_V(&current_view, PROTOCOLS, PR_PGWANTVIEW, msg, 1, collect_answ, (char*)pg, sizeof(sys_groupview));
        msg_delete(msg);
        if((cl_root = pg_find(pg_root, &sender)) == 0 || check_proc_status(sender.addr_process, 0) == 0)
        {
            pg_free(pg);
            return(0);
        }

  found:
        cp = pg_find(cl_root->qu_queue, &gid);
        if(count == 1)
        {
            if(cp == 0)
                cp = pg_add_sys_groupview(cl_root->qu_queue, &gid, pg, pg_free);
            else if(cp->qu_pg->pg_flag&PG_CACHED)
            {
                bcopy((char*)pg, (char*)cp->qu_pg, pglength(pg));
                pg_free(pg);
            }
            cp->qu_pg->pg_flag |= PG_CACHED;
        }
        else
        {
            pg_free(pg);
            if(cp && cp->qu_pg->pg_ccount == 0)
            {
                qu_free(cp);
                cp = 0;
            }
        }
        return(cp);
  }

/* View unknown remotely... check locally */
pgwantview_rcv(msg)
  message *msg;
  {
        address *gid = (address*)msg_getfield(msg, ADDR_GID, 1, (int*)0);
        register qnode *cl_root, *cp;
        gid->addr_entry = 0;
        for(cl_root = pg_root->qu_next; cl_root != pg_root; cl_root = cl_root->qu_next)
            if(cp = pg_find(cl_root->qu_queue, gid))
            {
                register sys_groupview *pg = cp->qu_pg;
                if(pg->pg_flag&PG_CACHED)
                    continue;
                reply(msg, (char*)pg, FTYPE_PGROUP, pglength(pg));
                return;       
            }
        reply(msg, (char*)0, 0, 0);
  }

/* After iteration, refresh cache entry if necessary */
cache_refresh(msg, nresp)
  register nresp;
  register message *msg;
  {
        static address addrs[2];
        address sender, *gid;
        register qnode *cl_root, *cp;
        register sys_groupview *pg;

        sender = *msg_getsender(msg);
        if((cl_root = pg_find(pg_root, &sender)) == (qnode*)0)
            return;
        gid = (address*)msg_getfield(msg, SYSFLD_GID, 1, (int*)0);
        if((cp = pg_find(cl_root->qu_queue, gid)) == (qnode*)0)
            return;
        pg = cp->qu_pg;
	if(pg->pg_ccount == 0)
	    return;
	else if(pg->pg_ccount == 1)
	{
	    /* Case where process failed while broadcast was underway */
	    if(pg->pg_flag&PG_FAILED)
	    {
	        --pg->pg_ccount;
		pg->pg_flag &= ~PG_FAILED;
	        pg_sig_all(pg, -2);
	        return;
	    }
	    t_sig_all(&pg->pg_rwait, 0);
	    if((pg->pg_flag&PG_REFRESH) == 0)
	        t_sig_all(&pg->pg_bwait, 0);
	}
        if(nresp != IE_AGAIN)
        {
	     --pg->pg_ccount;
	     return;
	}
        /* Must iterate this broadcast */
        if((pg->pg_flag&PG_REFRESH) == 0)
        {
            register vid;
            qnode *qp;
            /* This means I am in charge of fixing the bad entry */
            pg->pg_flag |= PG_REFRESH;
            vid = pg->pg_viewid;
            /* Count is non-zero hence cview_refresh won't delete pg pointer */
            if(qp = cview_refresh(sender, cl_root, *gid, vid))
                cp = qp; 
            /* Wait for everyone to pile up waiting at hipri */
            if(--pg->pg_ccount > 0)
                if(t_wait(&pg->pg_rwait, "pg_rwait") < 0)
                    return;

            if(pg->pg_viewid == vid)
            {
                /* Nobody knew about this entry! */
                pg_sig_all(pg, -2);
                qu_free(cp);
                return;
            }
            pg->pg_flag &= ~PG_REFRESH;
            pg_sig_all(pg, 0);
        }
        else
        {
            /* Wait for it to be fixed */
            --pg->pg_ccount;
            (void)t_wait(&pg->pg_hipri, "hipri");
        }
  }
