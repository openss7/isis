/*  $RCSfile: pr_client.c,v $ $Revision: 2.26 $ $Date: 90/09/18 14:33:26 $  */
/*
 *	Originally coded by Ken Birman
 *
 *      Connected stream code for packet transmission from ISIS to client
 *      Performance considerations lead to a somewhat complicated
 *      protocol.
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

# include "pr.h"
#ifdef UNIX_DOM
# include <sys/un.h>
#endif

struct  hostent *gethostbyname();
struct  servent *getservbyname();

static  connect_socket;
static  CLIENT_PORT;               /* Overrides default if non-zero */

typedef struct cldesc
{
        short   cl_flag;        /* Set if the client is unable to accept data */
        short   cl_pid;         /* Process id of this client */
        address cl_paddr;       /* Address of this client */
        int     cl_queuelen;    /* Number of qnoded bytes when congested */
        qnode   *cl_msgqueue;   /* The congestion qnode */
        qnode   *cl_pmonitor;   /* Remote processes this client is monitoring */
        message *cl_curmsg;     /* Message being transmitted now */
        message *cl_mp;         /* Message most recently received */
        block_desc *cl_blkdesc; /* Message being received now, if CLIENT_READING set */
        int     cl_rlen;        /* Remaining length of message being read */
        char    *cl_rptr;       /* Pointer into message read area */
        iovec   *cl_curiov;     /* copy of the iovec */
        iovec   *cl_iovptr;     /* ptr to current iovec entry */
        int     cl_iovlen;      /* Remaining iovec len */
        int     cl_nbytes;      /* Remaining bytes to send */
        int     cl_lastact;     /* Time when last active */
	int	cl_pdelay;	/* Probe timeout (0 if disabled) */
        bitvec  cl_watchedby;   /* Sites watching this client */
} cldesc;

typedef struct remote
{	short	rc_pid;
	bitvec	rc_watchedby;
} remote;

#define CLIENT_BUSY         0x01    /* Client is busy */
#define CLIENT_READING      0x02    /* Reading a message from this client */
#define CLIENT_PMONITOR     0x04    /* Client is monitoring one or more remote processes */
#define CLIENT_MALLOC       0x08    /* Client is monitoring one or more remote processes */
#define CLIENT_OVERFLOW     0x10    /* Printed channel overflow message */
#define CLIENT_PROBING      0x20    /* Probe in progress */

#define MAXREMOTE           64

static cldesc   cl[MAXBITS];	 /* Current local clients */
static remote   rc[MAXREMOTE];	 /* Current remote clients */
static sd_isis;

typedef struct
{
        int     fi_pid;
        bitvec  fi_watchedby;
} failinfo;

adesc fi_ad ={ sizeof(failinfo), 0, 4 };

#define fi_alloc()   ((failinfo*)mallocate(&fi_ad))
#define fi_free(fip)  mdeallocate((char*)fip, &fi_ad)

client_init()
  {
        static saddr my_addr;
#ifdef  UNIX_DOM
        struct sockaddr_un ux_addr;
#endif

        /* Create a TCP socket to listen on */
#ifdef  UNIX_DOM
        if((connect_socket = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
        {
            perror("socket");
            panic("client_init");
        }
        connect_bit = connect_socket;
        bis(&input_mask, connect_socket);
        if(CLIENT_PORT == 0)
            CLIENT_PORT = connect_port[my_site_no];
        if(CLIENT_PORT == 0)
        {
            register struct servent *sp = getservbyname("isis", "tcp");
            if(sp == (struct servent*)0)
                panic("isis.*: service not listed in /etc/services on this host");
            CLIENT_PORT = ntohs(sp->s_port);
        }
        ux_addr.sun_family = AF_UNIX;
        sprintf(ux_addr.sun_path, "/tmp/Is%d", CLIENT_PORT);
        unlink(ux_addr.sun_path);
        if(bind(connect_socket, (struct sockaddr*)&ux_addr, 
           strlen(ux_addr.sun_path) + sizeof(ux_addr.sun_family)) == -1)
        {
            perror("bind");
            panic("client_init unable to create <%s>.  Is ISIS already running?\n", ux_addr.sun_path);
        }
	chmod(ux_addr.sun_path, 0x666);
#else   UNIX_DOM
        if((connect_socket = socket(AF_INET, SOCK_STREAM, 0)) == -1)
        {
            perror("socket");
            panic("client_init");
        }
        connect_bit = connect_socket;
        bis(&input_mask, connect_socket);
        if(CLIENT_PORT == 0)
            CLIENT_PORT = connect_port[my_site_no];
        if(CLIENT_PORT == 0)
        {
            register struct servent *sp = getservbyname("isis", "tcp");
            if(sp == (struct servent*)0)
                panic("isis.*: service not listed in /etc/services on this host");
            CLIENT_PORT = ntohs(sp->s_port);
        }
        my_addr.sin_family = AF_INET;
        if(connect_port[my_site_no])
            my_addr.sin_port = htons(connect_port[my_site_no]);
        else
            my_addr.sin_port = htons(CLIENT_PORT);
        if(bind(connect_socket, (struct sockaddr*)&my_addr, sizeof(my_addr)) == -1)
        {
            perror("bind connect_socket");
            panic("client_init unable to acquire port %d.  Is ISIS already running?\n", ntohs(my_addr.sin_port));
        }
#endif  UNIX_DOM
        listen(connect_socket, 5);
  }

static cinfo ci;

/* Select shows that we should be able to do an accept now */
client_accept()
  {
        register snum = accept(connect_socket, (struct sockaddr *)0, (int*)0);
        register message *msg;
        register longvec[5];
	extern ISIS_PORTNO, nsites;

        if(snum == -1)
        {
            perror("accept");
            panic("accept failed! max_cl %d", max_cl);
        }
        bis(&input_mask, snum);
        if(snum > max_cl)
            max_cl = snum;
        if(cl[snum].cl_pid)
            client_failed(snum, FALSE);
#       if (SUN)
            setsockopt(snum, SOL_SOCKET, SO_DONTLINGER, (char*)0, (int*)0);
#       endif
        if(snum >= MAXBITS)
            panic("socket_no > MAXBITS");
        fcntl(snum, F_SETFL, FNDELAY);

        if(snum == 0 || snum > 127)
            panic("Unexpected fdes %d in client_accept", snum);

        /* Had to break out struct ci into fields - tclark 7/23/90  */
        longvec[0] = my_site_no;
        longvec[1] = my_site_incarn;
        longvec[2] = my_site_id;
        longvec[3] = snum;
        longvec[4] = ISIS_PORTNO;
        msg = msg_gen("%D, %s", longvec, 5, isis_dir);

        msg_insertref(msg, FLD_ANSW, (char*)&current_view, FTYPE_SVIEW, sizeof(current_view), (vfunc*) nullroutine);
        msg_insertref(msg, FLD_ANSW, (char*)site_names, FTYPE_CHAR, (nsites+1)*64, (vfunc*) nullroutine);

        if(tcp_write(snum, msg, 1) == -1)
        {
            close(snum);
            bic(&input_mask, snum);
        }
        msg_delete(msg);
  }

/* Read a message from a local file descriptor */
pr_local_rcv(sd)
 {
        register cldesc *cp = &cl[sd];
        register message *mp;
        register long *space;
        register nb, eof = 1;

        if((cp->cl_flag&CLIENT_READING) == 0)
        {
            cp->cl_rlen = 0;
            nb = read(sd, (char*)&cp->cl_rlen, sizeof(long));
            if(nb != sizeof(long))
                goto bad;
            eof = 0;
            cp->cl_rlen = ntohl (cp->cl_rlen);
            cp->cl_blkdesc = msg_malloc (cp->cl_rlen);
            cp->cl_flag |= CLIENT_READING;
            space = (long *) msg_body(cp->cl_blkdesc);
            space[0] = htonl (cp->cl_rlen);
            cp->cl_rptr = (char*)&space[1];
            cp->cl_rlen -= sizeof(long);
	    cp->cl_blkdesc->blk_avail -= sizeof(long);
        }
        else
            space = (long *) msg_body(cp->cl_blkdesc);
        while(cp->cl_rlen)
        {
            nb = read(sd, cp->cl_rptr, cp->cl_rlen);
            if(nb <= 0)
            {
                if(nb == 0 && eof)
                    goto bad;
                if(nb == 0 || (nb == -1 && errno == EWOULDBLOCK))
                    return;
                goto bad;
            }
            eof = 0;
	    cp->cl_blkdesc->blk_avail -= nb;
            cp->cl_rlen -= nb;
            cp->cl_rptr += nb;
        }
        if(cp->cl_pid && cp->cl_pid != ISIS && cp->cl_paddr.addr_site == 0)
            /* He was too quick */ 
            goto bad;
        if(mp = msg_reconstruct(cp->cl_blkdesc))
        {
            EVENT(S_SYSCALLS);
#	    if (CL_DEBUG)
	        print("rcv from client %d: ", cp->cl_pid); pmsg(mp);
#	    endif
            if(cp->cl_pid > 0)
            {
                address sender;
                sender = cp->cl_paddr;
                sender.addr_entry = msg_getsender(mp)->addr_entry;
                msg_setsender(mp, &sender);
            }
            if(msg_tracemsgs)
            {
                int pno = PN_CLIENT;
                msg_replacefield(mp, SYSFLD_PROTO, (char*)&pno, FTYPE_LONG, sizeof(int));
            }
            cp->cl_flag &= ~CLIENT_READING;
            cp->cl_blkdesc = 0;
            cp->cl_rlen = 0;
            cp->cl_rptr = 0;
            begin
            {
                register cldesc *ccp;
                /* Inefficient, but all I could come up with! */
                for(ccp = cl; ccp <= &cl[max_cl]; ccp++)
                    if(ccp->cl_mp == mp)
                        ccp->cl_mp = 0;
                cp->cl_mp = mp;
            }
            pr_local_delivery(mp, (int*)0);
            msg_delete(mp);
            return;
        }
  bad:
        client_failed(sd, TRUE);
  }

/* Write a message to a local file descriptor, blocks until done */
pr_local_send(pid, mp)
  register pid;
  register message *mp;
  {
        register sd, incflag = 1;
        register address *ap;
        address pname;

        sd = client_lookup(pid);
        pname = ADDRESS(my_site_no, my_site_incarn, pid, 0);
        if(pg_verify(pname, mp) == -1)
            return;
        for(ap = msg_getdests(mp); !aptr_isnull(ap); ap++)
            if(addr_cmp(ap, &pname) == 0)
                switch(ap->addr_entry)
                {
                  message *gmgr_spy();
                  case GENERIC_ADDMEMB:
                  case GENERIC_ADDCLIENT:
                  case GENERIC_DELETE:
                  case GENERIC_SIGNAL:
                    if(mp = gmgr_spy(*ap, mp, sd))
                    {
                        incflag = 0;
                        goto deliver;
                    }
                    return;

                  case GENERIC_IGNORE:
                    reply(mp, (char*)0, 0, 0);
                    return;
                }
  deliver:
        if(sd == -1)
        {
            address pname;
            if(msg_getid(mp)&1)
           {
                pname = ADDRESS(my_site_no, my_site_incarn, pid, 0);
                reply_as(&pname, mp, (char*)0, 0, 0);
            }
        }
        else
        {
	    if(pid > 0 && pb_checkempty(pid))
	        msg_setpbflag(mp);
            tcp_write(sd, mp, incflag);
        }
  }

#define LONG_IOV        64              /* Longer than this -> use malloc() */

adesc iov_ad ={ sizeof(iovec)*LONG_IOV, 0, 4 };

#define iov_alloc()     ((iovec*)mallocate(&iov_ad))
#define iov_free(iovp)  mdeallocate((char*)iovp, &iov_ad)

#ifdef  SIMWRITEV

#define	min(a,b)	(a<b? a: b)

writev(so, iovp, iovl)
  register iovec *iovp;
  register iovl;
  {
        register nb = 0, wb;
        while(iovl--)
        {
            wb = write(so, iovp->iov_base, min(4096, iovp->iov_len));
            if(wb == -1)
                return(-1);
            nb += wb;
            if(wb < (iovp++)->iov_len)
                break;
        }
        return(nb);
  }
#endif

/* Write to client after spying on the message traffic */
tcp_write(sd, mp, incflag)
  register message *mp;
  {
        register cldesc *cp = &cl[sd];
        register nb, iovlen, ssite;

        EVENT(S_CLSENT);
        if(incflag)
            msg_increfcount(mp);
        if(cp->cl_flag&CLIENT_BUSY)
        {
            /* If client is not ready, don't try to send yet */
            client_enqueue(sd, mp);
            return(0);
        }
	ssite = msg_getreplyto(mp)->addr_site;
	if(cp->cl_pid != ISIS)
            bis(&cp->cl_watchedby, ssite);
	else
	{
	    register n;
	    register address *ap;
	    for(ap = msg_getdests(mp); !aptr_isnull(ap); ap++)
		if(ap->addr_site == my_site_no)
		{
	            for(n = 0; n < MAXREMOTE; n++)
		        if(rc[n].rc_pid == ap->addr_process)
			{
			    bis(&rc[n].rc_watchedby, ssite);
			    break;
			}
		}
	}
#	if (CL_DEBUG)
	    print("send to client %d: ", cp->cl_pid); pmsg(mp);
#	endif
        cp->cl_curmsg = mp;
        cp->cl_nbytes = msg_getlen(mp);
        cp->cl_iovlen = msg_getiovlen(mp);
        nb = sizeof(iovec)*cp->cl_iovlen;
        if(cp->cl_iovlen > LONG_IOV)
        {
            cp->cl_curiov = (iovec*)malloc(nb = sizeof(iovec)*cp->cl_iovlen);
            cp->cl_flag |= CLIENT_MALLOC;
        }
        else
            cp->cl_curiov = iov_alloc();
        bcopy((char*)msg_getiovec(mp), (char*)cp->cl_curiov, nb);
        cp->cl_iovptr = cp->cl_curiov;
	cp->cl_pdelay = 0;
        /* Respect the system-wide limit on iovec lengths */
        if((iovlen = cp->cl_iovlen) > MAX_IOVLEN)
             iovlen = MAX_IOVLEN;
        nb = writev(sd, cp->cl_iovptr, iovlen);
        if(nb == -1 && errno != EWOULDBLOCK && errno != EINTR)
        {
            if(errno != EPIPE)
                print("tcp_write failed to client %d\n", cp->cl_pid);
            client_failed(sd, TRUE);
            return(-1);
        }
	cl_xfered(cp, nb);
        return(0);
  }

cl_restart_io(sdlist)
  bitvec sdlist;
  {
        register sd, nb;

        bicv(&congested, &sdlist);
        for(sd = 0; sd <= max_cl; sd++)
            if(bit(&sdlist, sd))
            {
                register cldesc *cp = &cl[sd];
                nb = writev(sd, cp->cl_iovptr, cp->cl_iovlen);
                if(nb == -1 && errno != EWOULDBLOCK)
                    client_failed(sd, TRUE);
                else
                {
                    cl_xfered(cp, nb);
                    while(cp->cl_pid && (cp->cl_flag&CLIENT_BUSY) == 0 && cp->cl_queuelen)
                    {
                        register qnode *qp = qu_head(cp->cl_msgqueue);
                        register message *mp;
                        mp = qp->qu_msg;
                        cp->cl_queuelen -= msg_getlen(mp);
                        tcp_write(sd, mp, 1);
                        /* Free qnode node and message, both at once */
                        qu_free(qp);
                    }
                }
            }
  }

cl_xfered(cp, anb)
  register cldesc *cp;
  {
        register nb = anb;
        /* Better safe than sorry */
        t_scheck();
        cp->cl_lastact = Gettime();
        if(cp->cl_curmsg == 0)
            return;
        if(nb == -1)
            nb = 0;
        if((cp->cl_nbytes -= nb) == 0)
        {
            if(cp->cl_flag&CLIENT_MALLOC)
                free(cp->cl_curiov);
            else
                iov_free(cp->cl_curiov);
            msg_delete(cp->cl_curmsg);
            cp->cl_flag &= ~CLIENT_MALLOC;
            cp->cl_curiov = 0;
            cp->cl_curmsg = 0;
            cp->cl_flag &= ~CLIENT_BUSY;
            goto done;
        }
        bis(&congested, cp-cl);
        cp->cl_flag |= CLIENT_BUSY;
        while(nb > cp->cl_iovptr->iov_len)
        {
            nb -= cp->cl_iovptr->iov_len;
            --cp->cl_iovlen;
            ++cp->cl_iovptr;
        }
        if(nb)
        {
            cp->cl_iovptr->iov_base += nb;
            cp->cl_iovptr->iov_len -= nb;
        }
  done:
        if(anb == -1 && errno != EWOULDBLOCK)
	    client_failed(cp-cl, TRUE);
  }

condition want_incarn;
static  cl_congested;

/* Registers PID with system */
cl_register(mp)
  register message *mp;
  {
        register *pid = (int*)msg_getfield(mp, CL_PID, 1, (int*)0);
        register cldesc *cp;
        for(cp = cl; cp <= &cl[max_cl]; cp++)
            if(cp->cl_mp == mp)
                break;
        if(pid[0] == ISIS)
          sd_isis = cp-cl;
	if(cp->cl_pid == ISIS && (pid[0] & PID_REMOTE))
	{
	    register n;
	    for(n = 0; n < MAXREMOTE; n++)
		if(rc[n].rc_pid == 0)
		    break;
	    rc[n].rc_pid = pid[0];
            bclr(&rc[n].rc_watchedby);
	}
	else
	{
	    cp->cl_pid = pid[0];
	    bclr(&cp->cl_watchedby);
	    if(cp->cl_msgqueue == 0)
		cp->cl_msgqueue = qu_null();
	    cp->cl_pmonitor = qu_null();
	    cp->cl_paddr = NULLADDRESS;
	}
        if(pid[0] != ISIS && current_view.sv_incarn[my_site_no] == RECOVERY_INCARN)
            t_wait(&want_incarn, "want_incarn");
	if(cp && cp->cl_pid == pid[0])
	{
            cp->cl_paddr = ADDRESS(my_site_no, my_site_incarn, pid[0], 0);
            cp->cl_paddr.addr_portno = pid[1];
	}
        reply(mp, (char*)&my_site_incarn, FTYPE_LONG, sizeof(int));
        if(cl_congested)
        {
            register message *cmsg = msg_newmsg();
            address addr;
	    if(cp)
                addr = cp->cl_paddr;
	    else
		addr = NULLADDRESS;
            addr.addr_entry = GENERIC_CONGESTED;
            msg_setdest(cmsg, &addr);
            pr_local_send(pid, cmsg);
        }
  }

do_cl_probe(pid)
  register pid;
  {
	register cldesc *cp;
	register sd;
	sd = client_lookup(pid);
	if(sd < 0)
	    return;
	cp = &cl[sd];
	if(cp->cl_flag & CLIENT_PROBING)
	{
	    client_failed(sd, 0);
	    return;
	}
	cp->cl_flag |= CLIENT_PROBING;
        if(cp->cl_pdelay > 0)
	    (void)timeout(cp->cl_pdelay, do_cl_probe, pid);
  }

cl_hello(mp)
  register message *mp;
  {
	register sd, pid;
	register cldesc *cp;
        if((sd = client_lookup(pid = msg_getsender(mp)->addr_process)) != -1)
	{
	    cp = &cl[sd];
	    cp->cl_flag &= ~CLIENT_PROBING;
	}
  }

cl_died(mp)
  register message *mp;
  {
	register sd, pid;
	register cldesc *cp;
	address who;
	if(msg_get(mp, "%A[1]", &who) == 1 && who.addr_process < -10)
	{
	    register n;
            for(n = 0; n < MAXREMOTE; n++)
                if(rc[n].rc_pid == who.addr_process)
                    break;
	    if(n != MAXREMOTE)
	    {
		register failinfo *fip;
		int client_crashed();
                rc[n].rc_pid = 0;
                fip = fi_alloc();
                fip->fi_pid = who.addr_process;
                fip->fi_watchedby = rc->rc_watchedby;
                t_fork(client_crashed, fip, (message*)0);
	    }
	}
	else
	{
	    print("cl_died: ome problem with ");
	    msg_printaccess(mp);
	}
  }

cl_probe(mp)
  register message *mp;
  {
	register sd, pid;
	register cldesc *cp;
	int freq, tout, pdelay, error;
	msg_get(mp, "%d,%d", &freq, &tout);
	pdelay = freq+tout;
	if(pdelay < 0 || pdelay >= 300)
	{
	    pdelay = 0;
	    error = IE_BADARG;
	}
	else
	    error = 0;
        if((sd = client_lookup(pid = msg_getsender(mp)->addr_process)) != -1)
	{
	    cp = &cl[sd];
	    cp->cl_pdelay = pdelay*1000;
	    if(pdelay > 0)
	    {
	        cp->cl_flag |= CLIENT_PROBING;
	        (void)timeout(cp->cl_pdelay, do_cl_probe, pid);
	    }
            reply(mp, &error, FTYPE_LONG, sizeof(int));
	}
	
  }

cl_learned_incarn()
  {
        register cldesc *cp;
        for(cp = cl; cp <= &cl[max_cl]; cp++)
             if(cp->cl_pid)
                cp->cl_paddr.addr_incarn = my_site_incarn;
  }

cl_send_all(ent, msg)
  register ent;
  register message *msg;
  {
        register cldesc *cp;
        address addr, alist[MAXBITS+1];
	register address *ap;
        register message *mp = msg;
        if(mp == (message*)0)
            mp = msg_newmsg();
	ap = alist;
        cl_congested = (ent == GENERIC_CONGESTED);
        for(cp = cl; cp <= &cl[max_cl]; cp++)
             if(cp->cl_pid)
             {
                addr = cp->cl_paddr;
                addr.addr_entry = ent;
	        *ap++ = addr;
             }
	if(ap != alist)
	{
	    *ap = NULLADDRESS;
	    msg_setdests(mp, alist);
	    for(ap = alist; !aptr_isnull(ap); ap++)
	        pr_local_send(ap->addr_process, mp);
	}
        if(msg == (message*)0)
            msg_delete(mp);
  }

cl_pmonitor(mp)
  register message *mp;
  {
        address addr;
        register cldesc *cp;
        int outcome;
        addr = *(address*)msg_getfield(mp, CL_PNAME, 1, (int*)0);
        for(cp = cl; cp <= &cl[max_cl]; cp++)
            if(cp->cl_mp == mp)
                break;
        cp->cl_mp = 0;
        addr.addr_entry = 0;
        cp->cl_flag |= CLIENT_PMONITOR;
        if(is_proc_alive(addr) && cp->cl_pmonitor)
        {
            outcome = 0;
            if(pg_find(cp->cl_pmonitor, &addr) == (qnode*)0)
                pg_add(cp->cl_pmonitor, &addr, 0, nullroutine);
        }
        else
            outcome = -1;
        reply(mp, &outcome, FTYPE_LONG, sizeof(outcome));
  }

/* Called when first adding a proc node: actually check with remote site */
is_proc_alive(addr)
  address addr;
  {
        if(addr.addr_site == my_site_no)
        {
            if(addr.addr_incarn != my_site_incarn)
                return(0);
            if(check_proc_status(addr.addr_process, 0))
                return(1);
            tsleep(2);
            if(check_proc_status(addr.addr_process, 0))
                return(1);
            tsleep(20);
            return(check_proc_status(addr.addr_process, 0));
        }
        else if(current_view.sv_incarn[addr.addr_site] != addr.addr_incarn)
            return(0);
        begin
        {
            register message *mp;
            char answ;
            static address dest[2];
            mp = msg_genmsg(CL_PNAME, (char*)&addr, FTYPE_ADDRESS, sizeof(address), 0);
            dest[0] = addr;
            dest[0].addr_process = PROTOCOLS;
            dest[0].addr_entry = PR_IS_ALIVE;
            if(BCAST(dest, mp, 1, collect_answ, &answ, 1) == 0 || answ == '-')
            {
                msg_delete(mp);
                return(0);
            }
            msg_delete(mp);
        }
        return(1);
  }

pr_is_alive(mp)
  register message *mp;
  {
        register address *ap = (address*)msg_getfield(mp, CL_PNAME, 1, (int*)0);
        register address* msender;
        msender = msg_getsender(mp);
        if(ap->addr_site == my_site_no && ap->addr_incarn == my_site_incarn)
        {
            if(check_proc_status(ap->addr_process, msender->addr_site))
                goto ok;
            tsleep(2); 
            if(check_proc_status(ap->addr_process, msender->addr_site))
                goto ok;
            /* Currently unknown, but give him a chance to start just in case... */ 
            tsleep(20); 
            if(check_proc_status(ap->addr_process, msender->addr_site))
            {
            ok:
                reply(mp, "+", FTYPE_CHAR, 1);
                return;
            }
        } 
        reply(mp, "-", FTYPE_CHAR, 1);
  }

check_proc_status(pid, s)
  register pid, s;
  {
        register cldesc *cp;
	register remote *rp;
        if(pid == PROTOCOLS)
            return(1);
	if(pid < -10)
	{
            for(rp = rc; rp <= &rc[MAXREMOTE]; rp++)
                if(rp->rc_pid == pid)
                {
                    if(s)
                        bis(&rp->rc_watchedby, s);
                    return(1);
                }
            return(0);
	}
        for(cp = cl; cp <= &cl[max_cl]; cp++)
            if(cp->cl_pid == pid)
            {
                if(s)
                    bis(&cp->cl_watchedby, s);
                return(1);
            }
        return(0);
  }

/* Called when a process has been observed to fail */
proc_has_failed(pname)
  address *pname;
  {
        register cldesc *cp;
        register qnode *qp;
        for(cp = cl; cp <= &cl[max_cl]; cp++)
            if((cp->cl_flag&CLIENT_PMONITOR) && (qp = pg_find(cp->cl_pmonitor, pname)))
            {
                static address addrs[2];
                register message *mp = msg_genmsg(CL_PNAME, (char*)pname, FTYPE_ADDRESS, sizeof(address), 0);
                qu_free(qp);
                addrs[0] = cp->cl_paddr;
                addrs[0].addr_entry = GENERIC_PROC_FAILED;
                msg_setdests(mp, addrs);
                pr_bcast(mp);
                msg_delete(mp);
                if(qu_head(cp->cl_pmonitor) == 0)
                    cp->cl_flag &= ~CLIENT_PMONITOR;
            }
  }

client_lookup(pid)
  register pid;
  {
        register sd;
	if(pid < -10)
	{
            for(sd = 0; sd < MAXREMOTE; sd++)
		if(rc[sd].rc_pid == pid)
		    break;
	    if(sd == MAXREMOTE)
		return(-1);
	    return(sd_isis);
	}
        for(sd = 0; sd <= max_cl; sd++)
            if(cl[sd].cl_pid == pid)
                return(sd);
        return(-1);
  }

client_newview()
  {
        register cldesc *cp;
        register qnode *qp, *nqp;
        for(cp = cl; cp <= &cl[max_cl]; cp++)
        {
            address addr;
            register message *mp = msg_genmsg(CL_VIEW, (char*)&current_view, FTYPE_CHAR, sizeof(sview), 0);
            addr = cp->cl_paddr;
            addr.addr_entry = GENERIC_NEW_SVIEW;
            msg_setdest(mp, &addr);
            pr_bcast(mp);
            msg_delete(mp);
            if(cp->cl_flag&CLIENT_PMONITOR)
                for(qp = cp->cl_pmonitor->qu_next; qp != cp->cl_pmonitor; qp = nqp)
                    if(current_view.sv_incarn[qp->qu_pname.addr_site] != qp->qu_pname.addr_incarn)
                    {
                        /* New view will trigger this watch event in the client */
                        nqp = qp->qu_next;
                        qu_free(qp);
                        if(qu_head(cp->cl_pmonitor) == 0)
                            cp->cl_flag &= ~CLIENT_PMONITOR;
                    }
                    else
                        nqp = qp->qu_next;
        }
  }


client_failed(sd, closef)
  register sd;
  {
        int pid, client_crashed();
        register cldesc *cp = &cl[sd];
        register failinfo *fip;
        char clfile[32];

        if(closef)
        {
            bic(&input_mask, sd);
	    bic(&congested, sd);
            close(sd);
        }
        if((pid = cp->cl_pid) == 0)
            return;
        if(pid == ISIS)
            panic("isis monitoring process at this site has crashed!");
#ifdef  UNIX_DOM
        sprintf(clfile, "/tmp/Cl%d", pid);
        unlink(clfile);
#endif
        cp->cl_pid = 0;
        if(cp->cl_msgqueue)
            qu_freeall(cp->cl_msgqueue);
        if(cp->cl_pmonitor)
            qu_freeall(cp->cl_pmonitor);
        if(cp->cl_curiov)
        {
            if(cp->cl_flag&CLIENT_MALLOC)
                free(cp->cl_curiov);
            else
                iov_free(cp->cl_curiov);
        }
        if(cp->cl_curmsg)
            msg_delete(cp->cl_curmsg);
        if(cp->cl_blkdesc)
            blk_free (cp->cl_blkdesc);
        fip = fi_alloc();
        fip->fi_pid = pid;
        fip->fi_watchedby = cp->cl_watchedby;
        bzero(cp, sizeof(*cp));
        t_fork(client_crashed, fip, (message*)0);
  }

client_crashed(fip)
  register failinfo *fip;
  {
        register message *mp;
        address pname;
        site_id slist[MAX_SITES];
        register site_id *sp = slist;
        register s, i;
        /* Now inform other sites and initiate failure gbcast */
        cb_procflush(fip->fi_pid);
        for(s = 1; s < MAX_SITES; s++)
            if((s == my_site_no || bit(&fip->fi_watchedby, s)) && (i = current_view.sv_incarn[s]) >= 0)
                *sp++ = MAKE_SITE_ID(s, i);
        *sp = 0;
        mp = msg_genmsg(FLD_FAILED, (char*)&fip->fi_pid, FTYPE_LONG, sizeof(int), 0);
        pname = ADDRESS(my_site_no, my_site_incarn, fip->fi_pid, 0);
        msg_setsender(mp, &pname);
        BCAST_SL(slist, PROTOCOLS, PR_CLFAILED, mp, 0, 0, 0, 0);
        msg_delete(mp);
        pg_failed(fip->fi_pid);
        fi_free(fip);
  }

client_kill(sd)
  register sd;
  {
        register cldesc *cp = &cl[sd];
        if(cp->cl_pid)
            kill(cp->cl_pid, SIGKILL);
  }

#define CL_ACTIVE       2000            /* Client gets 2 seconds to read something */

client_enqueue(sd, msg)
  register sd;
  register message *msg;
  {
        register cldesc *cp = &cl[sd];
        register len = msg_getlen(msg);

        if((cp->cl_queuelen += len)+cp->cl_nbytes > MAXCQUEUE)
        {
            if(Gettime()-cp->cl_lastact < CL_ACTIVE)
                goto ok;
            if((cp->cl_flag&CLIENT_OVERFLOW) == 0)
                print("Message queue for client %d overflowed: discarding a message\n", cp->cl_pid);
            cp->cl_flag |= CLIENT_OVERFLOW;
            /* Client will either catch signal or simply barf */
            if(cp->cl_pid)
                kill(cp->cl_pid, SIG_OVERFLOW);
            cp->cl_queuelen -= len;
            reply_as(&cp->cl_paddr, msg, (char*)0, 0, 0);
            msg_delete(msg);
            return;
        }
  ok:
        if(cp->cl_msgqueue == 0)
            cp->cl_msgqueue = qu_null();
        qu_add_mp(cp->cl_msgqueue, QU_MSG, msg, MSG_DELETE);
  }

dump_clients()
  {
        register sd;
	bitvec mask;
	static struct timeval no_wait = {0,0};
        mask = input_mask;
	select(MAXBITS, &mask, 0, 0, &no_wait);
        for(sd = 0; sd <= max_cl; sd++)
        {
            register cldesc *cp = &cl[sd];
            if(cp->cl_pid)
            {
                register d = 0;
                register qnode *qp;
                print("[%.2d] ", sd);
                pclient(cp->cl_pid);
		if(bit(&mask, sd))
		    print(" (* input available *)");
                print(": ");
                if(cp->cl_flag&CLIENT_BUSY)
                    print("busy (%d bytes in transit, %d bytes queued) ", cp->cl_nbytes, cp->cl_queuelen);
                else
                    print("idle");
                if(cp->cl_flag&CLIENT_READING)
                    print(" reading (need %d bytes)", cp->cl_rlen);
                if(cp->cl_flag&CLIENT_PMONITOR)
                {
                    register qnode *qp;
                    print(" monitoring < ");
                    for(qp = cp->cl_pmonitor->qu_next; qp != cp->cl_pmonitor; qp = qp->qu_next)
                    {
                        paddr(&qp->qu_pname);
                        print(" ");
                    }
                    print(">");
                }
                bic(&cp->cl_watchedby, my_site_no);
                if(btst(&cp->cl_watchedby))
                {
                    register s;
                    print(" watched by sites < ");
                    for(s = 1; s < MAX_SITES; s++)
                        if(bit(&cp->cl_watchedby, s) && current_view.sv_incarn[s] != DOWN_INCARN)
                            print("%d ", s);
                    print(">");
                }
                bis(&cp->cl_watchedby, my_site_no);
                print("\n");
                if(cp->cl_msgqueue == 0)
                    print("  *** null msgqueue pointer!  how can this be? ***\n");
                else for(qp = cp->cl_msgqueue->qu_next; qp != cp->cl_msgqueue; qp = qp->qu_next)
                {
                    print("  Delivery queue[%d]: ", ++d);
                    pmsg(qp->qu_msg);
                }
            }
        }
        print("Active remote clients:\n");
	for(sd = 0; sd < MAXREMOTE; sd++)
	{
	   if(rc[sd].rc_pid)
	   {
		print("  remote_%d ", rc[sd].rc_pid & (short)~PID_REMOTE);
                bic(&rc[sd].rc_watchedby, my_site_no);
                if(btst(&rc[sd].rc_watchedby))
                {
                    register s;
                    print(" watched by sites < ");
                    for(s = 1; s < MAX_SITES; s++)
                        if(bit(&rc[sd].rc_watchedby, s) && current_view.sv_incarn[s] != DOWN_INCARN)
                            print("%d ", s);
                    print(">");
                }
                bis(&rc[sd].rc_watchedby, my_site_no);
	        print("\n");
	   }
	}
  }

pclient(pid)
  {
        switch(pid)
        {
          default: print("client pid=%d", pid); break;
          case ISIS: print("<site-monitor>"); break;
          case REXEC: print("<rexec>"); break;
          case RMGR: print("<rmgr>"); break;
          case NEWS: print("<news>"); break;
          case XMGR: print("<xmgr>"); break;
        }
  }
