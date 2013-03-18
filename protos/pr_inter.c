/*  $RCSfile: pr_inter.c,v $ $Revision: 2.22 $ $Date: 90/08/28 15:11:47 $  */
/*
 *	Originally coded by Ken Birman
 *      UDP code for packet transmission, flow control between ISIS sites
 *	Can't use TCP here `cause too many connections are needed
 *	Also detects ``apparent failures''
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

struct  hostent *gethostbyname();
struct  servent *getservbyname();

#if     (HPUX)
#define   sendmsg(x, y, z)      panic("sendmsg was called\n")
#endif

/* Normal mode: rapid communication with the site */
extern	FDTIMEOUT;

#define sec                   1000
#define TIME_ESTAB (FDTIMEOUT*sec)        /* Give up on connection estab. after 30 secs */
#define TIME_FAIL      FDTIMEOUT/2        /* Detect failures after, say, 10 times the rtdelay, which varies */
#define TIME_RET         (4*sec)        /* Initial value of rtdelay: retransmit after 4secs */
#define MAX_RET                3        /* Trigger failure protocol after MAXRET retransmissions */
#define MIN_RET              sec        /* Minimum RT delay */
#define MAX_INET            1400        /* Packets larger than this will be fragmented in UDP */

#define ACCEPT                 0
#define DISCARD                1

int     net_xmits, ISIS_TIME;
static  ttid;
static  saddr my_addr;
static  message *ackpacket, *deadpacket, *hello;
static  intersite is_info, *ackinf, *deadinf;
static  ioq ioqs[MAX_SITES];
int     staged_xmit;
int     hello_delay;
static  msg_tank *tank; /* FIFO of incoming messages not yet sent to destination process. */
static  bitvec tank_mask;
static  struct timeval tank_wait = { 0, 0 };
message *is_newmsg();
extern net_sweep();

static adesc    is_adesc ={ sizeof(mdesc), 0, 16, };

#define mdalloc()        ((mdesc*)mallocate(&is_adesc))
#define mdfree(qp)       mdeallocate((char*)qp,&is_adesc)

#define FLUSH_ON        0               /* Turn lazy_flush on */
#define DO_FLUSH        1               /* Do lazy_flush now */

#define MAXSEQN         0x7f
#define SEQN(x)         ((x)&MAXSEQN)
#define INC(x, i)       SEQN(x+i)
#define DEC(x)          INC(x, MAXSEQN)

/* Decide if <sn> is between <lb> and <hb> */
INRANGE(lb, hb, sn)
  register lb, hb, sn;
  {
        if(lb == hb)
            return(sn == lb);
        if((lb < hb)? (sn < lb || sn > hb): (sn > hb && sn < lb))
            return(0);
        return(1);
  }

net_init()
  {
	hello_delay = FDTIMEOUT;
        tank = msg_tank_create(250000);

        /* Create a UDP socket */
        if((net_socket = socket(AF_INET, SOCK_DGRAM, 0)) == -1)
        {
            perror("socket");
            panic("net_init");
        }
	set_isis_sockopts(net_socket);
        bis(&input_mask, net_socket);
        my_addr.sin_family = AF_INET;

        if(udp_port[my_site_no] == 0)
        {
            register struct servent *sp = getservbyname("isis", "udp");
            if(sp == (struct servent*)0)
                panic("isis.*: service not listed in /etc/services on this host");
            udp_port[my_site_no] = ntohs(sp->s_port);
        }
        my_addr.sin_port = htons(udp_port[my_site_no]);


        if(bind(net_socket, (struct sockaddr*)&my_addr, sizeof(my_addr)) == -1)
        {
            perror("bind");
            panic("<protos> was unable to acquire UDP port %d.  Was ISIS run twice?", ntohs(my_addr.sin_port));
        }

        is_info.is_seqn = SEQ_DEAD;
        is_info.is_from = my_site_id;
        deadpacket = is_newmsg();
        deadinf = (intersite*)msg_getfield(deadpacket, INTERSITE_HDR, 1, (int*)0);
        is_info.is_seqn = SEQ_ACK;
        ackpacket = is_newmsg();
        ackinf = (intersite*)msg_getfield(ackpacket, INTERSITE_HDR, 1, (int*)0);

        hello = msg_newmsg();

        net_sweep();
  }

/* Prepare to communicate with site `s' */
net_isalive(s)
  site_id s;
  {
        register struct hostent *hep;
        register ioq *ioqp = &ioqs[SITE_NO(s)];
        static char hknown[MAX_SITES];
        register char *sp;
        if(SITE_NO(s) == my_site_no)
            return;
        if(ioqp->io_sid == s)
	    return;
        ioqp->io_sid = s;
        ioqp->io_state &= ~(IO_DEAD|IO_TOLDFD);
        /* Avoid calling him down before he has time to start saying hello */
        if(my_site_incarn != RECOVERY_INCARN)
            /* Expect acks and retransmissions */
            ioqp->io_state |= IO_ESTAB;
        if(ioqp->io_state&IO_ALIVE)
            return;
        if(hknown[SITE_NO(s)] == 0)
        {
            char str[64];
            strcpy(str, site_names[SITE_NO(s)]);
            for(sp = str; *sp; ++sp)
                if(*sp == '/')
                    *sp-- = 0;
            if((hep = gethostbyname(str)) == 0)
            {
                print("net_isalive: site %s unknown in local hosts table\n", str);
                return;
            }
            hknown[SITE_NO(s)]++;
            ioqp->io_address.sin_family = AF_INET;
            bcopy(hep->h_addr, &ioqp->io_address.sin_addr, hep->h_length);
            ioqp->io_address.sin_port = htons(udp_port[SITE_NO(s)]);
        }
        if((ioqp->io_state&IO_INUSE) == 0)
        {
            register packet *out;
            ioqp->io_state |= IO_INUSE;
            ioqp->io_backlog = qu_null();
            for(out = ioqp->io_out; out != &ioqp->io_out[WSIZE]; out++)
                out->pk_contents = qu_null();
        }
        ioqp->io_state |= IO_ALIVE;
        ioqp->io_wantack = 0;
        ioqp->io_nslots = WSIZE;
        ioqp->io_iseqn = 0;
        ioqp->io_oseqn = 1;
        ioqp->io_nseqn = 1;
        ioqp->io_ndups = ioqp->io_nacks = ioqp->io_nmsgs = ioqp->io_ndata = 0;
        ioqp->io_nret = ioqp->io_nsent = ioqp->io_abits = 0;
        ioqp->io_lastin = ISIS_TIME;
        qu_add_ioq(IOQS, IOQ_PTR, ioqp);
        if(my_site_incarn != RECOVERY_INCARN)
            ioqp->io_rtdelay = TIME_RET;
	else
            ioqp->io_rtdelay = TIME_RET*10;
        return;
  }

/* failure detector uses this to tell the network of a dead site-id */
net_isdead(s)
  site_id s;
  {
        register i;
        register qnode *qp;
        register ioq *ioqp = &ioqs[SITE_NO(s)];
        print("net_isdead: %d/%d\n", SITE_NO(s), SITE_INCARN(s));
        if(ioqp->io_dead == s)
            return;
        if(ioqp->io_sid != s && SITE_INCARN(ioqp->io_sid) != RECOVERY_INCARN)
        {
            print("io_sid was %d/%d!\n", SITE_NO(ioqp->io_sid), SITE_INCARN(ioqp->io_sid));
            return;
        }
        for(qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next)
            if(qp->qu_ioq == ioqp)
            {
                qu_free(qp);
                break;
            }

        /* Drain output and input packets */
        for(i = 0; i != WSIZE; i++)
        {
            register packet *out = &ioqp->io_out[INC(ioqp->io_oseqn, i)&WMASK];
            register message *mp;

            if(mp = out->pk_packet)
            {
                register qnode *hp;
                msg_delete(mp);
                while(hp = qu_head(out->pk_contents))
                {
                    (*hp->qu_callback)(ioqp->io_sid, hp->qu_args[0], hp->qu_args[1]);
                    qu_free(hp);
                }
                out->pk_packet = 0;
                out->pk_state = 0;
            }
        }

        /* Next drain backlogged messages */
        while(qp = qu_head(ioqp->io_backlog))
        {
            register mdesc *md = qp->qu_md;
            qu_free(qp);
            if(md->md_cb)
                (*md->md_cb)(s, md->md_arg0, md->md_arg1);
            msg_delete(md->md_msg);
            mdfree(md);
        }

        for(i = 0; i < MAXWSIZE; i++)
        {
            register message *mp;
            if(mp = ioqp->io_in[i])
            {
                msg_delete(mp);
                ioqp->io_in[i] = 0;
            }
        }
        bzero(ioqp->io_in, sizeof(ioqp->io_in));
        ioqp->io_sid = MAKE_SITE_ID (SITE_NO(ioqp->io_sid), RECOVERY_INCARN);
        ioqp->io_state = IO_INUSE|IO_DEAD;
        ioqp->io_wantack = 0;
        ioqp->io_nslots = WSIZE;
        ioqp->io_iseqn = 0;
        ioqp->io_oseqn = 1;
        ioqp->io_nseqn = 1;
        intersite_congested -= ioqp->io_backed;
        intersite_congested -= ioqp->io_nbytes;
        ioqp->io_nbytes = ioqp->io_abits = ioqp->io_backed = 0;
        ioqp->io_dead = s;
  }

/* Called from pr_fdect when a site fails early in its recovery sequence */
net_death_reset(s)
  site_id s;
  {
        register ioq *ioqp = &ioqs[SITE_NO(s)];
        int old_dead;
        old_dead = ioqp->io_dead;
        net_isdead(ioqp->io_sid);
        ioqp->io_dead = old_dead;
  }

/*
 *      These are the interesting routines.  They do packeted output
 *      and acknowledged input.  Fragment the packet if necessary.
 */
net_send(mp, to, callback, arg0, arg1)
  register message *mp;
  register site_id *to;
  int (*callback)();
  char *arg0, *arg1;
  {
        static fragmenting;
        if(fragmenting == 0 && msg_getlen(mp) > (MAXMSGLEN-128))
        {
            ++fragmenting;
            net_fragment(mp, to, callback, arg0, arg1);
            --fragmenting;
        }
        else
        {
            --to;
            while(*++to)
            {
                register ioq *ioqp = &ioqs[SITE_NO(*to)];
                if(*to == ioqp->io_sid)
                    (void)net_trytosend(mp, ioqp, FALSE, callback, arg0, arg1);
                else if(callback)
                    (*callback)(*to, arg0, arg1);
            }
        }
  }

/*
 *   Trick to encourage piggybacking:
 *      force_piggybacking: set if all messages should be piggybacked for now 
 *      net_urgent: counts packets containing urgent messages 
 *      ioqp->io_accum: identifies the corresponding urgent packet, if any
 *      ioqp->io_state: IO_URGENT,IO_ACCUM set as needed
 */
static  force_piggybacking, net_urgent;

flush_urgent(ioqp)
  register ioq *ioqp;
  {
        if(ioqp->io_state&IO_URGENT)
            net_output(ioqp, ioqp->io_accum);
  }

/* Fragment this message into smaller pieces for transmission */
net_fragment(mp, to, callback, arg0, arg1)
  message *mp;
  site_id *to;
  int (*callback)();
  char *arg0, *arg1;
  {
        iovec *iovp;
        register iovec *ip;
        int iovl, msglen, nfrags, msgsize, firstfrag, iolen;
        char *iobase;
        message *fmp;
        fraghdr fhdr;
        address *dp, dests[MAX_SITES+1];
        site_id *dsite;
        iovp = msg_getiovec(mp);
        iovl = msg_getiovlen(mp);
        msgsize = 0;
        ++force_piggybacking;
        dp = dests;
        for(dsite = to; SITE_NO(*dsite); ++dsite)
            *dp++ = ADDRESS(SITE_NO(*dsite), SITE_INCARN(*dsite), PROTOCOLS, PR_FRAGMAN);
        *dp = NULLADDRESS;

        msglen = 0;
        for(ip = iovp; ip < &iovp[iovl]; ip++)
            msglen += ip->iov_len;
        nfrags = (msglen+FRAGSIZE-1)/FRAGSIZE;
        fhdr.fr_nfrags = nfrags;
        fhdr.fr_msglen = msglen;

        /* Now send <nfrags> messages containing data */
        firstfrag = 0;
        iolen = iovp->iov_len;
        iobase = iovp->iov_base;
        while(nfrags--)
        {
            register fragsize = 0, len;

            fmp = is_newmsg();
            if(firstfrag++ == 0)
            {
                /* Only the first frag contains header */
                msg_addfield(fmp, INTERSITE_FHDR, (char*)&fhdr, FTYPE_LONG, sizeof(fhdr));
            }
            msg_setdests(fmp, dests);
            do
            {
                if((len = iolen) > FRAGSIZE-fragsize)
                    len = FRAGSIZE-fragsize;
                /* Should try and eliminate this extra copying eventually */
                msg_addfield(fmp, INTERSITE_FRAG, iobase, FTYPE_CHAR, len);
                if(iolen -= len)
                    iobase += len;
                else
                {
                    ++iovp;
                    iolen = iovp->iov_len;
                    iobase = iovp->iov_base;
                }
                fragsize += len;
                msgsize += len;
            }
            while(fragsize < FRAGSIZE && msgsize < msglen);
            if(nfrags)
                net_send(fmp, to, nullroutine, 0, 0);
            else
                /* Callback on last fragment */
                net_send(fmp, to, callback, arg0, arg1);
            msg_delete(fmp);
        }
        --force_piggybacking;
        if(net_urgent)
        {
            register qnode *qp;
            for(qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next)
                flush_urgent(qp->qu_ioq);
        }
  }


/*
 * Reassemble a fragmented message
 */
fragman(fmp)
  message *fmp;
  {
        register ioq *ioqp;
        register fraghdr *fh;
        static char *bptrs[256];
        static int blens[256];
        register i, n;

        ioqp = &ioqs[msg_getsender(fmp)->addr_site];
        fh = &ioqp->io_fhdr;
        if((ioqp->io_state&IO_ASSEM) == 0)
        {
            fraghdr *fhdr = (fraghdr*)msg_getfield(fmp, INTERSITE_FHDR, 1, (int*)0);
            *fh = *fhdr;
            ioqp->io_blk_desc = msg_malloc (((fh->fr_msglen+3)&~3));
            ioqp->io_fptr = msg_body(ioqp->io_blk_desc);
            ioqp->io_state |= IO_ASSEM;
        }
        n = msg_getfields(fmp, INTERSITE_FRAG, bptrs, blens, 256);
        for(i = 0; i < n; i++)
        {
            bcopy(bptrs[i], ioqp->io_fptr, blens[i]);
            ioqp->io_fptr += blens[i];
        }
        /* If this was the last message, check for integrity and deliver it */
        if(--fh->fr_nfrags <= 0)
        {
            message *mp;
            ioqp->io_blk_desc->blk_avail -= fh->fr_msglen;
            if(mp = msg_reconstruct(ioqp->io_blk_desc))
                pr_local_delivery(mp, (int*)0);
	    else
		panic("fragman: reconstruct failed!");
            ioqp->io_state &= ~IO_ASSEM;
            ioqp->io_fptr = 0;
            msg_delete(mp);
        }
  }

/* Drain as many messages off the congestion qnode as possible, encourage piggybacking */
net_drain(ioqp)
  register ioq *ioqp;
  {
        register qnode *qp;

        ++force_piggybacking;
        while(qp = qu_head(ioqp->io_backlog))
        {
            register mdesc *md = qp->qu_md;
            if(net_trytosend(md->md_msg, ioqp, TRUE, md->md_cb, md->md_arg0, md->md_arg1))
            {
                --force_piggybacking;
                flush_urgent(ioqp);
                return;
            }
            ioqp->io_backed -= msg_getlen(md->md_msg);
            intersite_congested -= msg_getlen(md->md_msg);
            msg_delete(md->md_msg);
            mdfree(md);
            qu_free(qp);
        }
        --force_piggybacking;
        flush_urgent(ioqp);
  }

/* Try to send this message, on congestion qnode if `tryingtodrain' */
net_trytosend(mp, ioqp, tryingtodrain, callback, arg0, arg1)
  register message *mp;
  register ioq *ioqp;
  register tryingtodrain;
  int (*callback)();
  char *arg0, *arg1;
  {
        register size = msg_getlen(mp);
        register packet *out = &ioqp->io_out[ioqp->io_nseqn&WMASK];
# ifdef INTERVERBOSE
        print("*** NET: TRY TO SEND "); pmsg(mp); msg_printaccess(mp);
# endif
        if((ioqp->io_state&(IO_INUSE|IO_DEAD)) != IO_INUSE)
        {
            if(ioqp->io_state&IO_DEAD)
            {
                if(callback)
                    (*callback)(ioqp->io_sid, arg0, arg1);
                return(0);
            }
        }
        msg_increfcount(mp);
        if((force_piggybacking || msg_islazy(mp)) && (ioqp->io_state&IO_ACCUM))
        {
            register total = out->pk_size+size;
            /* Continue to accumulate packet if possible */
            if(total <= MAXSIZE && (total <= MAX_INET || total > MAX_INET*4/3))
            {
                if((ioqp->io_state&IO_URGENT) == 0 && msg_islazy(mp) == 0)
                {
                    ioqp->io_state |= IO_URGENT;
                    ++net_urgent;
                }
                msg_addmsg(out->pk_packet, INTERSITE_MSG, mp);
                if(callback)
                    qu_add_cb(out->pk_contents, callback, arg0, arg1);
                msg_delete(mp);
                out->pk_size = msg_getlen(out->pk_packet);
                if(++out->pk_cnt == MAXMSGS-1 || out->pk_size >= MAXSIZE-SMLPKT)
                    net_output(ioqp, out);
                return(0);
            }
        }

        /* Flush accum packet before starting a new packet */
        if(ioqp->io_state&IO_ACCUM)
        {
            net_output(ioqp, out);
            out = &ioqp->io_out[ioqp->io_nseqn&WMASK];
        }

        /* This code generates a new packet for <mp> */
        if(ioqp->io_nslots == 0 || (ioqp->io_backed && tryingtodrain == FALSE) || ioqp->io_nbytes > PK_HIWAT)
        {
            if(tryingtodrain == FALSE)
            {
                register mdesc *md = mdalloc();
                md->md_msg = mp;
                md->md_cb = callback;
                md->md_arg0 = arg0;
                md->md_arg1 = arg1;
                qu_add_md(ioqp->io_backlog, MSG_DESC, md, nullroutine);
                ioqp->io_backed += msg_getlen(mp);
                intersite_congested += msg_getlen(mp);
                return(0);
            }
            msg_delete(mp);
            return(-1);
        }
        if(ioqp->io_nslots-- == WSIZE)
            /* Haven't sent to him in a while */
            ioqp->io_lastin = ISIS_TIME;
        out->pk_packet = is_newmsg();
        out->pk_state = PK_INUSE;
        out->pk_cnt = 1;
        msg_addmsg(out->pk_packet, INTERSITE_MSG, mp);
        out->pk_size = msg_getlen(out->pk_packet);
        if(callback)
            qu_add_cb(out->pk_contents, callback, arg0, arg1);
        if((force_piggybacking || msg_islazy(mp)) && size < MAXSIZE-SMLPKT)
        {
            lazy_flush(FLUSH_ON);
            ioqp->io_state |= IO_ACCUM;
            ioqp->io_accum = out;
            if((ioqp->io_state&IO_URGENT) == 0 && msg_islazy(mp) == 0)
            {
                ioqp->io_state |= IO_URGENT;
                ++net_urgent;
            }
        }
        else
            net_output(ioqp, out);
        msg_delete(mp);
        return(0);
  }

message *
is_newmsg()
  {
        register message *msg = msg_newmsg();
        msg_addfield(msg, INTERSITE_HDR, (char*)&is_info, FTYPE_INTERSITE, sizeof(is_info));
        return(msg);
  }

/* First time transmission of a message */
net_output(ioqp, out)
  register ioq *ioqp;
  register packet *out;
  {
        register message *mp = out->pk_packet;
        register intersite *is = (intersite*)msg_getfield(mp, INTERSITE_HDR, 1, (int*)0);
        ioqp->io_nsent++;
        out->pk_state |= PK_SENT;
        is->is_seqn = ioqp->io_nseqn;
        is->is_from = my_site_id;
        is->is_dest = ioqp->io_sid;
        is->is_abprio = ab_priority;
        is->is_viewid = current_view.sv_viewid;
        out->pk_size = msg_getlen(mp);
        ioqp->io_nseqn = INC(ioqp->io_nseqn, 1);
        if(ioqp->io_accum == out)
        {
            if(ioqp->io_state&IO_URGENT)
                --net_urgent;
            ioqp->io_state &= ~(IO_ACCUM|IO_URGENT);
            ioqp->io_accum = 0;
        }
        ioqp->io_nbytes += msg_getlen(mp);
        intersite_congested += msg_getlen(mp);
        out->pk_whensent = ISIS_TIME;
        out->pk_nret = 0;
        out->pk_rtwhen = out->pk_whensent+ioqp->io_rtdelay;
        ttid = timeout_reschedule(ttid, 1*sec, net_sweep, 0, 0);
        net_xmit(ioqp, mp);
        return(0);
  }

/* Put a copy of the packet on the wire... */
net_xmit(ioqp, mp)
  register ioq *ioqp;
  register message *mp;
  {
        register iovlen;
        register intersite *hdr;

        t_scheck();

        if((ioqp->io_state&IO_INUSE) == 0)
            return;

        EVENT(S_ISENT);
        if(mp == ackpacket)
        {
            if(my_site_incarn == RECOVERY_INCARN) 
                return;
            hdr = ackinf;
            hdr->is_from = my_site_id;
            hdr->is_dest = ioqp->io_sid;
            CEVENT(SITE_NO(ioqp->io_sid), C_NACK);
        }
        else
        {
            hdr = (intersite*)msg_getfield(mp, INTERSITE_HDR, 1, (int*)0);
            CEVENT(SITE_NO(ioqp->io_sid), C_NSENT);
        }
        hdr->is_abits = ioqp->io_abits;
        hdr->is_aseqn = ioqp->io_iseqn;
        ioqp->io_wantack = 0;

        iovlen = msg_getiovlen(mp);

# ifdef SCATTERSEND
        if(staged_xmit || iovlen > MAX_UDP_IOVLEN)
# endif SCATTERSEND
        {
            register char *sbp = staging_buf;
            register iovec *iovp = msg_getiovec(mp);
            register len = msg_getlen(mp);
            while(iovlen--)
            {
                bcopy(iovp->iov_base, sbp, iovp->iov_len);
                sbp += iovp++ -> iov_len;
            }
#ifdef HPUX
loop:
            if(sendto(net_socket, staging_buf, len, 0, (struct sockaddr*)&ioqp->io_address, sizeof(saddr)) != -1)
                errno = 0;
            else if (errno == EWOULDBLOCK)
            {
                net_socket = isis_reset_sock(net_socket);
                goto loop;
            }
#else HPUX
            if(sendto(net_socket, staging_buf, len, 0, (struct sockaddr*)&ioqp->io_address, sizeof(saddr)) != -1)
                errno = 0;
#endif HPUX
        }

# ifdef SCATTERSEND
        else
        {
            static struct msghdr mh;
            mh.msg_name = (caddr_t)&ioqp->io_address;
            mh.msg_namelen = sizeof(saddr);
            mh.msg_iov = msg_getiovec(mp);
            mh.msg_iovlen = iovlen;
            if(sendmsg(net_socket, &mh, 0) != -1)
                errno = 0;
        }
# endif SCATTERSEND

        if(errno == 0 || errno == ECONNREFUSED || errno == ENOBUFS) 
        {
            if(++net_xmits > 1)
                net_drain_any_input();
            return;
        }
        perror("send");
  }

/* Input all pending network packets and tank them. */
net_drain_any_input()
  {
        bis(&tank_mask, net_socket);
        while(tank_status() != TANK_FULL && select(MAXBITS, &tank_mask, (int*)0, (int*)0, &tank_wait) == 1)
        {
            Gettime();
            net_drain_one_input(ACCEPT);
        }
  }

net_drain_input()
  {
        register count = 0;
        bis(&tank_mask, net_socket);
        if(tank_status() != TANK_FULL)
            do
            {
                if(count++)
                    Gettime();
                net_drain_one_input(ACCEPT);
            }
            while(tank_status() != TANK_FULL && select(MAXBITS, &tank_mask, (int*)0, (int*)0, &tank_wait) == 1);
  }


tank_status()
  {
        if(tank->size > tank->max_size)
            return(TANK_FULL);
        if(tank->size == 0)
            return(TANK_EMPTY);
        return(TANK_OTHER);
  }

static	just_warped;

net_drain_one_input(flag) 
  {
        int nb, fromlen;
        saddr from;
        long *msg_buf, len;
        block_desc *blk_desc;
        message *mp;

	if(just_warped)
	{
	    /* pr_main will not know we drained in net_timewarp */
	    --just_warped;
	    return;
	}
        net_xmits = 0;
        fromlen = sizeof(from);
        nb = recvfrom(net_socket, &len, sizeof(long), MSG_PEEK, (struct sockaddr*)&from, &fromlen);
        if(nb != sizeof(long))
            return;
        len = ntohl (len);
        if(len >= (MAXMSGLEN+128) || (blk_desc = msg_malloc(len)) == (block_desc*)0)
	    panic("net_drain: bad message length (%d)!\n", len);
        msg_buf = (long *)msg_body(blk_desc);
        fromlen = sizeof(from);
        nb = recvfrom(net_socket, msg_buf, len, 0, (struct sockaddr*)&from, &fromlen);
        if(nb != len)
        {
            perror("net_drain_input -- recvfrom");
            panic("net_drain_input wanted %d received %d", len, nb);
        }
  	if(flag == DISCARD)
            return;
	blk_desc->blk_avail -= nb;
        mp = msg_reconstruct(blk_desc);
        if(mp == (message*)0)
        {
            print("net_drain_input -- can't reconstruct message\n");
            return;
        }
        msg_tank_enqueue(tank, mp);
  }

/* Read and process a message from the network input tank. */
/* Returns TRUE if we processed a message, FALSE if there are no messages to process. */
net_deliver()
  {         
        register ioq *ioqp;
        register intersite *is;
        message *mp;
        static in_net_input, strange_id;

        if ((mp = msg_tank_dequeue(tank)) == (message *) 0)
          return(TANK_EMPTY);

        if(in_net_input++)
            panic("recursion (%d) in net_input", in_net_input);
        if((is = (intersite*)msg_getfield(mp, INTERSITE_HDR, 1, (int*)0)) == 0)
        {
            print("net_input -- can't extract intersite-hdr\n");
            msg_delete(mp);
            --in_net_input;
            return(TANK_OTHER);
        }
        EVENT(S_IGOT);
        CEVENT(SITE_NO(is->is_from), C_NGOT);
        ioqp = &ioqs[SITE_NO(is->is_from)];
        if(is->is_seqn == SEQ_DEAD)
        {
            if(is->is_dest == my_site_id)
                fd_iamdead(ioqp->io_sid);
            msg_delete(mp);
            --in_net_input;
            return(TANK_OTHER);
        }

        if(SITE_INCARN(is->is_from) != RECOVERY_INCARN && my_site_incarn != RECOVERY_INCARN)
        {
            /* Normal case -- I'm ok, you're ok */
            if(is->is_dest != my_site_id)
            {
                /* Well, maybe you're not ok. */
                print("Got a message to %x but I am %x\n", is->is_dest, my_site_id);
            discard:
                msg_delete(mp);
                --in_net_input;
                return(TANK_OTHER);
            }
            else if(ioqp->io_sid != is->is_from)
            {
                if(ioqp->io_dead && ioqp->io_dead == is->is_from)
                {
                    deadinf->is_dest = is->is_from;
                    net_xmit(ioqp, deadpacket);
                    print("YOU ARE DEAD: %x\n", is->is_from);
                }
                else
                    print("DISCARD: %x expected %x RECOVERY_INCARN %d\n", is->is_from, ioqp->io_sid, RECOVERY_INCARN);
                goto discard;
            }
        }
        else if(SITE_INCARN(is->is_from) == RECOVERY_INCARN)
        {
            if(my_site_incarn == RECOVERY_INCARN || is->is_dest != my_site_id || fd_coordinator == 0)
            {
                if(my_site_incarn == RECOVERY_INCARN)
                    print("Got a recovery request during recovery\n");
                else if(is->is_dest != my_site_id)
                    print("Got a message to %x but I am %x\n", is->is_dest, my_site_id);
                else
                    print("Got a recovery message but I am not the coordinator\n");
                goto discard;
            }
            if((ioqp->io_state&IO_ESTAB) || (is->is_seqn == 1 && SITE_INCARN(ioqp->io_sid) != RECOVERY_INCARN))
            {
                if((ioqp->io_state&(IO_ALIVE|IO_TOLDFD)) == IO_ALIVE)
                {
                    print("Site %d/%d seems to have died (unexpectedly saw RECOVERY request)\n", SITE_NO(ioqp->io_sid),SITE_INCARN(ioqp->io_sid));
                    ioqp->io_state |= IO_TOLDFD;
                    fd_seemsdead(ioqp->io_sid);
                }    
            }
        }
        else if(my_site_incarn == RECOVERY_INCARN)
        {
            if(is->is_seqn != 1 || is->is_from != ioqp->io_sid)
                goto discard;
            ioqp->io_state |= IO_ESTAB;
        }
        else
            ioqp->io_state |= IO_ESTAB;
        if((ioqp->io_state&IO_ALIVE) == 0)
            net_isalive(is->is_from);
        if((ioqp->io_state&IO_TOLDFD) && SITE_INCARN(is->is_from) != RECOVERY_INCARN)
        {
            msg_delete(mp);
            --in_net_input;
            return(TANK_OTHER);
        }
        ioqp->io_lastin = ISIS_TIME;
        if(is->is_seqn != SEQ_ACK)
        {
            register message **in;
            register len = msg_getlen(mp);
            if(len < MAXSIZE/WSIZE)
                len = MAXSIZE/WSIZE;
	    ioqp->io_wantack += len;
            /* Discard duplicate input messages */
            if(prio_is_gt(is->is_abprio, ab_priority))
                ab_priority = is->is_abprio;
            in = &ioqp->io_in[is->is_seqn&MAXWMASK];
            if(INRANGE(INC(ioqp->io_iseqn, 1), INC(ioqp->io_iseqn, MAXWSIZE), is->is_seqn) && *in == 0)
            {
                ++ioqp->io_ndata;
                *in = mp;
                msg_increfcount(mp);
            }
            else
            {
                ++ioqp->io_ndups;
                CEVENT(SITE_NO(ioqp->io_sid), C_NDUP);
            }
        }

        /* Briefly supress output */
        ++force_piggybacking;
        peek_ack(is);
        if(is->is_seqn != SEQ_ACK && in_net_input == 1)
        {
            register n;
            register message **in;
            while(*(in = &ioqp->io_in[INC(ioqp->io_iseqn, 1)&MAXWMASK]))
            {
                message *msgs[MAXMSGS];
                register m;
                register intersite *iis = (intersite*)msg_getfield(*in, INTERSITE_HDR, 1, (int*)0);
                if(iis->is_viewid != current_view.sv_viewid && iis->is_viewid != strange_id)
                    fd_strangeviewid(strange_id = iis->is_viewid);
                /* Now process the input messages */
                ioqp->io_iseqn = INC(ioqp->io_iseqn, 1);
                ioqp->io_abits >>= 1;
                m = msg_getmsgs(*in, INTERSITE_MSG, msgs, MAXMSGS);
                ioqp->io_nmsgs += m;
                for(n = 0; n < m; n++)
                {
                    if(ioqp->io_state&IO_ASSEM)
                    {
#                       ifdef INTERVERBOSE
                            print("*** NET: TRY TO REASSEMBLE "); pmsg(msgs[n]);
#                       endif INTERVERBOSE
                        fragman(msgs[n]);
                    }
                    else
                    {
#                       ifdef INTERVERBOSE
                            print("*** NET: TRY TO DELIVER "); pmsg(msgs[n]); msg_printaccess(msgs[n]);
#                       endif INTERVERBOSE
                        EVENT(S_IMSGS);
                        CEVENT(SITE_NO(ioqp->io_sid), C_NMSG);
                        pr_local_delivery(msgs[n], (int*)0);
                    }
                    msg_delete(msgs[n]);
                }
                if((ioqp->io_state&(IO_INUSE|IO_DEAD)) != IO_INUSE)
                {
                    /* Special for site shutdown while loop ran above */
                    --force_piggybacking;
                    --in_net_input;
                    msg_delete(mp);
                    return(TANK_ATEONE);
                }
                msg_delete(*in);
                *in = (message*)0;
            }
        }
        if(ioqp->io_backed && (ioqp->io_nslots && ioqp->io_nbytes < PK_LOWAT))
            net_drain(ioqp);
        else
            CEVENT(SITE_NO(ioqp->io_sid), C_NGACK);
        t_suspend("snd_ack");
        --force_piggybacking;
        if(net_urgent)
        {
            register qnode *qp;
            for(qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next)
                flush_urgent(qp->qu_ioq);
        }
        if(ioqp->io_wantack > MAXSIZE/2)
	{
            ++ioqp->io_nacks;
            net_xmit(ioqp, ackpacket);
	}
        else
	    ttid = timeout_reschedule(ttid, 1*sec, net_sweep, 0, 0);
        msg_delete(mp);
        --in_net_input;
        return(TANK_ATEONE);
  }

/* Glimpse of ACK info in arriving packet: `processed up to ... (non-incl.) received ....' */
peek_ack(is)
  register intersite *is;
  {
        register ioq *ioqp = &ioqs[SITE_NO(is->is_from)];
        register packet *out;
        register qnode *hp;
        register now = -1, aseqn, bn;

        if(is->is_from != ioqp->io_sid || (ioqp->io_state&IO_DEAD))
            return;
        bn = 1;
        aseqn = INC(is->is_aseqn, 1);
        if(is->is_abits && ioqp->io_oseqn != ioqp->io_nseqn && INRANGE(ioqp->io_oseqn, DEC(ioqp->io_nseqn), aseqn))
            while(bn && is->is_abits >= bn)
            {
                 if(is->is_abits&bn)
                 {
                     register p = aseqn&WMASK;
                     if((ioqp->io_out[p].pk_state&(PK_INUSE|PK_SENT)) == (PK_INUSE|PK_SENT))
                         ioqp->io_out[p].pk_state |= PK_ACKED;
                 }
                 aseqn = INC(aseqn, 1);
                 bn <<= 1;
            }

        /* regenerate ack info */
        if((is->is_seqn&SEQ_SPCL) == 0)
            if(INRANGE(INC(ioqp->io_iseqn, 1), INC(ioqp->io_iseqn, MAXWSIZE), is->is_seqn))
            {
                register n = is->is_seqn-ioqp->io_iseqn-1;
                if(n < 0)
                    n += MAXSEQN+1;
                ioqp->io_abits |= 1<<n;
            }

        aseqn = is->is_aseqn;
        /* Clear output slots if I can safely do so */
        while(ioqp->io_oseqn != ioqp->io_nseqn && INRANGE(ioqp->io_oseqn, DEC(ioqp->io_nseqn), aseqn))
        {
            register dt;
            out = &ioqp->io_out[ioqp->io_oseqn&WMASK];
            if(out->pk_state&(PK_INUSE|PK_SENT) != (PK_INUSE|PK_SENT))
                panic("Unexpected output state %x\n", out->pk_state);
            ++ioqp->io_nslots;
            ioqp->io_nbytes -= msg_getlen(out->pk_packet);
            intersite_congested -= msg_getlen(out->pk_packet);
            ioqp->io_oseqn = INC(ioqp->io_oseqn, 1);
            msg_delete(out->pk_packet);
            out->pk_packet = 0;
            out->pk_state = 0;
            if(out->pk_nret == 0)
            {
                if(now == -1)
                    now = ISIS_TIME;
                if((dt = now-out->pk_whensent+1*sec) <= 2*sec)
                    dt = 3*sec;
                if((ioqp->io_rtdelay = ((ioqp->io_rtdelay<<3)-ioqp->io_rtdelay+dt)>>3) < MIN_RET)
		    ioqp->io_rtdelay = MIN_RET;
            }
            while(hp = qu_head(out->pk_contents))
            {
                (*hp->qu_callback)(ioqp->io_sid, hp->qu_args[0], hp->qu_args[1]);
                qu_free(hp);
            }
        }
	/* Prompt handling of NACK's */
        begin
	{
            register n, unacked, acked_lo, acked_hi;
            unacked = 0;
            acked_hi = acked_lo = -1;
            /* Scan to see if there are any nacked packets */
            for(n = ioqp->io_oseqn; n != ioqp->io_nseqn; n = INC(n, 1))
            {
                register flag;
                out = &ioqp->io_out[n&WMASK];
                if(((flag = out->pk_state&(PK_INUSE|PK_ACKED))&PK_INUSE) == 0)
                    continue;
                if(unacked && flag == (PK_INUSE|PK_ACKED))
                {
                    /*
                     * Here if saw ... non-acked .... sent|acked
                     * treat the later ack as an implicit nack on prior packet
                     */
                    if(acked_lo == -1)
                        acked_lo = n;
                }
                else if(out->pk_nret == 0 && flag == PK_INUSE)
                {
                    /* Found a non-acked packet, but may not have been nacked */
                    ++unacked;
                    if(acked_lo != -1)
                    {
                        /* This is if we previously saw a nack-ack sequence */
                        acked_hi = acked_lo;
                        acked_lo = -1;
                    }
                }
            }
            if(acked_lo == -1)
                acked_lo = acked_hi;
            if(unacked == 0 || acked_lo == -1)
                return;
            /* the nacked ones are in the span ioseqn...acked_lo */
            for(n = ioqp->io_oseqn; n != acked_lo; n = INC(n, 1))
            {
                out = &ioqp->io_out[n&WMASK];
                if(out->pk_nret == 0 && (out->pk_state&(PK_INUSE|PK_ACKED)) == PK_INUSE)
                {
                    ++out->pk_nret;
                    out->pk_rtwhen = ISIS_TIME+ioqp->io_rtdelay/out->pk_nret;
                    net_xmit(ioqp, out->pk_packet);
                    ++ioqp->io_nret;
                }
            }
	}
  }

net_learned_incarn()
  {
        print("Learned my correct incarnation: I am site %d/%d\n", SITE_NO(my_site_id), SITE_INCARN(my_site_id));
        msg_setsender (ackpacket, &my_address);
        msg_setsender (deadpacket, &my_address);
  }

/* Invoked when halting, true if nothing unacknowledged on any qnode */
net_quiet()
  {
        register qnode *qp;
        for(qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next)
        {
            register ioq *ioqp = qp->qu_ioq;

            if(ioqp->io_nslots != WSIZE)
                return(0);
        }
        return(1);
  }

lazy_flush(arg)
  {
        register qnode *qp;
        static will_flush;
        if(arg == FLUSH_ON)
        {
            /* Just turn flush on */
            if(will_flush++ == 0)
                timeout(5*sec, lazy_flush, DO_FLUSH, 0);
            return;
        }
        will_flush = 0;
        for(qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next)
        {
            register ioq *ioqp = qp->qu_ioq;
            if(ioqp->io_state&IO_ACCUM)
                net_output(ioqp, ioqp->io_accum);
        }
  }


site_id on_my_right;

/* Version invoked in "slow" mode */
net_hello()
  {
        hello_delay = 0;
        net_sweep();
  }


/* 
 *      Invoked every second, approximately.
 *      Notice failed remote after 20secs, trigger failure detection
 */
net_sweep()
  {
        register qnode *qp, *nqp;
        site_id slist[MAX_SITES];
        register site_id *sp = slist;
        register now;
        int stuff_to_send = 0;

        ttid = 0;
        now = ISIS_TIME;

        /* Rote IOQS entries for fairness */
        if(qp = qu_head(IOQS))
        {
            qu_remove(qp);
            qu_append(IOQS, qp);
        }

        for(qp = IOQS->qu_next; qp != IOQS; qp = nqp)
        {
            register ioq *ioqp = qp->qu_ioq;
            register packet *out;
            register n;

            nqp = qp->qu_next;
            if((ioqp->io_state&(IO_ALIVE|IO_TOLDFD)) == IO_ALIVE)
            {
                if((ioqp->io_sid == on_my_right || bit(&proposed_recovered, SITE_NO(ioqp->io_sid))) && --hello_delay <= 0)
                {
                    /* Force something into the channel if necessary */
                    if(ioqp->io_nslots == WSIZE)
                        net_trytosend(hello, ioqp, FALSE, 0, 0, 0);
                    hello_delay = FDTIMEOUT;
                }
                /* Retransmit as needed */
                for(n = ioqp->io_oseqn; n != ioqp->io_nseqn; n = INC(n, 1))
                {
                    out = &ioqp->io_out[n&WMASK];
                    if((out->pk_state&(PK_INUSE|PK_ACKED)) == PK_INUSE)
                    {
                        ++stuff_to_send;
                        if(now > out->pk_rtwhen)
                        {
			    if(out->pk_nret == MAX_RET)
			    {
				print("Transmitted same packet %d times, giving up (len %d)\n", MAX_RET, msg_getlen(out->pk_packet));
				goto failed;
			    }
			    else
                                out->pk_rtwhen = now+20*sec;
                            net_xmit(ioqp, out->pk_packet);
                            ++ioqp->io_nret;
			    ++out->pk_nret;
                        }
                    }
                }
            }
            if((ioqp->io_state&(IO_ESTAB|IO_ALIVE|IO_TOLDFD)) == (IO_ESTAB|IO_ALIVE))
            {
                register t = now-ioqp->io_lastin;
                /* Detect failures */
                if(ioqp->io_nslots < WSIZE && t >= 30*sec && t >= (ioqp->io_rtdelay*TIME_FAIL))
		{
		    print("Timeout: site %d/%d unresponsive for %d secs\n", SITE_NO(ioqp->io_sid),SITE_INCARN(ioqp->io_sid), t/sec);
                    goto failed;
		}
            }
            if((ioqp->io_state&(IO_ESTAB|IO_ALIVE|IO_TOLDFD)) == IO_ALIVE)
            {
                /* Detect failures during startup protocol */
		if(ioqp->io_nslots < WSIZE && now-ioqp->io_lastin >= TIME_ESTAB)
                {
		    print("Timeout during restart: connection not established after %d secs\n",
			(now-ioqp->io_lastin)/sec);
		failed:
                    print("Site %d/%d seems to have died\n", SITE_NO(ioqp->io_sid),SITE_INCARN(ioqp->io_sid));
                    ioqp->io_state |= IO_TOLDFD;
                    *sp++ = ioqp->io_sid;
                }
            }
        }
        for(qp = IOQS->qu_next; qp != IOQS; qp = nqp)
        {
            register ioq *ioqp = qp->qu_ioq;
            nqp = qp->qu_next;
            if(ioqp->io_wantack)
            {
                ++ioqp->io_nacks;
                net_xmit(ioqp, ackpacket);
            }
        }
        *sp = 0;
        if(sp != slist)
            fd_seemdead(slist);
        if(ttid)
            return;
        if(stuff_to_send)
            ttid = timeout(1*sec, net_sweep, 0, 0);
        else 
        {
            if(hello_delay == 0)
                hello_delay = FDTIMEOUT;
            ttid = timeout(hello_delay*1*sec, net_hello, 0, 0);
        }
  }

net_timewarp(delta)
  register delta;
  {
        register qnode *qp;
        for(qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next)
        {
            register i;
            register ioq *ioqp = qp->qu_ioq;
            ioqp->io_lastin += delta;
            for(i = 0; i != WSIZE; i++)
            {
                register packet *out = &ioqp->io_out[INC(ioqp->io_oseqn, i)&WMASK];
                if(out->pk_packet)
                {
                    out->pk_rtwhen += delta;
                    out->pk_whensent += delta;
                }
            }
        }
        bis(&tank_mask, net_socket);
        while(select(MAXBITS, &tank_mask, (int*)0, (int*)0, &tank_wait) == 1)
            net_drain_one_input(DISCARD);
	++just_warped;
  }

net_newview()
  {
        register site_id *sp;
        print("Site view %d/%d:", current_view.sv_viewid&0xFF, current_view.sv_viewid>>8);
        for(sp = current_view.sv_slist; *sp; sp++)
            if(SITE_INCARN(*sp)) 
                print(" %d/%d", SITE_NO(*sp), SITE_INCARN(*sp));
            else 
                print(" %d", SITE_NO(*sp)); 
        print("\n");
        for(sp = current_view.sv_slist; *sp; sp++)
            if(*sp == my_site_id)
                break;
        if(*++sp == 0)
            sp = current_view.sv_slist;
        if(*sp != my_site_id)
            on_my_right = *sp;
        else
            on_my_right = 0;
  }

dump_intersite()
  {
        register ioq *ioqp;

        print("Intersite: ");
        if(force_piggybacking)
            print("forced_piggybacking enabled (%d)\n", force_piggybacking);
        else
            print("\n");
        for(ioqp = ioqs; ioqp != &ioqs[MAX_SITES]; ioqp++)
            ioq_dump(ioqp);
        msg_tank_dump(tank);
  }

ioq_dump(ioqp)
  register ioq *ioqp;
  {
        register packet *out;
        register p, now = ISIS_TIME;
        if(ioqp->io_state)
        {
            print("  %d/%d\t[%s]:\n\t", SITE_NO(ioqp->io_sid), SITE_INCARN(ioqp->io_sid), site_names[SITE_NO(ioqp->io_sid)]);
            if(ioqp->io_state)
            {
                if(ioqp->io_state&IO_ESTAB)
                    print("estab;");
                if(ioqp->io_state&IO_ALIVE)
                    print("alive;");
                if(ioqp->io_state&IO_ACCUM)
                    print("accum;");
                if(ioqp->io_state&IO_URGENT)
                    print("urg;");
                if(ioqp->io_state&IO_ASSEM)
                    print("assem;");
                if(ioqp->io_state&IO_TOLDFD)
                    print("seems dead;");
                if(ioqp->io_state&IO_DEAD)
                    print("is dead;");
            }
            if(ioqp->io_dead)
                print(" <rip %d/%d>", SITE_NO(ioqp->io_dead), SITE_INCARN(ioqp->io_dead));
            print(" got: %d/%d+%d dups, sent: %d+%d ret, %d acks, ", ioqp->io_ndata, ioqp->io_nmsgs, ioqp->io_ndups, ioqp->io_nsent, ioqp->io_nret, ioqp->io_nacks);
            print(" backlog %d\n", ioqp->io_backed);
            for(p = ioqp->io_oseqn; p != ioqp->io_nseqn; p = INC(p, 1))
            {
                int ws, whs;
                out = &ioqp->io_out[p&WMASK];
                print("\t... packet %d = sl%d,sz%d,cnt%d", p, out-ioqp->io_out, out->pk_size, out->pk_cnt);
                if((out->pk_state&PK_ACKED) == 0)
                {
                    ws = now-out->pk_whensent;
                    whs = ws/10 % 100;
                    ws /= 1*sec;
                    print(", nret%d, waiting %d.%.2d, ", out->pk_nret, ws, whs);
                    ws = out->pk_rtwhen-now;
                    whs = ws/10 % 100;
                    ws /= 1*sec;
                    print("next ret in %d.%.2d", out->pk_nret, ws, whs, out->pk_rtwhen-now);
                }
                print(", state=<");
                if(ioqp->io_accum == out)
                    print("accum;");
                if(out->pk_state&PK_INUSE)
                    print("inuse;");
                if(out->pk_state&PK_SENT)
                    print("sent;");
                if(out->pk_state&PK_ACKED)
                    print("acked;");
                print(">\n");
            }
            while(p != INC(ioqp->io_oseqn, WSIZE))
            {
                out = &ioqp->io_out[p&WMASK];
                if(out->pk_packet && out->pk_state == PK_INUSE && p == ioqp->io_nseqn)
                    print("\t... packet %d under construction\n", p);
                    
                else
                {
                    if(out->pk_packet)
                        print("\t*** FOUND A PACKET IN SEQN %d... oseqn %d nseqn %d!\n", p, ioqp->io_oseqn, ioqp->io_nseqn);
                    if(out->pk_state)
                        print("\t*** FOUND STATE %x in SEQN %d!... oseqn %d nseqn %d\n", out->pk_state, p, ioqp->io_oseqn, ioqp->io_nseqn);
                }
                p = INC(p, 1);
            }
            for(p = 0; p < MAXWSIZE; p++)
                if(ioqp->io_in[p])
                    break;
            if(p == MAXWSIZE)
                return;
            print("\t... full input slots: seqn ");
            for(p = 1; p <= MAXWSIZE; p++)
                if(ioqp->io_in[INC(ioqp->io_iseqn,p)&MAXWMASK])
                    print("%d ", INC(ioqp->io_iseqn,p));
            print("\n");
        }
  }

/* Dump contents of an inter-site packet */
is_dump(why, mp)
  char *why;
  register message *mp;
  {
        register intersite *is;
        register n, m;
        message *msgs[MAXMSGS];
        is = (intersite*)msg_getfield(mp, INTERSITE_HDR, 1, (int*)0);
        m = msg_getmsgs(mp, INTERSITE_MSG, msgs, MAXMSGS);
        print("IS_DUMP: %s msg %x<%d> len %d", why, mp, m, msg_getlen(mp));
        if(is == 0)
            print("[no header]\n");
        else
        {
            switch(is->is_seqn)
            {
              case SEQ_DEAD: print("[dead msg]\n"); goto phd;
              case SEQ_ACK: print("[from %x ACK, aseqn %d abits %x]\n", is->is_aseqn, is->is_abits); goto phd;
            }
            print("[from %x seqn %d aseqn %d abits %x]\n", is->is_from, is->is_seqn, is->is_aseqn, is->is_abits);
        }
        for(n = 0; n < m; n++)
        {
            print("  <%d>: mp %x = ", n, msgs[n]);
            pmsg(msgs[n]);
            msg_delete(msgs[n]);
        }
  phd:  msg_printheader(mp); msg_printaccess(mp);
  }

set_isis_sockopts(sock)
  register sock;
  {
  int ioarg, retval;
#       ifdef HPUX
	{
#           ifdef SO_SNDBUF
	    {
		int size = 8*1024;
		setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &size, sizeof(size));
		size = 8*1024;
		setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &size, sizeof(size));
	    }
#           endif SO_SNDBUF
#           ifdef SO_BURST_OUT
	    {
		int val = 7;
		setsockopt(sock, SOL_SOCKET, SO_BURST_OUT, &val, sizeof(val));
	    }
#           endif SO_BURST_OUT
            /* This ioctl call eliminates the blocking done by HPUX sendto command */
            ioarg = 1;
            retval = ioctl(sock, FIOSNBIO, &ioarg);
            if (retval == -1)
                perror("ioctl:"); 
	}
#       else HPUX
	{
#           ifdef SO_SNDBUF
            {
                int size = 16*1024;
                setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &size, sizeof(size));
                size = 16*1024;
                setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &size, sizeof(size));
            }
#           endif SO_SNDBUF
	}
#       endif HPUX
  }


#ifdef HPUX

static saddr sin;

/* Want to close/reopen same socket and port no */
/* As workaround to HPUX sendto bug             */
int isis_reset_sock(sock)
  int sock;
  {
        saddr name;
        int save_sock, new_sock;
        int namelen = sizeof(saddr);
   
        save_sock = sock;
        if (getsockname(sock, &name, &namelen) == -1)
           panic("getsockname failed in isis_reset_sock");
        
        close(sock);
        sock = socket(AF_INET, SOCK_DGRAM, 0);

        if (sock != save_sock)
           {
           /* must be same socket number for inter_client, so dup2 */
           if ((new_sock = dup2(sock, save_sock)) == -1)
              panic("dup2 failed in isis_reset_sock");
           
           close(sock);
           sock = new_sock;
           }

        sin.sin_family = AF_INET;
        sin.sin_port = name.sin_port;

        if (bind(sock, (struct sockaddr_in*)&sin, sizeof(sin)) == -1)
        {
            printf("Can't allocate UDP port %d! \r\n", ntohs(sin.sin_port));
            exit(0);
        }
        set_isis_sockopts(sock);

        return(sock);
  }
#endif HPUX
