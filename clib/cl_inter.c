/*  $RCSfile: cl_inter.c,v $ $Revision: 2.114 $ $Date: 90/09/15 20:42:32 $  */

/*
 *	Originally coded by Ken Birman.  Mixture of transport protocol 0 and
 *	bypass-specific stuff; NOT useful as a model of how to build a
 *      new transport protocol (sorry).
 *
 *      UDP code for direct transmission between ISIS clients
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

# define  ISIS_SYS
# include "isis.h"

struct hostent *gethostbyname();
struct servent *getservbyname();

#if     (HPUX)
#define   sendmsg(x, y, z)      panic("sendmsg was called\n")
#endif

/* Normal mode: rapid communication with the site */
#define TIME_FAIL             10	/* Detect failures after 10 times the rtdelay */
#define TIME_ESTAB          5000	/* Give up on connection estab. after 5 secs */
#define TIME_RET            4000	/* Initially, retransmit after 4secs */

int intercl_xmits;
static void fragman();
static void isis_fragment();
static int intercl_trytosend(), intercl_output();
static void intercl_xmit(), intercl_drain_one_input(), intercl_accept();
static void ioq_dump(), msg_tank_dump();
static message *msg_tank_dequeue();
static msg_tank *msg_tank_create();
static ttid;
static will_flush, will_check;
static message *ackiclpkt;
static interclient ic_info, *ackinf;
static msg_tank *tank;			/* FIFO of incoming messages not yet sent to destination
					   process. */
static struct timeval tank_wait = { 0, 0 };

static message *ic_newmsg();
static qnode *intercl_waitq, *IOQS, *ZOMBIES, *KEEPERS;
static intercl_unblocking;
qnode *mother_msg_queue;
static ioq *mother_iqp;
int tank_status;
char staging_buf[MAXMSGLEN];
block_desc *isis_msgblk;

int send_outstanding;			/* # of bytes waiting to send */
int send_congest;

#define CONGEST_HIGH 16000
#define CONGEST_LOW  8000

#define set_tank_status()                                                \
  {                                                                      \
        if(tank->size > tank->max_size)                                  \
            tank_status = TANK_FULL;                                     \
        else if(tank->size == 0)                                         \
            tank_status = TANK_EMPTY;                                    \
        else                                                             \
	    tank_status = TANK_OTHER;                                    \
  }

static adesc ic_adesc = { sizeof(mdesc), 0, 16, };

#define mdalloc()        ((mdesc*)mallocate(&ic_adesc))
#define mdfree(qp)       mdeallocate((char*)qp,&ic_adesc)

#define MAXSEQN         0x7f
#define SEQN(x)         ((x)&MAXSEQN)
#define INC(x, i)       SEQN(x+i)
#define DEC(x)          INC(x, MAXSEQN)

/* Decide if <sn> is between <lb> and <hb> */
static int
INRANGE(lb, hb, sn)
	register lb, hb, sn;
{
	if (lb == hb)
		return (sn == lb);
	if ((lb < hb) ? (sn < lb || sn > hb) : (sn > hb && sn < lb))
		return (0);
	return (1);
}

static qnode *frgq;

void
intercl_init()
{
	void check_nacks(), intercl_dequeue(), intercl_sweep(), lazy_flush();

	frgq = qu_null();
	tank = msg_tank_create(250000);
	isis_entry(GENERIC_CL_FRAGMAN, fragman, "fragment reassembly");
	isis_task(check_nacks, "clib:check_nacks");
	isis_task(intercl_dequeue, "clib:intercl_dequeue");
	isis_task(intercl_sweep, "clib:intercl_sweep");
	isis_task(lazy_flush, "clib:lazy_flush");
	ic_info.ic_from = my_address;
	ic_info.ic_seqn = SEQ_ACK;
	ackiclpkt = ic_newmsg();
	ackinf = (interclient *) msg_getfield(ackiclpkt, INTERCLIENT_HDR, 1, (int *) 0);
	isis_msgblk = msg_malloc(MAXMSGLEN);
	IOQS = qu_null();
	KEEPERS = qu_null();
	ZOMBIES = qu_null();
	intercl_waitq = qu_null();
	intercl_sweep();
	set_tank_status();
}

struct adesc io_ad = { sizeof(ioq), sizeof(ioq), 4 };

#define io_alloc()   ((ioq*)mallocate(&io_ad))

void
io_free(ioqp)
	register ioq *ioqp;
{
	mdeallocate((char *) ioqp, &io_ad);
}

/* These were private to the next routine.  They've been made external */
/* so that other transport routines can get the same information */
/* without duplicating the work done here.  This assumes that */
/* intercl_isalive is called before the appropriate routine for other */
/* transport protocols */

char hknown[MAX_SITES];
saddr addr_by_site[MAX_SITES];

saddr *
get_addr_by_address(addr, portno)
	address *addr;
	register portno;
{
	register struct hostent *hep;
	register char *sp;
	register site = 0;

	if (my_process_id != ISIS && addr && addr_isrclient(addr)) {
		static qnode *raddrs;
		register qnode *qp;
		register message *mp, *rmp;
		saddr *raddr = NULL;
		extern ISIS_PORTNO;

		if (raddrs == 0)
			raddrs = qu_null();
		else if (qp = pg_find(raddrs, addr))
			return ((saddr *) qp->qu_data);
		mp = msg_gen("%d,%A[1]", ISIS_M_LOOKUP, addr);
		rmp =
		    udp_rpc(0, site_names[addr->addr_site], (saddr *) 0, addr->addr_site,
			    ISIS_PORTNO, mp);
		msg_delete(mp);
		if (rmp) {
			void free();

			msg_get(rmp, "%+C", &raddr, (int *) 0);
			pg_add(raddrs, addr, raddr, (vfunc *) free);
			msg_delete(rmp);
		}
		if (rmp == 0 || raddr->sin_port == 0) {
			print("ISIS client ");
			paddr(&my_address);
			print(" unable to contact ");
			paddr(addr);
			print("... continuing\n");
			if (raddr)
				free(raddr);
			raddr = NULL;
		} else {
			raddr->sin_port = htons(portno);
			raddr->sin_family = AF_INET;
		}
		return (raddr);
	}
	if (addr)
		site = addr->addr_site;
	if (site == 0)
		gethostname(site_names[0], 64);
	if (hknown[site] == 0) {
		char str[64];

		strcpy(str, site_names[site]);
		for (sp = str; *sp; ++sp)
			if (*sp == '/')
				*sp-- = 0;
		if ((hep = gethostbyname(str)) == 0) {
			print("gethostbyname(3N): addr_site %s (isis addr_site no. %d) unknown\n",
			      str, site);
			if (site == 0)
				panic("get_addr_by_address");
			return ((saddr *) NULL);
		}
		hknown[site]++;
		addr_by_site[site].sin_family = AF_INET;
		bcopy(hep->h_addr, &addr_by_site[site].sin_addr, hep->h_length);
	}
	addr_by_site[site].sin_port = htons(portno);
	return (&addr_by_site[site]);
}

intercl_lookup(addr, raddr)
	register address *addr;
	register saddr *raddr;
{
	register ioq *ioqp;
	register qnode *iqp;

	if ((iqp = pg_find(IOQS, addr)) == 0)
		return (0);
	ioqp = iqp->qu_ioq;
	*raddr = ioqp->io_address;
	return (sizeof(ioqp->io_address));
}

/* Prepare to communicate with process `addr' */
void
intercl_isalive(gaddr, addr, inet_addr)
	address *gaddr, *addr;
	saddr *inet_addr;
{
	register ioq *ioqp;
	register saddr *raddr;
	register qnode *iqp, *nqp;

	if (aptr_isnull(addr) || addr_ismine(addr))
		return;
	if (iqp = pg_find(IOQS, addr)) {
		ioqp = iqp->qu_ioq;
		if (gaddr)
			pg_add(ioqp->io_reflist, gaddr, NULLARG, NULLROUTINE);
		return;
	}
	ioqp = io_alloc();
	ioqp->io_claddr = *addr;
	ioqp->io_state &= ~IO_DEAD;
	if (ioqp->io_state & IO_ALIVE)
		return;
	if (((raddr = inet_addr) == NULL) &&
	    (raddr = get_addr_by_address(addr, addr->addr_portno)) == NULL)
		return;
	ioqp->io_address = *raddr;
	ioqp->io_probefreq = 0;
	if (my_process_id != ISIS && addr_isrclient(addr)) {
		ioqp->io_probefreq = 60000;
		ioqp->io_probetimeout = 30000;
	}
	if ((ioqp->io_state & IO_INUSE) == 0) {
		register iclpkt *out;

		ioqp->io_state |= IO_INUSE;
		ioqp->io_backlog = qu_null();
		for (out = ioqp->io_out; out != &ioqp->io_out[WSIZE]; out++)
			out->pk_contents = qu_null();
	}
	ioqp->io_state |= IO_ALIVE;
	ioqp->io_wantack = 0;
	ioqp->io_spool = 0;
	ioqp->io_nslots = WSIZE;
	ioqp->io_iseqn = 0;
	ioqp->io_reflist = qu_null();
	if (gaddr)
		pg_add(ioqp->io_reflist, gaddr, NULLARG, NULLROUTINE);
	ioqp->io_oseqn = 1;
	ioqp->io_nseqn = 1;
	ioqp->io_ndups = ioqp->io_nacks = ioqp->io_nmsgs = ioqp->io_ndata = 0;
	ioqp->io_nret = ioqp->io_nsent = ioqp->io_abits = 0;
	ioqp->io_rtdelay = TIME_RET;
	ioqp->io_lastin = ISIS_TIME;
	pg_add_ioq(IOQS, addr, ioqp, (vfunc *) io_free);
	for (iqp = KEEPERS->qu_next; iqp != KEEPERS; iqp = nqp) {
		register message *mp;
		register interclient *ic;

		mp = iqp->qu_msg;
		ic = (interclient *) msg_getfield(mp, INTERCLIENT_HDR, 1, (int *) 0);
		nqp = iqp->qu_next;
		if (addr_isequal(addr, &ic->ic_from)) {
			intercl_accept(mp);
			qu_free(iqp);
		}
	}
	return;
}

intercl_ismother(paddr)
	register address *paddr;
{
	register qnode *qp;

	if (qp = pg_find(IOQS, paddr))
		mother_iqp = qp->qu_ioq;
}

/*
 *      These are the interesting routines.  They do iclpkted output
 *      and acknowledged input.  Fragment the iclpkt if necessary.
 */
int
intercl_trysend(paddr, mp)
	register address *paddr;
	register message *mp;
{
	register qnode *qp;
	static address dests[2];

	if (!addr_ismine(paddr)) {
		qp = pg_find(IOQS, msg_getsender(mp));
		if (qp == 0)
			return (0);
	}
	dests[0] = *paddr;
	msg_setdests(mp, dests);
	net_send(&NULLADDRESS, 0, mp, dests, (vfunc *) 0, (VOID *) 0, (VOID *) 0);
	return (1);
}

/*
 *   Trick to encourage piggybacking:
 *      force_piggybacking: set if all messages should be piggybacked for now 
 *      intercl_urgent: counts iclpkts containing urgent messages 
 *      ioqp->io_accum: identifies the corresponding urgent iclpkt, if any
 *      ioqp->io_state: IO_URGENT,IO_ACCUM set as needed
 */
static force_piggybacking, intercl_urgent;

void
net_send(gaddr, exmode, mp, to, callback, arg0, arg1)
	int exmode;
	register message *mp;
	register address *gaddr, *to;
	vfunc *callback;
	VOID *arg0, *arg1;
{
	static fragmenting;

	t_scheck();
	if (fragmenting == 0) {
		register count = 0;
		register address *ap;

		ap = to;
		while (!aptr_isnull(ap)) {
			if (addr_ismine(ap)) {
				if (exmode <= 0) {
					register message *msg = msg_copy(mp);

					isis_gotmsg(msg, BYP_CHECK, 0);
					msg_delete(msg);
					if (callback)
						(*callback) (ap, arg0, arg1);
				}
			} else
				++count;
			++ap;
		}
		if (count == 0)
			return;
		if (msg_getlen(mp) > MSG_MUSTFRAG) {
			force_lazy(TRUE);
			++fragmenting;
			isis_fragment(FRAGSIZE, net_send, gaddr, exmode, mp, to, callback, arg0,
				      arg1);
			--fragmenting;
			force_lazy(FALSE);
			return;
		}
	}
	while (!aptr_isnull(to)) {
		if (!addr_ismine(to)) {
			register qnode *qp = pg_find(IOQS, to);
			register ioq *ioqp;

			if (qp == 0) {
				register msgid = msg_getid(mp);

				if (exmode == -1 && (msgid & 1)) {	/* ISIS -> remote_xxx */
					register message *rmsg = msg_newmsg();

					msg_addfield(rmsg, FLD_ISNULLREP, 0, 0, 0);
					msg_setid(rmsg, msgid);
					msg_setsender(rmsg, to);
					fbcast_l("mR", msg_getreplyto(mp), GENERIC_RCV_REPLY, rmsg,
						 0);
				}
				if (callback)
					(*callback) (to, arg0, arg1);
			} else {
				ioqp = qp->qu_ioq;
				if (addr_cmp(to, &ioqp->io_claddr) == 0)
					(void) intercl_trytosend(mp, ioqp, FALSE, callback, arg0,
								 arg1);
				else if (callback)
					(*callback) (to, arg0, arg1);
			}
		}
		++to;
	}
	return;
}

static void
intercl_delete(item)
	register item;
{
	register qnode *qp;

	if (qp = qu_find(frgq, item))
		qu_free(qp);
}

/* Fragment this message into smaller pieces for transmission */
static void
isis_fragment(size, mt_send, gaddr, exmode, mp, to, callback, arg0, arg1)
	int size;
	void (*mt_send) ();
	message *mp;
	address *gaddr;
	int exmode;
	address *to;
	void (*callback) ();
	VOID *arg0, *arg1;
{
	register iovec *iovp;
	register iovl, nfrags, msgsize, firstfrag, iolen;
	char *iobase;
	message *fmp;
	fraghdr fhdr;
	address dests[MAX_SITES + 1];
	register address *dp, *dsite;

	iovp = msg_getiovec(mp);
	iovl = msg_getiovlen(mp);
	msgsize = 0;
	dp = dests;
	for (dsite = to; !aptr_isnull(dsite); ++dsite, ++dp) {
		*dp = *dsite;
		dp->addr_entry = GENERIC_CL_FRAGMAN;
	}
	*dp = NULLADDRESS;
	nfrags = (mp->msg_len + size - 1) / size;
	fhdr.fr_nfrags = nfrags;
	fhdr.fr_msglen = mp->msg_len;

	/* Now send <nfrags> messages containing data */
	firstfrag = 0;
	iolen = iovp->iov_len;
	iobase = iovp->iov_base;
	while (nfrags--) {
		register fragsize = 0, len;

		fmp = ic_newmsg();
		if (firstfrag == 0) {
			/* Only the first frag contains header */
			++firstfrag;
			msg_addfield(fmp, INTERCLIENT_FHDR, (char *) &fhdr, FTYPE_LONG,
				     sizeof(fhdr));
		}
		msg_setdests(fmp, dests);
		do {
			if ((len = iolen) > size - fragsize)
				len = size - fragsize;
			fragsize += len;
			msgsize += len;
			if (nfrags || msgsize < mp->msg_len)
				msg_insertref(fmp, INTERCLIENT_FRAG, iobase, FTYPE_CHAR, len,
					      (vfunc *) 0);
			else {
				/* Callback on very last fragment... */
				msg_increfcount(mp);
				qu_add_mp(frgq, (int) iobase, mp, MSG_DELETE);
				msg_insertref(fmp, INTERCLIENT_FRAG, iobase, FTYPE_CHAR, len,
					      intercl_delete);
			}
			if (iolen -= len)
				iobase += len;
			else {
				++iovp;
				iolen = iovp->iov_len;
				iobase = iovp->iov_base;
			}
		}
		while (fragsize < size && msgsize < mp->msg_len);
		if (nfrags)
			(*mt_send) (gaddr, exmode, fmp, dests, NULLROUTINE, NULLARG, NULLARG);
		else
			/* Callback on last fragment */
			(*mt_send) (gaddr, exmode, fmp, dests, callback, arg0, arg1);
		msg_delete(fmp);
	}
}

/*
 * Reassemble a fragmented message
 */
static void
fragman(fmp)
	message *fmp;
{
	register ioq *ioqp;
	register fraghdr *fh;
	static char *bptrs[256];
	static int blens[256];
	register i, n;
	qnode *qp;

	qp = pg_find(IOQS, msg_getsender(fmp));
	if (qp == 0)
		panic("Fragman");
	ioqp = qp->qu_ioq;
	fh = &ioqp->io_fhdr;
	if ((ioqp->io_state & IO_ASSEM) == 0) {
		fraghdr *fhdr = (fraghdr *) msg_getfield(fmp, INTERCLIENT_FHDR, 1, (int *) 0);

		if (fhdr == 0) {
			cl_dump(-1, "%x: duplicate fragment?", fmp);
			return;
		}
		*fh = *fhdr;
		ioqp->io_blk_desc = msg_malloc(fh->fr_msglen);
		ioqp->io_fbody = msg_body(ioqp->io_blk_desc);
		ioqp->io_fptr = ioqp->io_fbody;
		ioqp->io_state |= IO_ASSEM;
	}
	n = msg_getfields(fmp, INTERCLIENT_FRAG, bptrs, blens, 256);
	for (i = 0; i < n; i++) {
		bcopy(bptrs[i], ioqp->io_fptr, blens[i]);
		ioqp->io_fptr += blens[i];
	}
	/* If this was the last message, check for integrity and deliver it */
	if (--fh->fr_nfrags <= 0) {
		message *mp;

		ioqp->io_blk_desc->blk_avail -= fh->fr_msglen;
		if (mp = msg_reconstruct(ioqp->io_blk_desc)) {
			intercl_gotmsg(ioqp, mp, 0);
			msg_delete(mp);
		}
		ioqp->io_state &= ~IO_ASSEM;
		ioqp->io_fbody = ioqp->io_fptr = 0;
	}
}

/* Drain as many messages off the congestion qnode as possible, encourage piggybacking */
static void
intercl_drain(ioqp)
	register ioq *ioqp;
{
	register qnode *qp;
	register unsigned long len;

	force_lazy(TRUE);
	while (qp = qu_head(ioqp->io_backlog)) {
		register mdesc *md = qp->qu_md;

		if (intercl_trytosend(md->md_msg, ioqp, TRUE, md->md_cb, md->md_arg0, md->md_arg1)) {
			force_lazy(FALSE);
			if (ioqp->io_state & IO_URGENT)
				intercl_output(ioqp, ioqp->io_accum);
			return;
		}
		len = msg_getlen(md->md_msg);
		ioqp->io_backed -= len;
		send_outstanding -= len;
		if (send_congest && (send_outstanding < CONGEST_LOW || memalloc - memfree < 200000)) {
			send_congest = 0;
			if (--isis_ncongest == 0) {
				isis_state &= ~ISIS_CONGESTED;
				t_sig_all(&isis_decongested, 0);
			}
		}
		msg_delete(md->md_msg);
		mdfree(md);
		qu_free(qp);
	}
	force_lazy(FALSE);
}

/* Try to send this message, on congestion qnode if `tryingtodrain' */
static int
intercl_trytosend(mp, ioqp, tryingtodrain, callback, arg0, arg1)
	register message *mp;
	register ioq *ioqp;
	register tryingtodrain;
	void (*callback) ();
	VOID *arg0, *arg1;
{
	register size = msg_getlen(mp);
	register iclpkt *out = &ioqp->io_out[ioqp->io_nseqn & WMASK];

# ifdef INTERVERBOSE
	print("*** INTERCL: SEND to ", ioqp->io_claddr);
	pmsg(mp);
# endif
	if ((ioqp->io_state & (IO_INUSE | IO_DEAD)) != IO_INUSE)
		panic("trytosend: io_state %x", ioqp->io_state);
	msg_increfcount(mp);
	if (force_piggybacking && (ioqp->io_state & IO_ACCUM)) {
		/* Continue to accumulate iclpkt if possible */
		if (out->pk_size + size <= MAXSIZE) {
			if ((ioqp->io_state & IO_URGENT) == 0) {
				ioqp->io_state |= IO_URGENT;
				++intercl_urgent;
			}
			msg_addmsg(out->pk_iclpkt, INTERCLIENT_MSG, mp);
			if (callback)
				qu_add_cb(out->pk_contents, callback, arg0, arg1);
			msg_delete(mp);
			out->pk_size = msg_getlen(out->pk_iclpkt);
			if (++out->pk_cnt == MAXMSGS - 1 || out->pk_size >= MAXSIZE - SMLPKT)
				intercl_output(ioqp, out);
			return (0);
		}
	}

	/* Flush accum iclpkt before starting a new iclpkt */
	if (ioqp->io_state & IO_ACCUM) {
		intercl_output(ioqp, out);
		out = &ioqp->io_out[ioqp->io_nseqn & WMASK];
	}

	/* This code generates a new iclpkt for <mp> */
	if (ioqp->io_nslots == 0 || (ioqp->io_backed && tryingtodrain == FALSE)
	    || ioqp->io_nbytes > PK_HIWAT) {
		if (tryingtodrain == FALSE) {
			register mdesc *md = mdalloc();
			register int len;

			md->md_msg = mp;
			md->md_cb = callback;
			md->md_arg0 = arg0;
			md->md_arg1 = arg1;
			qu_add_md(ioqp->io_backlog, 1, md, NULLROUTINE);
			len = msg_getlen(mp);
			ioqp->io_backed += len;
			send_outstanding += len;
			if (!send_congest && send_outstanding >= CONGEST_HIGH
			    && (memalloc - memfree) > 400000) {
				++send_congest;
				++isis_ncongest;
				isis_state |= ISIS_CONGESTED;
			}
			return (0);
		}
		msg_delete(mp);
		return (-1);
	}
	ioqp->io_nslots--;
	out->pk_iclpkt = ic_newmsg();
	out->pk_state = PK_INUSE;
	out->pk_cnt = 1;
	msg_addmsg(out->pk_iclpkt, INTERCLIENT_MSG, mp);
	out->pk_size = msg_getlen(out->pk_iclpkt);
	if (callback)
		qu_add_cb(out->pk_contents, callback, arg0, arg1);
	if (force_piggybacking && size < MAXSIZE - SMLPKT) {
		/* Turn flush on */
		if (will_flush++ == 0)
			isis_ondrain(lazy_flush, 0);
		ioqp->io_state |= IO_ACCUM;
		ioqp->io_accum = out;
		if ((ioqp->io_state & IO_URGENT) == 0) {
			ioqp->io_state |= IO_URGENT;
			++intercl_urgent;
		}
	} else
		intercl_output(ioqp, out);
	msg_delete(mp);
	return (0);
}

static message *
ic_newmsg()
{
	register message *msg = msg_newmsg();

	msg_addfield(msg, INTERCLIENT_HDR, (char *) &ic_info, FTYPE_INTERCLIENT, sizeof(ic_info));
	return (msg);
}

/* First time transmission of a message */
static int
intercl_output(ioqp, out)
	register ioq *ioqp;
	register iclpkt *out;
{
	register message *mp = out->pk_iclpkt;
	register interclient *ic = (interclient *) msg_getfield(mp, INTERCLIENT_HDR, 1, (int *) 0);

	ioqp->io_nsent++;
	out->pk_state |= PK_SENT;
	ic->ic_seqn = ioqp->io_nseqn;
	ic->ic_from = my_address;
	ic->ic_dest = ioqp->io_claddr;
	out->pk_size = msg_getlen(mp);
	ioqp->io_nseqn = INC(ioqp->io_nseqn, 1);
	if (ioqp->io_accum == out) {
		if (ioqp->io_state & IO_URGENT)
			--intercl_urgent;
		ioqp->io_state &= ~(IO_ACCUM | IO_URGENT);
		ioqp->io_accum = 0;
	}
	ioqp->io_nbytes += msg_getlen(mp);
	out->pk_whensent = ISIS_TIME;
	out->pk_nret = 0;
	out->pk_rtwhen = out->pk_whensent + ioqp->io_rtdelay;
	ttid = isis_timeout_reschedule(ttid, ioqp->io_rtdelay, intercl_sweep, 0, 0);
	intercl_xmit(ioqp, mp);
	return (0);
}

# ifdef SCATTERSEND
static staged_xmit;
# endif SCATTERSEND

/* Put a copy of the iclpkt on the wire... */
static void
intercl_xmit(ioqp, mp)
	register ioq *ioqp;
	register message *mp;
{
	register iovlen;
	register interclient *hdr;

	t_scheck();

	if ((ioqp->io_state & IO_INUSE) == 0)
		return;

	if (mp == ackiclpkt) {
		if (my_site_incarn == RECOVERY_INCARN)
			return;
		hdr = ackinf;
		hdr->ic_from = my_address;
		hdr->ic_dest = ioqp->io_claddr;
	} else
		hdr = (interclient *) msg_getfield(mp, INTERCLIENT_HDR, 1, (int *) 0);
	hdr->ic_abits = ioqp->io_abits;
	hdr->ic_aseqn = ioqp->io_iseqn;
	ioqp->io_wantack = 0;
	iovlen = msg_getiovlen(mp);
# ifdef SCATTERSEND
	if (staged_xmit || iovlen > MAX_UDP_IOVLEN)
# endif SCATTERSEND
	{
		register char *sbp = staging_buf;
		register iovec *iovp = msg_getiovec(mp);
		register len = msg_getlen(mp);

		while (iovlen--) {
			bcopy(iovp->iov_base, sbp, iovp->iov_len);
			sbp += iovp++->iov_len;
		}
#ifdef HPUX
	      loop:
		if (sendto
		    (intercl_socket, staging_buf, len, 0, (struct sockaddr *) &ioqp->io_address,
		     sizeof(saddr)) != -1)
			errno = 0;
		else if (errno == EWOULDBLOCK) {
			intercl_socket = isis_reset_sock(intercl_socket);
			goto loop;
		} else
			perror("intercl: sendto");
#else				/* HPUX */
		if (sendto
		    (intercl_socket, staging_buf, len, 0, (struct sockaddr *) &ioqp->io_address,
		     sizeof(saddr)) != -1)
			errno = 0;
		else
			perror("intercl: sendto");
#endif				/* HPUX */
	}
# ifdef SCATTERSEND
	else {
		static struct msghdr mh;

		mh.msg_name = (caddr_t) &ioqp->io_address;
		mh.msg_namelen = sizeof(saddr);
		mh.msg_iov = msg_getiovec(mp);
		mh.msg_iovlen = iovlen;
		if (sendmsg(intercl_socket, &mh, 0) != -1)
			errno = 0;
		else
			perror("intercl: sendmsg");
	}
# endif SCATTERSEND
	if (errno == 0 || errno == ECONNREFUSED || errno == ENOBUFS) {
		if (++intercl_xmits == 2)
			intercl_drain_any_input();
		return;
	}
	perror("send");
}

/* Input all pending network iclpkts and tank them. */
static bitvec tank_mask;
static tank_mask_bits;

void
intercl_drain_any_input()
{
	while (tank_mask_bits <= intercl_socket)
		tank_mask_bits += 32;
	bis(&tank_mask, intercl_socket);
	while (tank->size <= tank->max_size
	       && select(tank_mask_bits, &tank_mask, (int *) 0, (int *) 0, &tank_wait) == 1)
		intercl_drain_one_input();
	set_tank_status();
}

int
intercl_do_input()
{
	if (tank->size <= tank->max_size) {
		intercl_drain_one_input();
		set_tank_status();
		return (0);
	}
	return (-1);
}

static void
intercl_drain_one_input()
{
	register message *mp;
	message *isis_rcvmsg();
	int fromlen;
	saddr from;

	intercl_xmits = 0;
	fromlen = sizeof(from);
	if (mp = isis_rcvmsg(intercl_socket, &from, &fromlen)) {
		intercl_accept(mp);
		msg_delete(mp);
	} else
		print("intercl_drain_input -- warning: can't reconstruct message\n");
}

static void
intercl_accept(mp)
	register message *mp;
{
	register ioq *ioqp;
	register interclient *ic;
	register iclpkt *out;
	register qnode *hp;
	register aseqn, n;

	if (ic = (interclient *) msg_getfield(mp, INTERCLIENT_HDR, 1, (int *) 0)) {
		hp = pg_find(IOQS, &ic->ic_from);
		msg_increfcount(mp);
		if (hp == 0) {
			qu_add_mp(KEEPERS, ISIS_TIME, mp, MSG_DELETE);
			if (!ttid)
				ttid = isis_timeout_reschedule(ttid, 1000, intercl_sweep, 0, 0);
			return;
		}
		qu_add_mp(tank->head, ic->ic_from.addr_site, mp, NULLROUTINE);
		if (tank->size > tank->max_size)
			panic("pr_msgtank: Tank overflow on message %d bytes long.\n",
			      msg_getlen(mp));
		tank->size += msg_getlen(mp);
		++tankcount[ic->ic_from.addr_site];
		ioqp = hp->qu_ioq;
		ioqp->io_lastin = ISIS_TIME;
		n = 1;
		aseqn = INC(ic->ic_aseqn, 1);
		if (ic->ic_abits && ioqp->io_oseqn != ioqp->io_nseqn
		    && INRANGE(ioqp->io_oseqn, DEC(ioqp->io_nseqn), aseqn))
			while (n && ic->ic_abits >= n) {
				if (ic->ic_abits & n) {
					register p = aseqn & WMASK;

					if ((ioqp->io_out[p].pk_state & (PK_INUSE | PK_SENT)) ==
					    (PK_INUSE | PK_SENT))
						ioqp->io_out[p].pk_state |= PK_ACKED;
				}
				aseqn = INC(aseqn, 1);
				n <<= 1;
			}

		/* regenerate ack info */
		if (ic->ic_seqn != SEQ_ACK)
			if (INRANGE
			    (INC(ioqp->io_iseqn, 1), INC(ioqp->io_iseqn, MAXWSIZE), ic->ic_seqn)) {
				register n = ic->ic_seqn - ioqp->io_iseqn - 1;

				if (n < 0)
					n += MAXSEQN + 1;
				ioqp->io_abits |= 1 << n;
			}

		aseqn = ic->ic_aseqn;
		/* Clear output slots if I can safely do so */
		while (ioqp->io_oseqn != ioqp->io_nseqn
		       && INRANGE(ioqp->io_oseqn, DEC(ioqp->io_nseqn), aseqn)) {
			register dt;

			out = &ioqp->io_out[ioqp->io_oseqn & WMASK];
			++ioqp->io_nslots;
			ioqp->io_nbytes -= msg_getlen(out->pk_iclpkt);
			ioqp->io_oseqn = INC(ioqp->io_oseqn, 1);
			msg_delete(out->pk_iclpkt);
			out->pk_iclpkt = 0;
			out->pk_state = 0;
			if (out->pk_nret == 0) {
				if ((dt = ISIS_TIME - out->pk_whensent + 1000) <= 2000)
					dt = 3000;
				ioqp->io_rtdelay =
				    ((ioqp->io_rtdelay << 3) - ioqp->io_rtdelay + dt) >> 3;
			}
			while (hp = qu_head(out->pk_contents)) {
				(*hp->qu_callback) (&ioqp->io_claddr, hp->qu_args[0],
						    hp->qu_args[1]);
				qu_free(hp);
			}
		}
		if (will_check == 0) {
			void check_nacks();

			++will_check;
			isis_ondrain(check_nacks, ioqp);
		}
	}
}

/* Retransmit any NACK'ed packets */
void
check_nacks()
{
	qnode *qp;
	register ioq *ioqp;
	register iclpkt *out;
	register n, unacked, acked_lo, acked_hi, tflag;

	for (qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next) {
		register ioq *ioqp = qp->qu_ioq;

		unacked = 0;
		tflag = acked_hi = acked_lo = -1;
		/* First scan to see if there are any nacked packets */
		for (n = ioqp->io_oseqn; n != ioqp->io_nseqn; n = INC(n, 1)) {
			register flag;

			out = &ioqp->io_out[n & WMASK];
			if (((flag = out->pk_state & (PK_INUSE | PK_ACKED)) & PK_INUSE) == 0)
				continue;
			if (unacked && flag == (PK_INUSE | PK_ACKED)) {
				/* 
				 * Here if saw ... non-acked .... sent|acked
				 * treat the later ack as an implicit nack on prior packet
				 */
				if (acked_lo == -1)
					acked_lo = n;
			} else if (out->pk_nret == 0 && flag == PK_INUSE) {
				/* Found a non-acked packet, but may not have been nacked */
				++unacked;
				if (acked_lo != -1) {
					/* This is if we previously saw a nack-ack sequence */
					acked_hi = acked_lo;
					acked_lo = -1;
				}
			}
		}
		if (acked_lo == -1)
			acked_lo = acked_hi;
		/* the nacked ones are in the span ioseqn...acked_lo */
		if (unacked && acked_lo != -1)
			for (n = ioqp->io_oseqn; n != acked_lo; n = INC(n, 1)) {
				out = &ioqp->io_out[n & WMASK];
				if (out->pk_nret == 0
				    && (out->pk_state & (PK_INUSE | PK_ACKED)) == PK_INUSE) {
					if (++tflag == 0)
						set_isis_time();
					++out->pk_nret;
					++ioqp->io_nret;
					out->pk_rtwhen = ISIS_TIME + ioqp->io_rtdelay;
					intercl_xmit(ioqp, out->pk_iclpkt);
				}
			}
	}
	will_check = 0;
}

/*
 * Read and process a message from the network input tank.
 * Returns TANK_.... to indicate what we did
 */
int
intercl_deliver()
{
	register ioq *ioqp;
	register interclient *ic;
	message *mp;
	static in_intercl_input;
	int got_dup = 0;
	qnode *qp;

	if ((mp = msg_tank_dequeue(tank)) == (message *) 0)
		return (TANK_EMPTY);

	if (in_intercl_input++)
		panic("intercl_input");
	if ((ic = (interclient *) msg_getfield(mp, INTERCLIENT_HDR, 1, (int *) 0)) == 0) {
		print("intercl_input -- can't extract interclient-hdr\n");
		msg_delete(mp);
		--in_intercl_input;
		return (TANK_OTHER);
	}
	if ((qp = pg_find(IOQS, &ic->ic_from)) == 0) {
		if (qp = pg_find(ZOMBIES, &ic->ic_from)) {
			msg_delete(mp);
			--in_intercl_input;
			return (TANK_OTHER);
		}
		intercl_isalive((address *) 0, &ic->ic_from, (saddr *) 0);
		qp = pg_find(IOQS, &ic->ic_from);
	}
	ioqp = qp->qu_ioq;
	ioqp->io_state |= IO_ESTAB;
	if (ic->ic_seqn != SEQ_ACK) {
		register message **in;
		register len = msg_getlen(mp);

		if (len < MAXSIZE / WSIZE)
			len = MAXSIZE / WSIZE;
		ioqp->io_wantack += len;
		/* Discard duplicate input messages */
		in = &ioqp->io_in[ic->ic_seqn & MAXWMASK];
		if (INRANGE(INC(ioqp->io_iseqn, 1), INC(ioqp->io_iseqn, MAXWSIZE), ic->ic_seqn)
		    && *in == 0) {
			++ioqp->io_ndata;
			*in = mp;
			msg_increfcount(mp);
		} else {
			++ioqp->io_ndups;
			++got_dup;
		}
	}

	/* Briefly supress output */
	force_lazy(TRUE);
	if (ic->ic_seqn != SEQ_ACK && in_intercl_input == 1) {
		register n;
		register message **in;

		while (*(in = &ioqp->io_in[INC(ioqp->io_iseqn, 1) & MAXWMASK])) {
			message *msgs[MAXMSGS];
			register m;

			/* Now process the input messages */
			ioqp->io_iseqn = INC(ioqp->io_iseqn, 1);
			ioqp->io_abits >>= 1;
			m = msg_getmsgs(*in, INTERCLIENT_MSG, msgs, MAXMSGS);
			ioqp->io_nmsgs += m;
			for (n = 0; n < m; n++) {
				register address *ap;

				ap = msg_getdests(msgs[n]);
				if ((ioqp->io_state & IO_ASSEM)
				    || (ap && ap->addr_entry == GENERIC_CL_FRAGMAN)) {
#                       ifdef INTERVERBOSE
					print("*** INTERCL: REASSEMBLE ");
					pmsg(msgs[n]);
#                       endif INTERVERBOSE
					fragman(msgs[n]);
				} else {
#                       ifdef INTERVERBOSE
					print("*** INTERCL: DELIVER ");
					pmsg(msgs[n]);
#                       endif INTERVERBOSE
					intercl_gotmsg(ioqp, msgs[n], 0);
				}
				msg_delete(msgs[n]);
			}
			if ((ioqp->io_state & (IO_INUSE | IO_DEAD)) != IO_INUSE) {
				/* Special for site shutdown while loop ran above */
				force_lazy(FALSE);
				--in_intercl_input;
				msg_delete(mp);
				return (TANK_ATEONE);
			}
			msg_delete(*in);
			*in = (message *) 0;
		}
	}
	if (ioqp->io_backed && (ioqp->io_nslots && ioqp->io_nbytes < PK_LOWAT))
		intercl_drain(ioqp);
	t_yield();
	force_lazy(FALSE);
	if (ioqp->io_wantack >= PK_HIWAT / 2 || got_dup) {
		++ioqp->io_nacks;
		intercl_xmit(ioqp, ackiclpkt);
	} else {
		if (ioqp->io_wantack)
			isis_state |= ISIS_WANTACK;
		ttid = isis_timeout_reschedule(ttid, 1000, intercl_sweep, 0, 0);
	}
	msg_delete(mp);
	--in_intercl_input;
	return (TANK_ATEONE);
}

/* Invoked when halting, true if nothing unacknowledged on any qnode */
int
intercl_quiet()
{
	register qnode *qp;

	for (qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next) {
		register ioq *ioqp = qp->qu_ioq;

		if (ioqp->io_nslots != WSIZE)
			return (0);
	}
	return (1);
}

void
lazy_flush()
{
	register qnode *qp;

	for (qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next) {
		register ioq *ioqp = qp->qu_ioq;

		if (ioqp->io_state & IO_ACCUM)
			intercl_output(ioqp, ioqp->io_accum);
	}
	will_flush = 0;
}

/* 
 *      Invoked every second, approximately.
 *      Notice failed remote after 20secs, trigger failure detection
 */
void
intercl_sweep()
{
	register qnode *qp, *nqp;
	int stuff_to_send = 0;

	ttid = 0;
	/* Rotate IOQS entries for fairness */
	if (qp = qu_head(IOQS)) {
		qu_remove(qp);
		qu_append(IOQS, qp);
	}
	for (qp = IOQS->qu_next; qp != IOQS; qp = nqp) {
		register ioq *ioqp = qp->qu_ioq;
		register iclpkt *out;
		register n;

		nqp = qp->qu_next;
		if (ioqp->io_state & IO_ALIVE) {
			/* Probe? */
			if (ioqp->io_probefreq) {
				++stuff_to_send;
				if (ioqp->io_lastin + ioqp->io_probefreq + ioqp->io_probetimeout <
				    ISIS_TIME) {
					void intercl_died();
					register message *msg;
					address addr;
					static tid;

					msg = msg_gen("%A[1]", &ioqp->io_claddr);
					addr = PRO(CL_DIED);
					isis_send(&addr, msg);
					msg_delete(msg);
					intercl_died(qp);
					continue;
				} else if (ioqp->io_lastin + ioqp->io_probefreq < ISIS_TIME) {
					/* Send an ACK packet, perhaps this will wake him up */
					++ioqp->io_nacks;
					intercl_xmit(ioqp, ackiclpkt);
				}
			}
			/* Retransmit as needed */
			for (n = ioqp->io_oseqn; n != ioqp->io_nseqn; n = INC(n, 1)) {
				out = &ioqp->io_out[n & WMASK];
				if ((out->pk_state & (PK_INUSE | PK_ACKED)) == PK_INUSE) {
					++stuff_to_send;
					if (ISIS_TIME > out->pk_rtwhen) {
						if (++out->pk_nret > 2) {
							if (ioqp == mother_iqp &&
							    ISIS_TIME - out->pk_whensent >
							    30 * 1000)
								panic
								    ("remote client: lost connection to mother machine");
							out->pk_rtwhen = ISIS_TIME + 1000;
						} else
							out->pk_rtwhen =
							    ISIS_TIME +
							    ioqp->io_rtdelay / out->pk_nret;
						intercl_xmit(ioqp, out->pk_iclpkt);
						++ioqp->io_nret;
					}
				}
			}
		}
	}
	if (isis_state & ISIS_WANTACK) {
		for (qp = IOQS->qu_next; qp != IOQS; qp = nqp) {
			register ioq *ioqp = qp->qu_ioq;

			nqp = qp->qu_next;
			if (ioqp->io_wantack) {
				++ioqp->io_nacks;
				intercl_xmit(ioqp, ackiclpkt);
			}
		}
		isis_state &= ~ISIS_WANTACK;
	}
	while (qp = qu_head(KEEPERS)) {
		if (qp->qu_time < ISIS_TIME - 1000) {
			qu_free(qp);
		} else {
			++stuff_to_send;
			break;
		}
	}
	if (stuff_to_send)
		ttid = isis_timeout(1000, intercl_sweep, 0, 0);
}

intercl_flushacks()
{
	register qnode *qp, *nqp;

	for (qp = IOQS->qu_next; qp != IOQS; qp = nqp) {
		register ioq *ioqp = qp->qu_ioq;

		nqp = qp->qu_next;
		if (ioqp->io_wantack) {
			++ioqp->io_nacks;
			intercl_xmit(ioqp, ackiclpkt);
		}
	}
	isis_state &= ~ISIS_WANTACK;
}

/* Called when a client dies, does callbacks and also calls by_isdead for all protocols */
void
intercl_died(iqp)
	register qnode *iqp;
{
	register i;
	register qnode *qp;
	register ioq *ioqp;

	if (iqp == 0)
		return;
	ioqp = iqp->qu_ioq;
	pg_add(ZOMBIES, &ioqp->io_claddr, NULLARG, NULLROUTINE);
	qu_remove(iqp);
	/* Drain output and input iclpkts */
	for (i = 0; i != WSIZE; i++) {
		register iclpkt *out = &ioqp->io_out[INC(ioqp->io_oseqn, i) & WMASK];
		register message *mp = out->pk_iclpkt;

		if (mp) {
			register qnode *hp;

			msg_delete(mp);
			while (hp = qu_head(out->pk_contents)) {
				(*hp->qu_callback) (&ioqp->io_claddr, hp->qu_args[0],
						    hp->qu_args[1]);
				qu_free(hp);
			}
		}
	}

	/* Next drain backlogged messages */
	while (qp = qu_head(ioqp->io_backlog)) {
		register mdesc *md = qp->qu_md;

		qu_free(qp);
		if (md->md_cb)
			(*md->md_cb) (&ioqp->io_claddr, md->md_arg0, md->md_arg1);
		msg_delete(md->md_msg);
		mdfree(md);
	}
	send_outstanding -= ioqp->io_backed;

	if (ioqp->io_spool) {
		while (qp = qu_head(ioqp->io_spool)) {
#               if (BYP_VERBOSE)
			register qnode *xp;

			for (xp = qp->qu_queue->qu_next; xp != qp->qu_queue; xp = xp->qu_next) {
				register by_info *byi;
				register message *mp;
				register *bseqn;

				mp = xp->qu_msg;
				byi = msg_getbyi(mp);
				bseqn = byi->by_bseq;
				print("[%d]: intercl_discard [%d %d %d %d] from %d view %d.%d\n",
				      my_process_id, bseqn[0], bseqn[1], bseqn[2], bseqn[3],
				      msg_getsender(mp)->addr_process, VMM(byi->by_viewid));
			}
#               endif

			qu_free(qp);
		}
		qu_free(ioqp->io_spool);
	}
	ioqp->io_spool = 0;
	qu_free(ioqp->io_reflist);
	ioqp->io_reflist = 0;

	if (ioqp->io_wantflush) {
		while (qp = qu_head(ioqp->io_wantflush)) {
			register message *mp;

			if ((mp = qp->qu_msg) == 0) {
				address dest;

				mp = msg_gen("%A[1],%A[1],%d", &qp->qu_pname, &ioqp->io_claddr, -1);
				dest = my_address;
				dest.addr_entry = GENERIC_BYFLSH;
				msg_setdest(mp, &dest);
				isis_gotmsg(mp, BYP_DONTCHECK, 0);
				msg_delete(mp);
			}
			qu_free(qp);
		}
#           if(HPUX)
		{
			/* HP hits a compiler problem here... */
			qnode *hpuxp = ioqp->io_wantflush;

			qu_free(hpuxp);
		}
#           else
		qu_free(ioqp->io_wantflush);
#           endif
		ioqp->io_wantflush = 0;
	}

	for (i = 0; i < MAXWSIZE; i++)
		if (ioqp->io_in[i])
			msg_delete(ioqp->io_in[i]);
	qu_free(iqp);
}

/* Called on first glimpse of a new view */
void
intercl_newview(first_time, gv)
	int first_time;
	register groupview *gv;
{
	register qnode *qp, *nqp;
	register address *ap;
	register i;

	if (!aptr_isnull(&gv->gv_departed)) {
		for (qp = IOQS->qu_next; qp != IOQS; qp = nqp) {
			register ioq *ioqp = qp->qu_ioq;
			register qnode *ap;

			nqp = qp->qu_next;
			if (!addr_isequal(&ioqp->io_claddr, &gv->gv_departed))
				continue;
			if (ap = pg_find(ioqp->io_reflist, &gv->gv_gaddr)) {
				qu_free(ap);
				if (qu_head(ioqp->io_reflist) == 0)
					intercl_died(qp);
			}
		}
		for (i = 0; i != NTRANSPORT; i++)
			if (by_physdead[i])
				(*by_physdead[i]) (&gv->gv_departed);
	} else if (gv->gv_nmemb == 0) {
		/* Specifically for case where I left the group */
		for (ap = gv->gv_members; !aptr_isnull(ap); ap++) {
			for (qp = IOQS->qu_next; qp != IOQS; qp = nqp) {
				register ioq *ioqp = qp->qu_ioq;
				register qnode *gp;

				nqp = qp->qu_next;
				if (!addr_isequal(&ioqp->io_claddr, ap))
					continue;
				if (gp = pg_find(ioqp->io_reflist, &gv->gv_gaddr)) {
					qu_free(gp);
					if (qu_head(ioqp->io_reflist) == 0)
						intercl_died(qp);
				}
			}
			for (i = 0; i != NTRANSPORT; i++)
				if (by_physdead[i])
					(*by_physdead[i]) (ap);
		}
		return;
	}
	for (ap = gv->gv_members; !aptr_isnull(ap); ap++)
		if (addr_ismine(ap))
			break;
	if (aptr_isnull(ap))
		return;
	if (first_time) {
		/* First time seen */
		for (ap = gv->gv_members; !aptr_isnull(ap); ap++)
			intercl_isalive(&gv->gv_gaddr, ap, (saddr *) 0);
	} else if (!aptr_isnull(&gv->gv_joined))
		intercl_isalive(&gv->gv_gaddr, &gv->gv_joined, (saddr *) 0);
}

void
dump_interclient()
{
	register qnode *qp;
	extern isis_gt_calls;

	print("Interclient dump: %d GT calls ", isis_gt_calls);
	if (force_piggybacking)
		print("forced_piggybacking enabled\n");
	else
		print("\n");
	if (IOQS) {
		for (qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next)
			ioq_dump(qp->qu_ioq);
		msg_tank_dump(tank);
	}
}

static void
ioq_dump(ioqp)
	register ioq *ioqp;
{
	register iclpkt *out;
	register p;
	register qnode *qp;

	if (ioqp->io_state) {
		print("  ");
		paddr(&ioqp->io_claddr);
		if (ioqp->io_claddr.addr_process > -10)
			print("\t[%s]:\n\t", site_names[ioqp->io_claddr.addr_site]);
		else
			print("\tPoll freq: %d timeout %d (last in: %d)\n\t",
			      ioqp->io_probefreq / 1000, ioqp->io_probetimeout / 1000,
			      (ISIS_TIME - ioqp->io_lastin) / 1000);
		if (ioqp->io_state) {
			if (ioqp->io_state & IO_ESTAB)
				print("estab;");
			if (ioqp->io_state & IO_ALIVE)
				print("alive;");
			if (ioqp->io_state & IO_ACCUM)
				print("accum;");
			if (ioqp->io_state & IO_URGENT)
				print("urg;");
			if (ioqp->io_state & IO_ASSEM)
				print("assem;");
			if (ioqp->io_state & IO_DEAD)
				print("is dead;");
		}
		print(" got: %d/%d+%d dups, sent: %d+%d ret, %d acks, ", ioqp->io_ndata,
		      ioqp->io_nmsgs, ioqp->io_ndups, ioqp->io_nsent, ioqp->io_nret,
		      ioqp->io_nacks);
		print(" backlog %d\n", ioqp->io_backed);
		if (ioqp->io_wantflush) {
			for (qp = ioqp->io_wantflush->qu_next; qp != ioqp->io_wantflush;
			     qp = qp->qu_next)
				if (qp->qu_msg == 0) {
					print("\tWant flush: ");
					paddr(&qp->qu_pname);
					print(", viewid %d.%d\n", VMM(qp->qu_viewid));
				} else {
					print("\tGot flush: ");
					paddr(&qp->qu_pname);
					print(", viewid %d.%d\n\t\t", VMM(qp->qu_viewid));
					pmsg(qp->qu_msg);
				}
		}
		if (ioqp->io_spool) {
			for (qp = ioqp->io_spool->qu_next; qp != ioqp->io_spool; qp = qp->qu_next) {
				register qnode *np;

				print("\tSpool for ");
				paddr(&qp->qu_pname);
				print("\n");
				if (qp->qu_queue == 0)
					print("\t\t***** BLAH *****\n");
				else
					for (np = qp->qu_queue->qu_next; np != qp->qu_queue;
					     np = np->qu_next) {
						print("\t\t");
						pmsg(np->qu_msg);
					}
			}
		}
		for (p = ioqp->io_oseqn; p != ioqp->io_nseqn; p = INC(p, 1)) {
			int ws, whs, rs, rhs;

			out = &ioqp->io_out[p & WMASK];
			ws = ISIS_TIME - out->pk_whensent;
			whs = ws / 10 % 100;
			ws /= 1000;
			rs = out->pk_rtwhen - ISIS_TIME;
			rhs = rs / 10 % 100;
			rs /= 1000;
			print("\t... iclpkt %d = sl%d,sz%d,cnt%d", p, out - ioqp->io_out,
			      out->pk_size, out->pk_cnt);
			if ((out->pk_state & PK_ACKED) == 0)
				print(", nret%d, waiting %d.%.2d ret after %d.%.2d", out->pk_nret,
				      ws, whs, rs, rhs);
			print(", state=<");
			if (ioqp->io_accum == out)
				print("accum;");
			if (out->pk_state & PK_INUSE)
				print("inuse;");
			if (out->pk_state & PK_SENT)
				print("sent;");
			if (out->pk_state & PK_ACKED)
				print("acked;");
			print(">\n");
		}
		while (p != INC(ioqp->io_oseqn, WSIZE)) {
			out = &ioqp->io_out[p & WMASK];
			if (out->pk_iclpkt)
				print("\t*** FOUND A PACKET IN SEQN %d... oseqn %d nseqn %d!\n", p,
				      ioqp->io_oseqn, ioqp->io_nseqn);
			if (out->pk_state)
				print("\t*** FOUND STATE %x in SEQN %d!... oseqn %d nseqn %d\n",
				      out->pk_state, p, ioqp->io_oseqn, ioqp->io_nseqn);
			p = INC(p, 1);
		}
		for (p = 0; p < MAXWSIZE; p++)
			if (ioqp->io_in[p])
				break;
		if (p == MAXWSIZE)
			return;
		print("\t... full input slots: seqn ");
		for (p = 1; p <= MAXWSIZE; p++)
			if (ioqp->io_in[INC(ioqp->io_iseqn, p) & MAXWMASK])
				print("%d ", INC(ioqp->io_iseqn, p));
		print("\n");
	}
}

/* Dump contents of an inter-site iclpkt */
static void
ic_dump(why, mp)
	char *why;
	register message *mp;
{
	register interclient *ic;
	register n, m;
	message *msgs[MAXMSGS];

	ic = (interclient *) msg_getfield(mp, INTERCLIENT_HDR, 1, (int *) 0);
	m = msg_getmsgs(mp, INTERCLIENT_MSG, msgs, MAXMSGS);
	print("IS_DUMP: %s msg %x<%d> len %d", why, mp, m, msg_getlen(mp));
	if (ic == 0)
		print("[no header]\n");
	else {
		if (ic->ic_seqn == SEQ_ACK) {
			print("[from %x ACK, aseqn %d abits %x]\n", ic->ic_aseqn, ic->ic_abits);
			goto phd;
		}
		print("[from %x seqn %d aseqn %d abits %x]\n", ic->ic_from, ic->ic_seqn,
		      ic->ic_aseqn, ic->ic_abits);
	}
	for (n = 0; n < m; n++) {
		print("  <%d>: mp %x = ", n, msgs[n]);
		pmsg(msgs[n]);
		msg_delete(msgs[n]);
	}
      phd:msg_printheader(mp);
	msg_printaccess(mp);
}

static msg_tank *
msg_tank_create(max_size)
	register max_size;
{
	static adesc ad = { sizeof(msg_tank), sizeof(msg_tank), 1 };
	register msg_tank *tank = (msg_tank *) mallocate(&ad);

	tank->head = qu_null();
	tank->max_size = (max_size == 0 ? default_max_size : max_size);
	return (tank);
}

static void
msg_tank_set_max(tank, max_size)
	register msg_tank *tank;
	register long max_size;
{
	tank->max_size = (max_size == 0 ? default_max_size : max_size);
}

/* Returns null (zero) message pointer if tank is empty. */
static message *
msg_tank_dequeue(tank)
	register msg_tank *tank;
{
	register qnode *q;
	register message *msg;

	if (q = qu_head(tank->head)) {
		msg = q->qu_msg;
		--tankcount[q->qu_name];
		qu_free(q);
		tank->size -= msg_getlen(msg);
		set_tank_status();
		return (msg);
	} else
		return ((message *) 0);
}

static void
msg_tank_dump(tank)
	register msg_tank *tank;
{
	qnode *qp;
	int n = 0;

	if (qu_head(intercl_waitq)) {
		print("  Interclient wait-queue: ");
		for (qp = intercl_waitq->qu_next; qp != intercl_waitq; qp = qp->qu_next) {
			print("    ");
			pmsg(qp->qu_msg);

		}
	}
	for (qp = tank->head->qu_next; qp != tank->head; qp = qp->qu_next)
		n++;
	print("  Message tank: %d messages, %d bytes\n", n, tank->size);
}

int
intercl_wantflush(addr, gaddr, viewid)
	register address *addr, *gaddr;
	int viewid;
{
	register qnode *iqp = pg_find(IOQS, addr), *qp;
	register ioq *ioqp;

	if (iqp == 0) {
		if (pg_find(ZOMBIES, addr))
			return (-1);
		panic("intercl_wantflush: addr unknown!");
	}
	ioqp = iqp->qu_ioq;
	if (ioqp->io_wantflush == 0)
		ioqp->io_wantflush = qu_null();
	for (qp = ioqp->io_wantflush->qu_next; qp != ioqp->io_wantflush; qp = qp->qu_next)
		if (addr_isequal(&qp->qu_pname, gaddr))
			if (qp->qu_viewid == viewid)
				break;
			else
				print
				    ("WARNING: pid %d wants flush %d.%d but prev. got flush %d.%d\n",
				     my_process_id, VMM(viewid), VMM(qp->qu_viewid));
	if (qp == ioqp->io_wantflush) {
		qp = pg_add_mp(ioqp->io_wantflush, gaddr, (message *) 0, NULLROUTINE);
		qp->qu_viewid = viewid;
	} else if (qp->qu_msg)
		isis_gotmsg(qp->qu_msg, BYP_CHECK, 0);
	return (0);
}

void
isis_receipt(mp, addr, pn)
	message *mp;
	address *addr;
	int pn;
{
	register qnode *iqp = pg_find(IOQS, addr);
	register ioq *ioqp;

	if (iqp == 0) {
		print("isis_receipt: proc address wasn't alive!\n");
		return;
	}
	ioqp = iqp->qu_ioq;
	intercl_gotmsg(ioqp, mp, pn);
}

void
intercl_gotmsg(ioqp, mp, pn)
	register message *mp;
	register ioq *ioqp;
	int pn;
{
	register address *dp;
	register qnode *qp;
	register by_info *byi;

#	if (BYP_VERBOSE)
	print("intercl_gotmsg: transmited by ");
	paddr(&ioqp->io_claddr);
	pmsg(mp);
#endif				/* BYP_VERBOSE */
	if (mother_iqp && ioqp == mother_iqp) {
		msg_increfcount(mp);
		qu_add_mp(mother_msg_queue, 0, mp, MSG_DELETE);
		return;
	}
	dp = msg_getdests(mp);
	if (my_process_id == ISIS && dp->addr_process == PROTOCOLS) {
		if (dp->addr_entry == CL_PROBE) {
			int freq = 60, timeout = 30;

			msg_get(mp, "%d,%d", &freq, &timeout);
			ioqp->io_probefreq = freq * 1000;
			ioqp->io_probetimeout = timeout * 1000;
			ioqp->io_lastin = ISIS_TIME;
		}
		msg_write(isis_socket, mp);
		return;
	}
	byi = msg_getbyi(mp);
	if (byi && (qp = pg_find(ioqp->io_spool, &byi->by_group))) {
#           if(INTERVERBOSE)
		print("... spool\n");
#           endif
		msg_increfcount(mp);
		qu_add_mp(qp->qu_queue, 0, mp, (vfunc *) MSG_DELETE);
		return;
	}
	if (dp->addr_entry == GENERIC_BYFLSH) {
		address gaddr, hisaddr;
		int vid;

		msg_rewind(mp);
		msg_get(mp, "%a,%a,%d", &gaddr, &hisaddr, &vid);
		msg_rewind(mp);
		/* Got flush early */
		if (ioqp->io_spool == 0)
			ioqp->io_spool = qu_null();
		if ((qp = pg_find(ioqp->io_spool, &gaddr)) == 0)
			pg_add_qu(ioqp->io_spool, &gaddr, qu_null());
		else {
			/* Spooling for some previous FLUSH activity */
			msg_increfcount(mp);
			qu_add_mp(qp->qu_queue, 0, mp, (vfunc *) MSG_DELETE);
			return;
		}
		if (ioqp->io_wantflush == 0)
			ioqp->io_wantflush = qu_null();
		for (qp = ioqp->io_wantflush->qu_next; qp != ioqp->io_wantflush; qp = qp->qu_next)
			if (!addr_isequal(&qp->qu_pname, &gaddr))
				continue;
			else if (qp->qu_viewid == vid) {
				msg_increfcount(mp);
				qp->qu_msg = mp;
				qp->qu_freeroutine = (vfunc *) MSG_DELETE;
				isis_gotmsg(mp, BYP_CHECK, 0);
				return;
			}
		msg_increfcount(mp);
		qp = pg_add_mp(ioqp->io_wantflush, &gaddr, mp, (vfunc *) MSG_DELETE);
		qp->qu_viewid = vid;
		return;
	}

	if (intercl_unblocking) {
		/* Delay delivery until systems quiets down */
		void intercl_dequeue();

#           if(INTERVERBOSE)
		print("... unblocking, onto intercl_waitq\n");
#           endif
		msg_increfcount(mp);
		qu_add_mp(intercl_waitq, pn, mp, MSG_DELETE);
		return;
	}
	if (byi)
		isis_gotmsg(mp, BYP_CHECK, pn);
	else
		isis_gotmsg(mp, BYP_DONTCHECK, pn);
}

void
intercl_dequeue()
{
	register qnode *qp;

	while (qp = qu_head(intercl_waitq)) {
		register message *mp = qp->qu_msg;

		qu_remove(qp);
#           if(INTERVERBOSE)
		print("... deliver from intercl_waitq ");
		pmsg(mp);
#           endif
		if (msg_getbyi(mp))
			isis_gotmsg(mp, BYP_CHECK, qp->qu_name);
		else
			isis_gotmsg(mp, BYP_DONTCHECK, qp->qu_name);
		qu_free(qp);
	}
	intercl_unblocking = 0;
}

void
intercl_unblock(bv, gaddr)
	register groupview *bv;
	address *gaddr;
{
	register address *addrs;
	register vid = bv->gv_viewid;

	addrs = bv->gv_members;
	++intercl_unblocking;
	while (!aptr_isnull(addrs)) {
		register qnode *iqp = pg_find(IOQS, addrs++), *qp, *nqp;
		register ioq *ioqp;

		if (iqp == 0)
			continue;
		ioqp = iqp->qu_ioq;
		if (ioqp->io_wantflush) {
			for (qp = ioqp->io_wantflush->qu_next; qp != ioqp->io_wantflush; qp = nqp) {
				nqp = qp->qu_next;
				if (qp->qu_viewid == vid && addr_isequal(&qp->qu_pname, gaddr))
					qu_free(qp);
			}
			if (qu_head(ioqp->io_wantflush) == 0) {
				qp = ioqp->io_wantflush;
				ioqp->io_wantflush = 0;
				qu_free(qp);
			}
		} else
			print("WARNING (pid %d) intercl_unblock: io_wantflush was null\n",
			      my_process_id);
		if (ioqp->io_spool) {
			if (qp = pg_find(ioqp->io_spool, gaddr)) {
				register qnode *np;

				qu_remove(qp);
				for (np = qp->qu_queue->qu_next; np != qp->qu_queue;
				     np = np->qu_next) {
#                       if(INTERVERBOSE)
					print("... despool ");
					pmsg(np->qu_msg);
#                       endif
					intercl_gotmsg(ioqp, np->qu_msg, 0);
				}
				qu_free(qp);
			}
			if (qu_head(ioqp->io_spool) == 0) {
				qp = ioqp->io_spool;
				ioqp->io_spool = 0;
				qu_free(qp);
			}
		}
	}
	isis_ondrain(intercl_dequeue, 0);
}

message *
isis_rcvmsg(sock, from, fromlen)
	saddr *from;
	int *fromlen;
{
	register message *mp;
	register nb;
	block_desc *blk_desc, *msg_malloc();
	extern block_desc *isis_msgblk;

	if (isis_msgblk == (block_desc *) 0)
		isis_msgblk = msg_malloc(MAXMSGLEN);
	*fromlen = sizeof(*from);
	while ((nb =
		recvfrom(sock, msg_body(isis_msgblk), MAXMSGLEN, 0, (struct sockaddr *) from,
			 fromlen)) <= 0) {
		if (nb == -1 && errno == EINTR)
			continue;
		return (0);
	}
	if (nb <= MSG_BLKSIZE) {
		if (nb < 0 || (nb & 3) || ((blk_desc = msg_malloc(nb)) == (block_desc *) 0))
			return (0);
		bcopy(msg_body(isis_msgblk), msg_body(blk_desc), nb);
	} else if (nb <= MAXMSGLEN) {
		blk_desc = isis_msgblk;
		isis_msgblk = msg_malloc(MAXMSGLEN);
	} else
		return (0);
	blk_desc->blk_avail -= nb;
	if (mp = msg_reconstruct(blk_desc))
		return (mp);
	blk_free(blk_desc);
	return ((message *) 0);
}

message *
udp_rpc(afdes, machine, raddr, site, portno, mp)
	register message *mp;
	register saddr *raddr;
	char *machine;
{
	register fdes, try;
	message *isis_rcvmsg();
	register message *rmp;
	int len;
	static rpc_fdes;

	if ((fdes = afdes) == 0 && (fdes = rpc_fdes) == 0) {
		static saddr sin;

		fdes = socket(AF_INET, SOCK_DGRAM, 0);
		sin.sin_family = AF_INET;
		if (bind(fdes, (struct sockaddr *) &sin, sizeof(sin)) == -1)
			panic("Can't allocate UDP port!");
		rpc_fdes = fdes;
		set_isis_sockopts(fdes);
	}
	if (raddr == 0) {
		address addr;

		addr = ADDRESS(site, 0, ISIS, 0);
		raddr = get_addr_by_address(&addr, portno);
	}
	len = sizeof(*raddr);
	for (try = 0; try < 5; try++) {
		struct timeval wait;
		int rv, inflag;

		isis_sendmsg(fdes, raddr, len, mp);
		inflag = 1 << fdes;
		wait.tv_usec = 0;
		wait.tv_sec = 5 + try;
		while ((rv = select(32, &inflag, (int *) 0, (int *) 0, &wait)) == -1
		       && errno == EINTR)
			continue;
		if (rv == 1)
			break;
		if (try)
			print("No reply from ISIS at <%s>: retrying...\n", machine);
		sleep(5 + try);
	}
	if (try == 5)
		return ((message *) 0);
	rmp = isis_rcvmsg(fdes, raddr, &len);
	return (rmp);
}

/* Algorithm for writing a message to a socket */
void
isis_sendmsg(sock, dst, dlen, mp)
	saddr *dst;
	int dlen;
	register message *mp;
{
	register rv, nb, iovlen;
	register struct iovec *iovp;
	register char *mbuf, *mptr;

	mptr = mbuf = malloc(nb = msg_getlen(mp));
	iovlen = msg_getiovlen(mp);
	iovp = msg_getiovec(mp);
	while (iovlen) {
		bcopy(iovp->iov_base, mptr, iovp->iov_len);
		mptr += iovp->iov_len;
		--iovlen;
		++iovp;
	}

      retrysend:
	if ((rv = sendto(sock, mbuf, nb, 0, (struct sockaddr *) dst, dlen)) != nb) {
#ifdef HPUX
		if (rv == -1 && errno == EWOULDBLOCK) {
			sock = isis_reset_sock(sock);
			goto retrysend;
		} else
#endif				/* HPUX */
			panic("isis_sendmsg: sendto system call failed!");
	}
	free(mbuf);
}

force_lazy(on_off)
{
	if (on_off) {
		++force_piggybacking;
		return;
	}
	force_piggybacking = 0;
	if (intercl_urgent) {
		register qnode *qp;

		for (qp = IOQS->qu_next; qp != IOQS; qp = qp->qu_next) {
			register ioq *ioqp = qp->qu_ioq;

			if (ioqp->io_state & IO_URGENT)
				intercl_output(ioqp, ioqp->io_accum);
		}
	}
}

#ifdef HPUX

static saddr sin;

/* Want to close/reopen same socket and port no */
/* As workaround to HPUX sendto bug             */
int
isis_reset_sock(sock)
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

	if (sock != save_sock) {
		/* must be same socket number for inter_client, so dup2 */
		if ((new_sock = dup2(sock, save_sock)) == -1)
			panic("dup2 failed in isis_reset_sock");

		close(sock);
		sock = new_sock;
	}

	sin.sin_family = AF_INET;
	sin.sin_port = name.sin_port;

	if (bind(sock, (struct sockaddr_in *) &sin, sizeof(sin)) == -1) {
		printf("Can't allocate UDP port %d! \r\n", ntohs(sin.sin_port));
		exit(0);
	}
	set_isis_sockopts(sock);

	return (sock);
}
#endif				/* HPUX */
