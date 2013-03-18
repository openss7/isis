/*  $RCSfile: cl_bcast.c,v $ $Revision: 2.122 $ $Date: 90/09/19 14:28:40 $  */
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
 */
#include "isis.h"

#define  isdigit(x)	(x >= '0' && x <= '9')

message *protos_rpc(), *bypass_send();
int     ISIS_MSGID;
static	use_transport;

struct bc_args
{
        short           bc_pname;
        short           bc_options;
        short           bc_rval;
        short           bc_nsent;
        short           bc_nreplies;
        short           bc_done;
        short           bc_errno;
	short		bc_transport;
        address         bc_dest;
        va_list         *bc_ap; 
        char            *bc_acopy;
        condition       bc_await;
        event_id        bc_eid;
	bc_args         *bc_next;
	int		bc_timeout;
};

#define NFREE   10
static adesc bc_ad ={ sizeof(bc_args), 0, 2};
static bc_args *bc_freelist;
static bc_nfree;

#define bc_alloc(bc)                                                 \
  if(bc_nfree)                                                       \
  {                                                                  \
      bc_nfree--;                                                    \
      bc = bc_freelist;                                              \
      bc_freelist = bc->bc_next;                                     \
  }                                                                  \
  else                                                               \
      bc = (bc_args*)mallocate(&bc_ad)

#define bc_free(bc)                                                  \
  {                                                                  \
        if(bc->bc_acopy)                                             \
            free(bc->bc_acopy);                                      \
        if(bc_nfree < NFREE)                                         \
        {                                                            \
            ++bc_nfree;                                              \
            bc->bc_next = bc_freelist;                               \
            bc_freelist = bc;                                        \
        }                                                            \
        else                                                         \
            mdeallocate((char*)bc, &bc_ad);                          \
  }

static  qnode   *bc_queue;
static  BCID;

#define BASIC_BCAST(rname, pro)                                         \
/* VARARGS */                                                           \
int                                                                     \
rname(va_alist)                                                         \
  va_dcl                                                                \
  {                                                                     \
        int rval;                                                       \
        va_list ap;                                                     \
        va_start(ap);                                                   \
        rval = do_bcast(pro, "", &ap);                                  \
        va_end(ap);                                                     \
        return(rval);                                                   \
  }

#define BCAST_L(rname, pro)                                             \
/* VARARGS */                                                           \
int                                                                     \
rname(va_alist)                                                         \
  va_dcl                                                                \
  {                                                                     \
        register rval;                                                  \
        va_list ap;                                                     \
        va_start(ap);                                                   \
        rval = do_bcast(pro, VA_REF(ap, char*), &ap);                   \
        va_end(ap);                                                     \
        return(rval);                                                   \
  }

BASIC_BCAST(bcast,CL_ABCAST)
BASIC_BCAST(abcast,CL_ABCAST)
BASIC_BCAST(cbcast,CL_CBCAST)
BASIC_BCAST(fbcast,CL_FBCAST)
BASIC_BCAST(mbcast,CL_MBCAST)
BASIC_BCAST(gbcast,CL_GBCAST)
BCAST_L(bcast_l,CL_ABCAST)
BCAST_L(abcast_l,CL_ABCAST)
BCAST_L(cbcast_l,CL_CBCAST)
BCAST_L(fbcast_l,CL_FBCAST)
BCAST_L(mbcast_l,CL_MBCAST)
BCAST_L(gbcast_l,CL_GBCAST)
BCAST_L(gbcast_grow,CL_GBCAST_GROW)

int
do_bcast(pro, opstr, ap)
  int pro;
  register char *opstr;
  va_list *ap;
  {
        int rval;
        register bc_args *bc;
        ISIS_ENTER();
        bc_alloc(bc);
        bc->bc_options = 0;
        bc->bc_acopy = 0;
        bc->bc_done = 0;
	bc->bc_transport = use_transport;
	bc->bc_await = 0;
	bc->bc_timeout = 0;
        /* Tricky code to make a copy of args for use in a forked off call... */
        while(*opstr)
          switch(*opstr++)
          {
		register n, op;
            case '!':
		bc->bc_options |= ISISBC_NONVSYNC;
                continue;
            case 'l':
                /* Long form */
                bc->bc_options |= ISISBC_LIST;
                continue;
            case 'm':
                /* Both are messages */
                bc->bc_options |= ISISBC_MSEND|ISISBC_MRCV;
                continue;
            case 's':
                /* Sending a message, but reply can be unpacked */
                bc->bc_options |= ISISBC_MSEND;
                continue;
            case 'r':
                /* Sending normally, but want reply messages */
                bc->bc_options |= ISISBC_MRCV;
                continue;
            case 'x':
		if(bc->bc_pname == CL_GBCAST || bc->bc_pname == CL_GBCAST_GROW)
		{
		    /* GBCAST 'x' not supported */
                    isis_errno = IE_NOTALLOWED;
                    rval = -1;
                    goto done;
	        }
                bc->bc_options |= ISISBC_EXCLUDE;
                continue;
            case 'f':
                /* Fork a task handled by BCAST_L */
                bc->bc_options |= ISISBC_TASK;
                continue;
            case 'z':
                /* Lazy */
                bc->bc_options |= ISISBC_LAZY;
                continue;
            case 'R':
                /* reply, ignore congestion */
                bc->bc_options |= ISISBC_REPLY;
                continue;
	    case 'B':
		bc->bc_options |= ISISBC_BYPASS;
                continue;
	    case 'g':
		bc->bc_options |= ISISBC_GROW;
                continue;
	    case 'P':
            case 'T':
		op = opstr[-1];
                n = 0;
                if(isdigit(*opstr)) while(isdigit(*opstr))
		    n = n*10 +  *opstr++-'0';
                else
                {
                    n = transport_lookup(opstr);
                    while(*opstr)
                        ++opstr;
                }
                if(op == 'P')
                {
                    bc->bc_options |= ISISBC_BYPASS;
                    bc->bc_transport = n;
                }
                else
                {
                    bc->bc_options |= ISISBC_TIMEOUT;
                    bc->bc_timeout = n;
                }
		continue;
            default:
                isis_errno = IE_BADARG;
                rval = -1;
                goto done;
          }
        bc->bc_pname = pro;
        if((bc->bc_options&(ISISBC_TASK|ISISBC_TIMEOUT)) == 0)
        {
            bc->bc_ap = ap;
            BCAST(bc);  
            rval = bc->bc_rval;
     done:  bc_free(bc);
            ISIS_RETURN(rval);
        }
        else
        {
            register off;
            off = (int) (isis_ctp->task_end-((char*)&ap));
            bc->bc_acopy = (char*)malloc(off);
            bcopy((char*)&ap, bc->bc_acopy, off);
            off = bc->bc_acopy-(char*)&ap;
            /* Relocate the two things that point into the current stack */
            ap = (va_list*)(((char*)ap) + off);
            *ap = (va_list)((*(char**)ap)+off);
            bc->bc_ap = ap;
            if(bc_queue == NULLQP)
                bc_queue = qu_null();
            rval = ++BCID;
            qu_add_bc(bc_queue, BCID, bc, NULLROUTINE);
            t_fork_urgent((vfunc *) BCAST, bc);
	    if(bc->bc_options&ISISBC_TIMEOUT)
	    {
		register tid;
		bc->bc_rval = 0;
		tid = isis_timeout(bc->bc_timeout*1000, (vfunc*) t_sig, (VOID*)&bc->bc_await, NULLARG);
		rval = bc_wait(rval);
		isis_timeout_cancel(tid);
	    }
        }
        ISIS_RETURN(rval);
  }

int
bc_wait(bcid)
  register bcid;
  {
        register rval;
        register bc_args *bc;
        register qnode *qp;
	ISIS_ENTER();
        if(bc_queue == 0 || (qp = qu_find(bc_queue, bcid)) == NULLQP)
        {
            isis_errno = IE_BADARG;
            ISIS_RETURN(-1);
        }
        bc = qp->qu_bc;
        qu_free(qp);
        if(bc->bc_done == 0)
            t_wait_l(&bc->bc_await, "isis system: waiting for bcast completion");
        rval = bc->bc_rval;
        isis_nsent = bc->bc_nsent;
        isis_nreplies = bc->bc_nreplies;
        isis_errno = bc->bc_errno;
        if(bc->bc_await)
            t_sig(&bc->bc_await, 0);
        else
            bc_free(bc);
        ISIS_RETURN(rval);
  }

int
bc_poll(bcid)
  register bcid;
  {
        register bc_args *bc;
        register qnode *qp;
        ISIS_ENTER();
        if(bc_queue == 0 || (qp = qu_find(bc_queue, bcid)) == NULLQP)
        {
            isis_errno = IE_BADARG;
            ISIS_RETURN(-1);
        }
        bc = qp->qu_bc;
        ISIS_RETURN(bc->bc_done);
  }

event_id *
bc_getevent(bcid)
  register bcid;
  {
        static event_id eid;
        register qnode *qp;
	ISIS_ENTER();
        if(bc_queue == 0 || (qp = qu_find(bc_queue, bcid)) == NULLQP)
            return(&eid);
        ISIS_RETURN(&qp->qu_bc->bc_eid);
  }

address *
eid_sender(eid)
  event_id *eid;
  {
        return(&eid->e_pname);
  }

int
bc_cancel(bcid)
  register bcid;
  {
        register qnode *qp;
        int outcome;
        ISIS_ENTER();
        if(bc_queue == 0 || (qp = qu_find(bc_queue, bcid)) == NULLQP)
            ISIS_RETURN(0);
        BEGINFROMC
            gbcast(&qp->qu_bc->bc_dest, GENERIC_G_CANCEL, "%E[1]", bc_getevent(bcid), 1, "%d", &outcome);
        ENDFROMC
        ISIS_RETURN(outcome);
  }

void
BCAST(bc)
  register bc_args *bc;
  {
        register message *msg;
        static message *Rmsgs[MAX_PROCS], **rmsgs;
	address addr[2];
        address dest, *alist;
        int nwant, allocated = 0, nresp, alen;
        char *nsent;
        message *rmsg;
	extern isis_bypass_enabled;

	if(bc->bc_options)
	{
            if(bc->bc_options&ISISBC_LIST)
                alist = VA_ARG(*bc->bc_ap, address *);
            else
            {
                register address *ap;
                if((ap = VA_ARG(*bc->bc_ap, address*)) == 0)
                {
                    isis_errno = IE_BADARG;
	            bc->bc_rval = -1;
		    goto done;
	        }
                addr[0] = *ap;
                addr[0].addr_entry = VA_ARG(*bc->bc_ap, int);
		addr[1] = NULLADDRESS;
                alist = addr;
            }
            if(bc->bc_options&ISISBC_MSEND)
	    {
	        msg = VA_ARG(*bc->bc_ap, message *);
		msg_deletefield(msg, SYSFLD_VERIFY, 1);
                if((bc->bc_options&ISISBC_GROW) == 0)
		    msg_deletefield(msg, SYSFLD_VCHANGE, 1);
		msg_deletefield(msg, SYSFLD_DOFLUSH, 1);
		if((bc->bc_options&ISISBC_EXCLUDE) == 0)
		    msg_deletefield(msg, SYSFLD_EXCLUDE, 1);
	    }
            else
            {
                msg = msg_newmsg();
                allocated = 1;
                if(msg_doputf(msg, SYSFLD_SCAN, bc->bc_ap) == -1)
                {
                    msg_delete(msg);
                    bc->bc_rval = -1;
                    goto done;
                }
            }
            if(bc->bc_options&ISISBC_EXCLUDE)
                msg_replacefield(msg, SYSFLD_EXCLUDE, (char*)&my_address, FTYPE_ADDRESS, sizeof(address));
            if(bc->bc_options&ISISBC_LAZY)
                msg_makelazy(msg, LAZY_ALWAYS);
	    begin
	    {
	        register address *ap;
	        register flag = 0;
	        ap = alist;
	        alen = 1;
	        while(!addr_isnull(ap))
                {
		    if(flag == 0 && addr_isgid(ap))
		        ++flag;
	            ++alen, ++ap;
	        }
	        if(flag)
	        {
                    if(alen == 2)
                        msg->msg_hdr->hdr_dest = alist[0];
		    else
	                msg_replacefield(msg, SYSFLD_ALIST, (char*)alist, FTYPE_ADDRESS, sizeof(address)*alen);
	        }
	    }
	}
	else
	{
	    /* Optimized for normal case */
            register address *ap;
            if((ap = VA_ARG(*bc->bc_ap, address*)) == 0)
            {
                isis_errno = IE_BADARG;
	        bc->bc_rval = -1;
	        goto done;
	    }
            addr[0] = *ap;
            addr[0].addr_entry = VA_ARG(*bc->bc_ap, int);
	    addr[1] = NULLADDRESS;
            alist = addr;
	    alen = 2;

            allocated = 1;
            msg = msg_newmsg();
	    if(addr_isgid(addr))
	        msg->msg_hdr->hdr_dest = addr[0];
            if(msg_doputf(msg, SYSFLD_SCAN, bc->bc_ap) == -1)
            {
                msg_delete(msg);
                bc->bc_rval = -1;
                goto done;
            }
	}
        bc->bc_eid.e_pname = my_address;
	nwant = VA_ARG(*bc->bc_ap, int);
	BEGINFROMC;
	if((bc->bc_options&ISISBC_REPLY) == 0)
	{
	    if(nwant == 0)
		isis_input_drain();
	    if(isis_state&ISIS_CONGESTED)
		isis_decon_wait(nwant);
	}
	else if(isis_bypass_enabled && (isis_state&ISIS_CONGESTED))
	    isis_decon_wait(0);
        if(nwant < 0 || nwant > MAJORITY)
            panic("bcast: bad arguments!  (Apparent value of nwant was %d)", nwant);
        if(isis_ctp->task_act)
        {
            address act;
            act = map_act(isis_ctp->task_act);
            msg_setact(msg, &act);
        }
	msg_increfcount(msg);
	if((bc->bc_pname != CL_GBCAST  && bc->bc_pname != CL_GBCAST_GROW) &&
            (rmsg = bypass_send(bc->bc_pname, bc->bc_options&ISISBC_EXCLUDE, alist, msg,
                nwant, bc->bc_options, bc->bc_transport)))
        {
            if(allocated)
                msg_delete(msg);
            if(nwant == 0 || rmsg == (message*)-1)
            {
		if(nwant == 0)
		{
                    isis_errno = isis_nsent = isis_nreplies = 0;
		    bc->bc_rval = isis_nreplies;
		}
		else
		    bc->bc_rval = -1;
                goto done;
            }
	    bc->bc_rval = 0;
        }
        else
        {
            register message *mp = msg_newmsg();
	    register address *ap;
	    if(bc->bc_options&ISISBC_BYPASS)
		print("WARNING: bcast option B was specified but can't use BYPASS protocol!\n");
            msg_setsender(mp, &isis_ctp->task_addr);
            switch(bc->bc_pname)
	    {
              case CL_GBCAST:
		msg_insertfield(msg, SYSFLD_DOFLUSH, 0, 0, 0);
		break;

              case CL_MBCAST:
		bc->bc_pname = CL_FBCAST;
              case CL_FBCAST:
              case CL_CBCAST:
	        for(ap = alist; !addr_isnull(ap); ap++)
		    if(addr_isgid(ap) || (addr_ispid(ap) && ap->addr_site != my_site_no))
		    {
		        pbuf_dirty();
		        break;
		    }
	    }
            msg_put(mp, "%A,%m,%d,%B[1]", alist, alen, msg, nwant, &my_bcastscope);
            if(allocated)
                msg_delete(msg);
            dest = PRO(bc->bc_pname);
            if(nwant == 0)
            {
                isis_send(&dest, mp);
                bc->bc_rval = isis_nsent = isis_nreplies = 0;
                msg_delete(mp);
                goto done;
            }
            rmsg = protos_rpc(&dest, mp, bc->bc_options&ISISBC_TASK);
            msg_delete(mp);
        }
	bc->bc_eid.e_msgid = 0;
        if(nsent = msg_getfield(rmsg, FLD_NSENT, 1, NULLIARG))
            isis_nsent = *nsent;
        else
            isis_nsent = 0;
	ENDFROMC;
        if(bc->bc_options&ISISBC_MRCV)
	    rmsgs = VA_REF(*bc->bc_ap, message**);
	else
	    rmsgs = Rmsgs;
	BEGINFROMC;
        isis_nreplies = nresp = msg_getmsgs(rmsg, FLD_ANSW, rmsgs, MAX_PROCS);
        if(nresp == 0)
        {
            register *cl_errno = (int*)msg_getfield(rmsg, SYSFLD_ERRNO, 1, NULLIARG);
            msg_delete(rmsg);
            if(cl_errno)
            {
                isis_errno = *cl_errno;
                bc->bc_rval = -1;
                goto done;
            }
	    bc->bc_rval = 0;
            goto done;
        }
        ENDFROMC;
        if((bc->bc_options&ISISBC_MRCV) == 0)
        {
            if(msg_dogetf(rmsgs, nresp, SYSFLD_SCAN, NULLIARG, bc->bc_ap) == -1)
            {
                extern char *isis_format;
                print("format <%s>: ", isis_format);
                isis_perror("msg_getf failed when unpacking replies");
            }
            while(nresp--)
                msg_delete(rmsgs[nresp]);
        }
        msg_delete(rmsg);
        bc->bc_rval = isis_nreplies;
  done:
	msg_delete(msg);
        bc->bc_nsent = isis_nsent;
        bc->bc_nreplies = isis_nreplies;
        bc->bc_errno = isis_errno;
        ++bc->bc_done;
        if(bc->bc_await)
            t_sig(&bc->bc_await, 0);
  }

void
flush()
  {
        register message *mp, *rmsg;
        address dest;
	by_flush();
        ISIS_ENTER();
        mp = msg_newmsg();
        dest = PRO(CL_WANTFLUSH);
        rmsg = protos_rpc(&dest, mp, 0);
        msg_delete(mp);
        msg_delete(rmsg);
	ISIS_EXIT();
  }

/* Send reply message to the sender of a message */
int
nullreply(msg)
  message *msg;
  {
        register message *rmsg;
        register msgid;
	ISIS_ENTER();
        msgid = msg_getid(msg);
        if(msgid == 0 || (msgid&1) == 0)
            ISIS_RETURN(0);
        rmsg = msg_newmsg();
        msg_addfield(rmsg, FLD_ISNULLREP, 0, 0, 0);
	msg_setid(rmsg, msgid);
        fbcast_l("mR", msg_getreplyto(msg), GENERIC_RCV_REPLY, rmsg, 0);
        msg_delete(rmsg);
	ISIS_RETURN(0);
  }

/* Send abort reply message to the sender of a message */
void
abortreply(msg)
  message *msg;
  {
	abortreply_l(msg, IE_ABORT);
  }

int
abortreply_l(msg, errno)
  message *msg;
  int errno;
  {
        register message *rmsg;
        int msgid;
	ISIS_ENTER();
        msgid = msg_getid(msg);
        if(msgid == 0)
            ISIS_RETURN(0);
        rmsg = msg_newmsg();
        msg_addfield(rmsg, FLD_ISABORTREP, (char *) &errno, FTYPE_LONG, sizeof(int));
        msg_setid(rmsg, msgid);
        cbcast_l("mR", msg_getreplyto(msg), GENERIC_RCV_REPLY, rmsg, 0);
        msg_delete(rmsg);
	ISIS_RETURN(0);
  }

void
reply(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
        do_reply(&ap);
        va_end(ap);
  }

void
do_reply(ap)
  va_list *ap;
  {
        register message *rmsg, *msg;
        int msgid;

	ISIS_ENTER();
	msg = VA_ARG(*ap, message *);
        if(msgid = msg_getid(msg))
        {
            rmsg = msg_newmsg();
	    use_transport = msg->msg_transport;
            if(msg_doputf(rmsg, SYSFLD_SCAN, ap) == -1) panic("REPLY: CAN'T GENERATE!");
            msg_setid(rmsg, msgid);
            if(isis_ctp->task_cohorts == 0)
            {
                BEGINFROMC
                    cbcast_l("mR", msg_getreplyto(msg), GENERIC_RCV_REPLY, rmsg, 0);
                ENDFROMC
            }
            else
            {
                msg_insertfield(rmsg, FLD_TRUESENDER, (char*)&isis_ctp->task_truesender, FTYPE_ADDRESS, sizeof(address));
                BEGINFROMC
                    cbcast_l("mlR", isis_ctp->task_cohorts, rmsg, 0);
                ENDFROMC
            }
            msg_delete(rmsg);
	    use_transport = 0;
        }
	ISIS_EXIT();
  }

void
reply_l(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
        do_reply_l(&ap);
        va_end(ap);
  }

void
do_reply_l(ap)
  va_list *ap;
  {
        register message *rmsg, *msg;
        register msgid, fifo = 0;
        register excl = 0, options = 0;
        register char *opstr;
        static address rdests[MAX_PROCS];
        register address *rp;
	ISIS_ENTER();
	opstr = VA_REF(*ap, char*);
	msg = VA_ARG(*ap, message*);
        while(*opstr)
          switch(*opstr++)
          {
            default:  panic("reply_l: bad option");
            case 'm': options |= ISISBC_MSEND; continue;
            case 'c': options |= ISISBC_CC; continue;
            case 'f': fifo++; continue;
            case 'x': excl++; continue;
          }
        rp = rdests;
        if(isis_ctp->task_cohorts == 0)
        {
            *rp = *msg_getreplyto(msg);
            rp++->addr_entry = GENERIC_RCV_REPLY;
        }
        else
        {
            register address *xrp = isis_ctp->task_cohorts-1;
            do
                *rp++ = *++xrp;
            while(!addr_isnull(xrp));
        }
        if(options&ISISBC_CC)
        {
            register address *xrp; 
            xrp = VA_ARG(*ap, address*)-1; 
            do
                *rp++ = *++xrp;
            while(!addr_isnull(xrp));
        }
        *rp = NULLADDRESS;
        if(rp >= &rdests[MAX_PROCS])
            panic("too many destinations in a reply message\n");
        if(msgid = msg_getid(msg))
        {
	    if(options&ISISBC_MSEND)
	        rmsg = VA_ARG(*ap, message*);
            else
            {
                rmsg = msg_newmsg();
                msg_doputf(rmsg, SYSFLD_SCAN, ap);
            }
            msg_setid(rmsg, msgid);
            if(isis_ctp->task_cohorts)
                msg_insertfield(msg, FLD_TRUESENDER, (char*)&isis_ctp->task_truesender, FTYPE_ADDRESS, sizeof(address));
            BEGINFROMC
		if(fifo == 0)
                    cbcast_l(excl? "mxlR": "mlR", rdests, rmsg, 0);
	        else if(excl || !addr_isnull(&rdests[1]) || intercl_trysend(rdests, rmsg) == 0)
		    fbcast_l(excl? "mxlR": "mlR", rdests, rmsg, 0);
            ENDFROMC
            if((options&ISISBC_MSEND) == 0)
                msg_delete(rmsg);
        }
        else
            panic("reply_l: replying to non-RPC message");
        isis_ctp->task_cohorts = 0;
	ISIS_EXIT();
  }

/* Forward <fmsg> to <to.ent> by actually delivering <cmsg> to that dest */
void
forward(fmsg, to, ent, cmsg)
  message *fmsg, *cmsg;
  address *to;
  int ent;
  {
        static address dests[3], finfo[2];
        register msgid;
        if(!addr_ispid(to))
            panic("forward: illegal to forward to a group");
	ISIS_ENTER();
        dests[0] = *msg_getreplyto(fmsg);
        dests[1].addr_entry = GENERIC_RCV_REPLY;
        dests[1] = *to;
        dests[1].addr_entry = ent;
        if(msgid = msg_getid(fmsg))
        {
            finfo[FWI_SENDER] = *msg_getsender(fmsg);
            finfo[FWI_NEWDEST] = *to;
            msg_setforwarder(cmsg, finfo);
            if(cmsg != fmsg)
            {
                /* Arrange for replies to <cmsg> to be treated like replies to <fmsg> */
                msg_setid(cmsg, msgid);
                msg_setreplyto(cmsg, &dests[0]);
            }
            cbcast_l("ml", dests, cmsg, 0);
        }
        else
            panic("forward: attempt to forward a non-RPC message!");
	ISIS_EXIT();
  }
