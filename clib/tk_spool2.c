/*  $RCSfile: tk_spool2.c,v $ $Revision: 2.10 $ $Date: 90/07/12 13:39:34 $  */
/*
 *      Originally coded by Ken Birman
 *      Interface to spooler spooling routines
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


#define ISIS_SYS
#include "isis.h"
#include "spooler.h"

static	address *spooler;
extern  spool_seqn;
extern  qnode *spool_waitq;	/* Initialized in spool1.c */
extern  struct timeval last_gettimeofday;

void
sp_send(action, sname, sp_msg)
  register char *sname;
  register action;
  register message *sp_msg;
  {
	int ntries = 0;
	static qnode *sp_cache;
	register qnode *qp;

	BEGINFROMC;
	if(addr_isnull(spooler))
	{
	    spooler = pg_lookup("@*:spooler");
	    if(addr_isnull(spooler))
		panic("<isis-spooler>: not operational");
	}
	if(sp_cache == 0)
	    sp_cache = qu_null();
	for(qp = sp_cache->qu_next; qp != sp_cache; qp = qp->qu_next)
	    if(strcmp(qp->qu_data, sname) == 0)
	        break;
  loop: if(qp == sp_cache)
	{
            address spool;
	    extern void free();
	    char *dupl;
	    if(bcast(spooler, SP_LOOKUP, "%s", sname, 1, "%A[1]", &spool) == -1 || addr_isnull(&spool))
                panic("<isis-spooler>: can't create spool for <%s>", sname);
	    dupl = malloc(strlen(sname)+1);
	    bcopy(sname, dupl, strlen(sname)+1);
	    qp = pg_add(sp_cache, &spool, dupl, free);
	}
	if(bcast_l("s", &qp->qu_pname, action, sp_msg, 0) == -1)  /* ??? get a reply ??? */
	{
	    isis_perror("<isis-spooler>: bcast error");
	    if(ntries++)
		panic("<isis-spooler>: too many bcast errors");
	    qu_free(qp);
	    goto loop;
	}
	ENDFROMC;
  }

void
sp_control_send(action, sp_msg)
  register action;
  register message *sp_msg;
  {
	int ntries = 0;
	static qnode *sp_cache;
	register qnode *qp;

	BEGINFROMC;
	if(addr_isnull(spooler))
	{
	    spooler = pg_lookup("@*:spooler");
	    if(addr_isnull(spooler))
		panic("<isis-spooler>: not operational");
	}
	if(bcast_l("s", spooler, action, sp_msg, 0) < 0)
	  panic("<isis-spooler>: can't send to spooler");
	ENDFROMC;
  }

#define	MESSAGE		0
#define	PATTERN		1

static int
gen_spmsg(sp_msg, ap, pat)
  register message *sp_msg;
  register va_list *ap;
  int pat;
  {
	register key, prepend = 0;
	struct timeval time;
	gettimeofday(&time, (struct timezone*)0);
	last_gettimeofday = time;
        BEGINFROMC;
	msg_putfld(sp_msg, SYSFLD_SPSCAN, "%d,%d,%d%d%A[1]", 
	    SP_TIME, time.tv_sec, time.tv_sec,
            SP_SENDER, msg_getsender(sp_msg));
        ENDFROMC;
	while(key = VA_ARG(*ap, int))
	{
	    switch(key)
	    {
                register char *str;
		register lo, hi;
		register address *addr_p;
              default:
		lo = VA_ARG(*ap, int);
                if(pat)
		    hi = VA_ARG(*ap, int);
		else
		    hi = lo;
                BEGINFROMC;
		msg_putfld(sp_msg, SYSFLD_SPSCAN, "%d,%d,%d", key, lo, hi);
                ENDFROMC;
		continue;
	      case SP_KEYWORDS:
                BEGINFROMC;
		msg_putfld(sp_msg, SYSFLD_SPSCAN, "%d", key);
                ENDFROMC;
		while(str = VA_REF(*ap, char*))
                {
                    BEGINFROMC;
		    msg_putfld(sp_msg, SYSFLD_SPSCAN, "%s", str);
                    ENDFROMC;
		}
		BEGINFROMC;
		msg_putfld(sp_msg, SYSFLD_SPSCAN, "%s", "--END--");
		ENDFROMC;
		continue;
	      case SP_CBCAST:
	      case SP_ABCAST:
	      case SP_GBCAST:
		BEGINFROMC;
                msg_putfld(sp_msg, SYSFLD_SPSCAN, "%d%d", SP_BCAST, key);
		ENDFROMC;
		continue;
	      case SP_PREPEND:
		++prepend;
                BEGINFROMC;
		msg_putfld(sp_msg, SYSFLD_SPSCAN, "%d,%d,%d", key, 1, 1);
	        ENDFROMC;
                continue;
	      case SP_ATIME:
	      case SP_RTIME:
		lo = VA_ARG(*ap, int);
		hi = VA_ARG(*ap, int);
                if(key == SP_RTIME)
                {
                    lo += time.tv_sec;
                    hi += time.tv_sec;
		}
                BEGINFROMC;
		msg_putfld(sp_msg, SYSFLD_SPSCAN, "%d%d%d", SP_TIME, lo, hi);
	        ENDFROMC;
		continue;
	      case SP_SENDER:
	      case SP_SPOOLER:
		addr_p = VA_ARG(*ap, address*);
                BEGINFROMC;
		msg_putfld(sp_msg, SYSFLD_SPSCAN, "%d, %A[1]", key, addr_p);
                ENDFROMC;
		continue;
              case SP_NETWORK:
                BEGINFROMC;
                msg_putfld(sp_msg, SYSFLD_NETWORK, "%s", VA_REF(*ap, char*));
                ENDFROMC;
	    }
	}
	return(prepend);
  }

void
spool(va_alist)
  va_dcl
  {
	va_list ap;
        va_start(ap);
        do_spool(&ap, 0);
	va_end(ap);
  }

void
spool_m(va_alist) 
  va_dcl
  {
	va_list ap;
        va_start(ap);
        do_spool(&ap, 1); 
        va_end(ap); 
  } 

void
do_spool(ap, m_mode)
  register va_list *ap;
  int m_mode;
  {
	register char *sname;
	register entry;
	register message *sp_msg;
	ISIS_ENTER();
	sname = VA_REF(*ap, char*);
	entry = VA_ARG(*ap, int);
	if(m_mode == 0)
	{
	    sp_msg = msg_newmsg();
	    if(msg_doputf(sp_msg, SYSFLD_SCAN, ap) == -1)
	    {
	        msg_delete(sp_msg);
	        ISIS_EXIT();
	    }
	}
	else
	{
	    sp_msg = VA_ARG(*ap, message*);
	    msg_increfcount(sp_msg);
	}
        BEGINFROMC;
	msg_putfld(sp_msg, SYSFLD_SPSCAN, "%s%d", sname, entry);
        ENDFROMC;
	gen_spmsg(sp_msg, ap, MESSAGE);
	sp_send(SP_SPOOL, sname, sp_msg);
	msg_delete(sp_msg);
	ISIS_EXIT();
  }

void
spool_replay(va_alist)
  va_dcl
  {
	va_list ap;
	va_start(ap);
	do_spool_replay(&ap);
	va_end(ap);
  }

void
do_spool_replay(ap)
  register va_list *ap;
  {
	register char *sname;
	register message *sp_msg;
	ISIS_ENTER();
	++spool_in_replay;
	spool_seqn = -1;
	sname = VA_REF(*ap, char*);
	sp_msg = msg_newmsg();
        BEGINFROMC;
	msg_putfld(sp_msg, SYSFLD_SPSCAN, "%s%d", sname, 0);
        ENDFROMC;
	gen_spmsg(sp_msg, ap, PATTERN);
	sp_send(SP_REPLAY, sname, sp_msg);
	msg_delete(sp_msg);
	--spool_in_replay;
	ISIS_EXIT();
  }

void
spool_discard(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
	do_spool_discard(&ap);
        va_end(ap);
  }

void
do_spool_discard(ap)
  register va_list *ap;
  {
	register char *sname;
	register message *sp_msg;
	ISIS_ENTER();
	sname = VA_REF(*ap, char*);
	sp_msg = msg_newmsg();
        BEGINFROMC;
	msg_putfld(sp_msg, SYSFLD_SPSCAN, "%s%d", sname, 0);
        ENDFROMC;
	gen_spmsg(sp_msg, ap, PATTERN);
	sp_send(SP_DISCARD, sname, sp_msg);
	msg_delete(sp_msg);
	ISIS_EXIT();
  }

void
spool_and_discard(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
	do_spool_and_discard(&ap, 0);
	va_end(ap);
  }

void
spool_m_and_discard(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
	do_spool_and_discard(&ap, 1);
	va_end(ap);
  }

void
do_spool_and_discard(ap, m_mode)
  register va_list *ap;
  int m_mode;
  {
	register char *sname;
	register entry, prepend;
	register message *sp_msg;
	ISIS_ENTER();
	sname = VA_REF(*ap, char*);
	entry = VA_ARG(*ap, int);
	if(m_mode == 0)
	{
	    sp_msg = msg_newmsg();
	    if(msg_doputf(sp_msg, SYSFLD_SCAN, ap) == -1)
	    {
	        msg_delete(sp_msg);
	        ISIS_EXIT();
	    }
	}
	else
	{
	    sp_msg = VA_ARG(*ap, message*);
	    msg_increfcount(sp_msg);
	}
        BEGINFROMC;
	msg_putfld(sp_msg, SYSFLD_SPSCAN, "%s%d", sname, entry);
        ENDFROMC;
	prepend = gen_spmsg(sp_msg, ap, MESSAGE);
        BEGINFROMC;
	msg_putfld(sp_msg, SYSFLD_SPSCAN, "%d", 0);
        ENDFROMC;
	gen_spmsg(sp_msg, ap, PATTERN);
	sp_send(prepend? SP_PREPEND_AND_DISCARD: SP_SPOOL_AND_DISCARD, sname, sp_msg);
	msg_delete(sp_msg);
	ISIS_EXIT();
  }

int
spool_inquire(sname, sid)
  char *sname;
  int sid;
  {
	isis_errno = IE_NOTIMP;
	return(-1);
  }

int
spool_cancel(sname, sid)
  char *sname;
  int sid;
  {
	isis_errno = IE_NOTIMP;
	return(-1);
  }

int
spool_wait(sname, sid)
  char *sname;
  int sid;
  {
	isis_errno = IE_NOTIMP;
	return(-1);
  }

void
spool_play_through(sname, on_off)
  char *sname;
  int on_off;
  {
	register message *mp;
	ISIS_ENTER();
	mp = msg_gen("%s,%d", sname, on_off);
	sp_send(SP_PLAY_THROUGH, sname, mp);
	msg_delete(mp);
	ISIS_EXIT();
  }

void
spool_set_replay_pointer(sname, spseqn)
  char *sname;
  int spseqn;
  {
	register message *mp;
	ISIS_ENTER();
	mp = msg_gen("%s,%d", sname, spseqn);
	sp_send(SP_SET_RPOINTER, sname, mp);
	msg_delete(mp);
	ISIS_EXIT();
  }

void
spool_set_ckpt_pointer(sname, spseqn)
  char *sname;
  int spseqn;
  {
	register message *mp;
	ISIS_ENTER();
	mp = msg_gen("%s,%d", sname, spseqn);
	sp_send(SP_SET_CPOINTER, sname, mp);
	msg_delete(mp);
	ISIS_EXIT();
  }

