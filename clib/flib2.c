/*  $RCSfile: flib2.c,v $ $Revision: 2.22 $ $Date: 90/07/25 13:50:13 $  */
/*
 * ISIS interface from Fortran 
 * (some of these routines are called from lisp too)
 * Contains flib1.c routines but with _ included again...
 *
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
 */
 
#include "isis.h"
#include "spooler.h"

int
abcast_l_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_ABCAST, VA_REF(ap, char *), &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
bcast_l_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_ABCAST, VA_REF(ap, char *), &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
addr_cmp_(a, b)
  address **a, **b;
  {
        return addr_cmp(*a, *b);
  }

int
addr_ismine_(a)
  address **a;
  {
        return addr_ismine(*a);
  }

int
addr_isnull_(a)
  address **a;
  {
        return addr_isnull(*a);
  }

int
addr_isequal_(a, b)
  address **a, **b;
  {
	return addr_isequal(*a, *b);
  }

int
addr_process_(a)
  address **a;
  {
        return (*a)->addr_process;
  }

int
addr_entry_(a)
  address **a;
  {
        return (*a)->addr_entry;
  }

int
addr_site_(a)
  address **a;
  {
        return (*a)->addr_site;
  }

int
addr_incarn_(a)
  address **a;
  {
        return (*a)->addr_incarn;
  }

int
alist_len_(alist)
  address *alist;
  {
        return alist_len(alist);
  }

int
bc_cancel_(bcid)
  int *bcid;
  {
        return bc_cancel(*bcid);
  }

int
bc_getevent_(bcid)
  int *bcid;
  {
        event_id *bc_getevent();
        return (int)bc_getevent(*bcid);
  }

int
bc_poll_(bcid)
  int *bcid;
  {
        return bc_poll(*bcid);
  }

int
bc_wait_(bcid)
  int *bcid;
  {
        return bc_wait(*bcid);
  }

int
cbcast_l_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_CBCAST, VA_REF(ap, char *), &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

void
cc_terminate_l_(va_alist)
  va_dcl
  {
	address *dest;
        va_list ap;
        va_start(ap);
        BEGINFROMFORTRAN
	    dest = VA_ARG(ap, address*);
            do_ccterminate(dest, &ap);
        ENDFROMFORTRAN
        va_end(ap);
  }

void
cc_terminate_(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
        BEGINFROMFORTRAN
            do_ccterminate((address*)0, &ap);
        ENDFROMFORTRAN
        va_end(ap);
  }

void
cc_refuse_()
  {
	cc_refuse();
  }

#ifndef MACH
static char *imsg = "**** subroutine/function parameter was not declared external";
#define is_ifunc(routine) { if(((char**)routine) && *((char**)routine) == (char*)0) panic(imsg); }
#else
#define is_ifunc(routine)
#endif

int
coord_cohort_(mp, gaddr, action, gotresult, arg)
  message **mp;
  address **gaddr;
  vfunc *action, *gotresult;
  void **arg;
  {
        is_ifunc(action);
        is_ifunc(gotresult);
        return coord_cohort(*mp, *gaddr, action, gotresult, *arg);
  }

int
eid_sender_(eid)
  event_id **eid;
  {
	address *eid_sender();
        return (int)eid_sender(*eid);
  }

int
fbcast_l_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_FBCAST, VA_REF(ap, char *), &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
gbcast_l_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_GBCAST, VA_REF(ap, char *), &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

void
isis_accept_events_(flag)
  int *flag;
  {
        isis_accept_events(*flag);
  }

int
isis_define_type_(formatletter, size, converter)
  int *formatletter, *size;
  vfunc *converter;
  {
        is_ifunc(converter);
        return(isis_define_type(*formatletter, *size, converter));
  }

void
isis_disconnect_()
  {
        isis_disconnect();
  }

void
isis_entry_(entry, routine, rname)
  int *entry;
  vfunc *routine;
  char *rname;
  {
	is_ifunc(routine);
	BEGINFROMFORTRAN;
            isis_entry(*entry, routine, rname);
	ENDFROMFORTRAN;
  }

void
isis_entrystacksize_(entry, size)
  int *entry, *size;
  {
        isis_entry_stacksize(*entry, *size);
  }

int
isis_init_(CLIENTPORT)
  int *CLIENTPORT;
  {
        return isis_init(*CLIENTPORT);
  }

void
isis_input_(fdes, handler, arg)
  int *fdes;
  char **arg;
  vfunc *handler;
  {
	is_ifunc(handler);
        isis_input(*fdes, handler, *arg);
  }

void
isis_except_(fdes, handler, arg)
  int *fdes;
  char **arg;
  vfunc *handler;
  {
	is_ifunc(handler);
        isis_except(*fdes, handler, *arg);
  }

int
isis_wait_(max, imask, omask, emask, tp)
  int *max, *imask, *omask, *emask;
  struct timeval *tp;
  {
        return isis_wait(*max, imask, omask, emask, tp);
  }

void
isis_waitcancel_(wid)
  int *wid;
  {
	isis_wait_cancel(*wid);
  }

int
isis_output_(fdes, handler, arg)
  int *fdes;
  char **arg;
  vfunc *handler;
  {
	is_ifunc(handler);
        return(isis_output(*fdes, handler, *arg));
  }

int
isis_input_sig_(fdes, cond, arg)
  int *fdes;
  condition *cond;
  char **arg;
  {
        return(isis_input_sig(*fdes, cond, *arg));
  }

int
isis_output_sig_(fdes, cond, arg)
  int *fdes;
  condition *cond;
  char **arg;
  {
        return(isis_output_sig(*fdes, cond, *arg));
  }

int
isis_except_sig_(fdes, cond, arg)
  int *fdes;
  condition *cond;
  char **arg;
  {
        return(isis_except_sig(*fdes, cond, *arg));
  }

void
isis_logging_(flag)
  int *flag;
  {
        isis_logging(*flag);
  }

void
isis_mainloop_(routine, arg0)
  vfunc *routine;
  char **arg0;
  {
        isis_mainloop(routine, arg0? *arg0: (char *) 0);
  }

void
isis_perror_(str)
  char *str;
  {
        isis_perror(str);
  }

int
isis_rexec_(nwanted, gid, sites, prog, args, env, user, passwd, addrs)
  int *nwanted;
  address **gid, **addrs;
  site_id *sites;
  char *prog, **args, **env, *user, *passwd;
  {
        return isis_rexec(*nwanted, *gid, sites, prog, args, env, user, passwd, *addrs);
  }

int
isis_signal_(signo, handler, arg)
  int *signo;
  vfunc *handler;
  char **arg;
  {
	is_ifunc(handler);
        return(isis_signal(*signo, handler, *arg));
  }

int
isis_signal_sig_(signo, cp, arg)
  int *signo;
  condition *cp;
  char **arg;
  {
        return(isis_signal_sig(*signo, cp, *arg));
  }

int
isis_sleep_(n)
  int *n;
  {
        return isis_sleep(*n);
  }

int
isis_sleepms_(ms)
  int *ms;
  {
        return isis_sleep_ms(*ms);
  }

void
isis_startdone_()
  {
        isis_start_done();
  }

void
isis_task_(routine, rname)
  vfunc *routine;
  char *rname;
  {
	is_ifunc(routine);
        isis_task(routine, rname);
  }

int
isis_timeout_(time, routine, arg0, arg1)
  int *time;
  char **arg0, **arg1;
  vfunc *routine;
  {
	is_ifunc(routine);
        return isis_timeout(*time, routine, *arg0, *arg1);
  }

int
isis_timeout_reschedule_(oldtid, time, routine, arg0, arg1)
  int *oldtid, *time;
  char **arg0, **arg1;
  vfunc *routine;
  {
	is_ifunc(routine);
        return isis_timeout_reschedule(*oldtid, *time, routine, *arg0, *arg1);
  }

void
isis_logentry_(gaddr, entry)
  address **gaddr;
  int *entry;
  {
        isis_logentry(*gaddr, *entry);
  }

int
log_checkpoint_(gname)
  char *gname;
  {
        return log_checkpoint(gname);
  }

int
log_flush_(gaddr)
  address **gaddr;
  {
        return log_flush(*gaddr);
  }

int
log_hasckpt_(gname)
  char *gname;
  {
        return log_has_ckpt(gname);
  }

int
log_recovered_(gaddr)
  address **gaddr;
  {
        return log_recovered(*gaddr);
  }

int
log_write_(gaddr, mp)
  address **gaddr;
  message **mp;
  {
        return log_write(*gaddr, *mp);
  }

void
msg_delete_(mp)
  message **mp;
  {
        msg_delete (*mp);
  }

int
msg_read_(sock)
  int *sock;
  {
        return (int)msg_read(*sock);
  }

int
msg_write_(sock, mp)
  int *sock;
  message **mp;
  {
        return msg_write(*sock, *mp);
  }

int
msg_fread_(file)
  FILE *file;
  {
        return (int)msg_fread(file);
  }

int
msg_fwrite_(file, mp)
  FILE *file;
  message **mp;
  {
        return msg_fwrite(file, *mp);
  }

int
msg_gen_(va_alist)
  va_dcl
  {
        va_list ap;
        register message *mp;
        register rval;
	ISIS_ENTER();
        mp = msg_newmsg();
        va_start(ap);
        BEGINFROMFORTRAN
            rval = msg_doputf(mp, SYSFLD_SCAN, &ap);
        ENDFROMFORTRAN
        va_end(ap);
        if(rval == 0)
            ISIS_RETURN((int)mp);
        msg_delete(mp);
        ISIS_RETURN(0);
  }

int
msg_get_(va_alist)
  va_dcl
  {
        va_list ap;
        message *mp;
        int rval;
        va_start(ap);
        BEGINFROMFORTRAN
	    mp = VA_ARG(ap, message*);
            rval = msg_dogetf(&mp, 1, SYSFLD_SCAN, &mp->msg_fpointer, &ap);
        ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
msg_getfld_(va_alist)
  va_dcl
  {
        int one = 1;
        va_list ap;
        message *mp;
        register fname, *fpointer, rval;
        va_start(ap);
        BEGINFROMFORTRAN
	    mp = VA_ARG(ap, message*);
	    fname = VA_ARG(ap, int);
            fpointer = VA_REF(ap, int*);
            if(fpointer && *fpointer == 0)
                fpointer = &one;
            rval = msg_dogetf(&mp, 1, fname, fpointer, &ap);
        ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
msg_getlen_(mp)
  message **mp;
  {
	return msg_getlen(*mp);
  }

int
msg_isforwarded_(mp)
  message **mp;
  {
	return msg_isforwarded(*mp);
  }

int
msg_dests_(mp)
  message **mp;
  {
        return (int)msg_getdests (*mp);
  }

int
msg_replyto_(mp)
  message **mp;
  {
        return (int)msg_getreplyto (*mp);
  }

int
msg_sender_(mp)
  message **mp;
  {
        return (int)msg_getsender (*mp);
  }

int
msg_getforwarder_(mp)
  message **mp;
  {
        return (int)msg_getforwarder (*mp);
  }

int
msg_gettype_(mp, field, inst)
  message **mp;
  int *field, *inst;
  {
        return msg_gettype(*mp, *field, *inst);
  }

void
msg_increfcount_(mp)
  message **mp;
  {
        msg_increfcount (*mp);
  }

void
msg_makelazy_(mp, howlazy)
  message **mp;
  int *howlazy;
  {
        msg_makelazy (*mp, *howlazy);
  }

int
msg_newmsg_()
  {
        return (int)msg_newmsg ();
  }

int
msg_put_(va_alist)
  va_dcl
  {
        va_list ap;
        message *mp;
        register rval;
        va_start(ap);
        BEGINFROMFORTRAN
            mp = VA_ARG(ap, message*);
            rval = msg_doputf(mp, SYSFLD_SCAN, &ap);
        ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
msg_putfld_(va_alist)
  va_dcl
  {
        va_list ap;
        message *mp;
        register rval, fname;
        va_start(ap);
        BEGINFROMFORTRAN
            mp = VA_ARG(ap, message*);
            fname = VA_ARG(ap, int);
            rval = msg_doputf(mp, fname, &ap);
        ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
msg_rewind_(mp)
  message **mp;
  {
        return msg_rewind(*mp);
  }

void
news_apost_(slist, subj, mp, back)
  site_id *slist;
  char *subj;
  message **mp;
  int *back;
  {
        news_apost(slist, subj, *mp, *back);
  }

int
news_cancel_(subj)
  char *subj;
  {
        return news_cancel(subj);
  }

void
news_clear_(slist, subj)
  site_id  slist[];
  char     *subj;
  {
        news_clear(slist, subj);
  }

void
news_clear_all_(slist, subj)
  site_id  slist[];
  char     *subj;
  {
        news_clear_all(slist, subj);
  }

int
news_subscribe_(subj, entry, back)
  char *subj;
  int *entry, *back;
  {
        return news_subscribe(subj, *entry, *back);
  }
int
pg_client_(gaddr, credentials)
  address **gaddr;
  char *credentials;
  {
        return (int)pg_client(*gaddr, credentials);
  }

void
pg_client_verifier_(gname, routine)
  char *gname;
  ifunc *routine;
  {
	is_ifunc(routine);
        pg_client_verifier(gname, routine);
  }

int
pg_delete_(gaddr)
  address **gaddr;
  {
        return pg_delete(*gaddr);
  }

int
pg_getlocalview_(gaddr)
  address **gaddr;
  {
        return (int)pg_getlocalview(*gaddr);
  }

int
pg_getview_(gaddr)
  address **gaddr;
  {
        return (int)pg_getview(*gaddr);
  }

int
pg_join_(va_alist)
  va_dcl
  {
        address *addr, *pg_dojoin();
	va_list ap;
        va_start(ap); 
        BEGINFROMFORTRAN
            addr = pg_dojoin(&ap); 
        ENDFROMFORTRAN
        va_end(ap); 
	return (int)addr;
  }

void
xfer_out_(va_alist)
  va_dcl
  {
        int locator;
        va_list ap;
        message *mp;
        va_start(ap);
	ISIS_ENTER();
        BEGINFROMFORTRAN
            locator = VA_ARG(ap, int);
            mp = msg_newmsg();
            msg_doputf(mp, SYSFLD_SCAN, &ap);
        ENDFROMFORTRAN
        do_xfer_out(locator, mp);
	va_end(ap);
        msg_delete(mp);
	ISIS_EXIT();
  }

void
xfer_flush_()
  {
	xfer_flush();
  }

void
xfer_refuse_()
  {
	xfer_refuse();
  }

void
pg_join_inhibit_(flag)
  int *flag;
  {
        pg_join_inhibit(*flag);
  }

void
pg_leave_(gaddr)
  address **gaddr;
  {
        pg_leave(*gaddr);
  }

int
pg_lookup_(gname)
  char *gname;
  {
        return (int)pg_lookup(gname);
  }

int
pg_monitor_(gaddr, routine, arg)
  address **gaddr;
  vfunc *routine;
  void **arg;
  {
        int rval;
	is_ifunc(routine);
        BEGINFROMFORTRAN
            rval = pg_monitor(*gaddr, routine, *arg);
        ENDFROMFORTRAN
        return rval;
  }

int
pg_monitor_cancel_(pwid)
  int *pwid;
  {
        return pg_monitor_cancel(*pwid);
  }

int
pg_rank_(gaddr, who)
  address **gaddr, **who;
  {
        return pg_rank(*gaddr, *who);
  }

int
pg_rank_all_(gaddr, who)
  address **gaddr, **who;
  {
        return pg_rank_all(*gaddr, *who);
  }

int
pg_signal_(gaddr, signo)
  address **gaddr;
  int *signo;
  {
        return pg_signal(*gaddr, *signo);
  }

int
pg_subgroup_(gaddr, sgname, incarn, mlist, clist)
  address **gaddr, **mlist, **clist;
  char *sgname;
  int *incarn;
  {
        address *pg_subgroup();
        return (int)pg_subgroup(*gaddr, sgname, *incarn, *mlist, *clist);
  }

int
pg_watch_(gaddr, who, event, routine, arg)
  address **gaddr, **who;
  int *event;
  void **arg;
  vfunc *routine;
  {
        int rval;
	is_ifunc(routine);
        BEGINFROMFORTRAN
            rval = pg_watch(*gaddr, *who, *event, routine, *arg);
        ENDFROMFORTRAN
        return rval;
  }

int
pg_watchcancel_(wid)
  int *wid;
  {
        return pg_watch_cancel(*wid);
  }

int
proc_watch_(paddr, routine, arg)
  address **paddr;
  vfunc *routine;
  void **arg;
  {
        int rval;
	is_ifunc(routine);
        BEGINFROMFORTRAN
            rval = proc_watch(*paddr, routine, *arg);
        ENDFROMFORTRAN
        return rval;
  }

int
proc_watch_cancel_(wid)
  int *wid;
  {
        return proc_watch_cancel(*wid);
  }

void
reply_l_(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
        BEGINFROMFORTRAN
            do_reply_l(&ap);
        ENDFROMFORTRAN
        va_end(ap);
  }

int
rmgr_create_(rmi)
  rmgr_info *rmi;
  {
        return (int)rmgr_create(rmi);
  }

int
rmgr_getinfo_(pgname, noblock)
  char *pgname;
  int *noblock;
  {
        return (int)rmgr_getinfo(pgname, *noblock);
  }

int
rmgr_join_(rmi)
  rmgr_info *rmi;
  {
        return (int)rmgr_join(rmi);
  }

int
rmgr_lasttofail_(gname, key, oldincarnp, noblock)
  char  *gname, *key;
  int   *oldincarnp, *noblock;
  {
        return rmgr_lasttofail(gname, key, oldincarnp, *noblock);
  }

int
rmgr_register_(key)
  char *key;
  {
        return rmgr_register(key);
  }

int
rmgr_restart_(pgname)
  char *pgname;
  {
        return (int)rmgr_restart(pgname);
  }

int
rmgr_start_log_(gad, key)
  address **gad;
  char *key;
  {
        return rmgr_start_log(*gad, key);
  }

int
rmgr_stop_log_(gad, key)
  address **gad;
  char *key;
  {
        return rmgr_stop_log(*gad, key);
  }

int
rmgr_unregister_()
  {
        return rmgr_unregister();
  }

int
rmgr_update_(key, program, argv, envp)
  char *key, *program, **argv, **envp;
  {
        return rmgr_update(key, program, argv, envp);
  }

int
site_getview_()
  {
        return (int)site_getview();
  }

int
sv_viewid_(sv)
  sview **sv;
  {
	return (*sv)->sv_viewid;
  }

int
sv_slist_(sv, i)
  sview **sv;
  int *i;
  {
        return (*sv)->sv_slist[*i];
  }

int
sv_incarn_(sv, i)
  sview **sv;
  int *i;
  {
        return (int)(*sv)->sv_incarn[*i];
  }

int
sv_failed_(sv)
  sview **sv;
  {
        return (int)&(*sv)->sv_failed;
  }

int
sv_recovered_(sv)
  sview **sv;
  {
        return (int)&(*sv)->sv_recovered;
  }

int
sv_monitor_(routine, arg)
  vfunc *routine;
  void **arg;
  {
        int rval;
	is_ifunc(routine);
        BEGINFROMFORTRAN
            rval = sv_monitor(routine, *arg);
        ENDFROMFORTRAN
        return rval;
  }

int
sv_monitor_cancel_(svid)
  int *svid;
  {
        return sv_monitor_cancel(*svid);
  }

int
sv_watch_(sid, event, routine, arg)
  site_id *sid;
  vfunc *routine;
  int *event;
  void **arg;
  {
        int rval;
	is_ifunc(routine);
        BEGINFROMFORTRAN
            rval = sv_watch(*sid, *event, routine, *arg);
        ENDFROMFORTRAN
        return rval;
  }

int
sv_watch_cancel_(svid)
  int *svid;
  {
        return sv_watch_cancel(*svid);
  }

int
t_holder_(gaddr, name)
  address **gaddr;
  char *name;
  {
	address *t_holder();
        return (int)t_holder(*gaddr, name);
  }

int
t_on_sys_stack_(routine, arg)
  ifunc *routine;
  char **arg;
  {
	is_ifunc(routine);
        return t_on_sys_stack(routine, *arg);
  }

int
t_pass_(gaddr, name)
  address **gaddr;
  char *name;
  {
        return t_pass(*gaddr, name);
  }

int
t_request_(gaddr, name, passonfail)
  address **gaddr;
  char *name;
  int *passonfail;
  {
        return t_request(*gaddr, name, *passonfail);
  }

void
t_scheck_()
  {
        t_scheck();
  }

void
t_fork_(routine, arg)
  vfunc *routine;
  char **arg;
  {
	BEGINFROMFORTRAN;
            t_fork(routine, arg? *arg: (char *) 0);
        ENDFROMFORTRAN;
  }

void
t_fork_urgent_(routine, arg)
  vfunc *routine;
  char **arg;
  {
	BEGINFROMFORTRAN;
            t_fork_urgent(routine, arg? *arg: 0);
        ENDFROMFORTRAN;
  }

void
t_sig_(cp, rval)
  condition *cp;
  void **rval;
  {
        t_sig(cp, *rval);
  }

void
t_sigall_(cp, rval)
  condition *cp;
  void **rval;
  {
        t_sig_all(cp, *rval);
  }

void
t_sigurgent_(cp, rval)
  condition *cp;
  void **rval;
  {
        t_sig_urgent(cp, *rval);
  }

void *
t_yield_()
  {
        return t_yield();
  }

void *
t_wait_(cp)
  condition *cp;
  {
        return t_wait(cp);
  }

void *
t_wait_l_(cp, why)
  condition *cp;
  char *why;
  {
        return t_wait_l(cp, why);
  }

int
x_abort_()
  {
        return x_abort();
  }

int
x_begin_()
  {
        return x_begin();
  }

int
x_commit_(phases)
  int *phases;
  {
        return x_commit(*phases);
  }

x_id *
x_getid_()
  {
        x_id *x_getid();
        return x_getid();
  }

int
xid_cmp_(id1, id2)
  x_id *id1, *id2;
  {
	return xid_cmp(id1, id2);
  }

void
x_outcomes_(xlist, partname)
  x_list *xlist;
  char *partname;
  {
        x_list *x_outcomes(), *xlp;
        xlp = x_outcomes(partname);
        bcopy(xlp, xlist,
              sizeof(x_list) + (sizeof (x_item)) * (xlp-> len  - 1));
  }

void
x_outcomes_flush_(partname, outcomes)
  char *partname;
  x_list *outcomes;
  {
        x_outcomes_flush(partname, outcomes);
  }

int
x_term_(va_alist)
  va_dcl
  {
	char *participant_name;
        typedef bool bfunc();
	bfunc *on_prepare;
	bfunc *on_commit;
	bfunc *on_abort;
        int rval;
	message *data = msg_newmsg();
	va_list vargs;

	va_start(vargs);
        BEGINFROMFORTRAN
	    participant_name = VA_REF(vargs, char *);
	    on_prepare = VA_REF(vargs, bfunc *);
	    on_commit = VA_REF(vargs, bfunc *);
	    on_abort = VA_REF(vargs, bfunc *);
	    rval = msg_doputf(data, SYSFLD_SCAN, &vargs);
	ENDFROMFORTRAN
	va_end(vargs);
	if(rval)
        {
            msg_delete(data);
            return(-1);
        }
	return x_term_msg(participant_name, on_prepare, on_commit, on_abort, data);
  }

int
my_sno_()
  {
        return my_site_no;
  }

int
my_sincarn_()
  {
        return my_site_incarn;
  }

int
my_sid_()
  {
        return my_site_id;
  }

int
my_pid_()
  {
        return my_process_id;
  }

int
my_address_()
  {
        return (int)&my_address;
  }

void
my_host_(where)
  char *where;
  {
        strcpy(where, my_host);
  }

void
site_names_(where, i)
  char *where;
  int *i;
  {
        strcpy(where, site_names[*i]);
  }

int
isis_errno_()
  {
        return isis_errno;
  }

void
isis_dir_(where)
  char *where;
  {
        strcpy(where, isis_dir);
  }

int
isis_nsent_()
  {
        return isis_nsent;
  }

int
isis_nreplies_()
  {
        return isis_nreplies;
  }

int
isis_socket_()
  {
        return isis_socket;
  }

int
isis_state_()
  {
        return isis_state;
  }

int
gv_viewid_(gv)
  register groupview **gv;
  {
        return (*gv)->gv_viewid;
  }

int
gv_incarn_(gv)
  register groupview **gv;
  {
        return (*gv)->gv_incarn;
  }

int
gv_flag_(gv)
  register groupview **gv;
  {
        return (*gv)->gv_flag;
  }

int
gv_gaddr_(gv)
  register groupview **gv;
  {
        return (int)&(*gv)->gv_gaddr;
  }

void
gv_name_(str, gv)
  register groupview **gv;
  register char *str;
  {
        strcpy(str, (*gv)->gv_name);
  }

int
gv_nmemb_(gv)
  register groupview **gv;
  {
        return (*gv)->gv_nmemb;
  }

int
gv_nclient_(gv)
  register groupview **gv;
  {
        return (*gv)->gv_nclient;
  }

int
gv_member_(gv, n)
  register int *n;
  register groupview **gv;
  {
        return (int)&(*gv)->gv_members[*n];
  }

int
gv_client_(gv, n)
  register int *n;
  register groupview **gv;
  {
        return (int)&(*gv)->gv_clients[*n];
  }

int
gv_joined_(gv)
  register groupview **gv;
  {
        return (int)&(*gv)->gv_joined;
  }

int
gv_departed_(gv)
  register groupview **gv;
  {
        return (int)&(*gv)->gv_departed;
  }

void
spool_replay_(va_alist)
  va_dcl
  {
        va_list ap;
	ISIS_ENTER();
        va_start(ap);
        BEGINFROMFORTRAN;
	    do_spool_replay(&ap);
        ENDFROMFORTRAN;
        va_end(ap);
  }

void
spool_discard_(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
        BEGINFROMFORTRAN;
	    do_spool_discard(&ap);
        ENDFROMFORTRAN;
        va_end(ap);
  }

void
spool_and_discard_(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
        BEGINFROMFORTRAN;
	    do_spool_and_discard(&ap, 0);
	ENDFROMFORTRAN;
	va_end(ap);
  }

int
spool_inquire_(sname, sid)
  char *sname;
  int sid;
  {
	isis_errno = IE_NOTIMP;
	return(-1);
  }

int
spool_cancel_(sname, sid)
  char *sname;
  int sid;
  {
	isis_errno = IE_NOTIMP;
	return(-1);
  }

int
spool_wait_(sname, sid)
  char *sname;
  int sid;
  {
	isis_errno = IE_NOTIMP;
	return(-1);
  }

void
spool_play_through_(sname, on_off)
  char *sname;
  int *on_off;
  {
	register message *mp;
	ISIS_ENTER();
	mp = msg_gen("%s,%d", sname, *on_off);
	sp_send(SP_PLAY_THROUGH, sname, mp);
	msg_delete(mp);
	ISIS_EXIT();
  }

void
spool_set_replay_pointer_(sname, spseqn)
  char *sname;
  int *spseqn;
  {
	register message *mp;
	ISIS_ENTER();
	mp = msg_gen("%s,%d", sname, *spseqn);
	sp_send(SP_SET_RPOINTER, sname, mp);
	msg_delete(mp);
	ISIS_EXIT();
  }

void
spool_set_ckpt_pointer_(sname, spseqn)
  char *sname;
  int *spseqn;
  {
	register message *mp;
	ISIS_ENTER();
	mp = msg_gen("%s,%d", sname, *spseqn);
	sp_send(SP_SET_CPOINTER, sname, mp);
	msg_delete(mp);
	ISIS_EXIT();
  }
