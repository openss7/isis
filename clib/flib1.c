/*  $RCSfile: flib1.c,v $ $Revision: 2.22 $ $Date: 90/07/25 13:50:03 $  */
/*
 * ISIS interface from Fortran 
 * (some of these routines are called from lisp too)
 * flib1: underscores supressed
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
abcastl_(va_alist)
	va_dcl
{
	int rval;
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN rval = do_bcast(CL_ABCAST, VA_REF(ap, char *), &ap);

	ENDFROMFORTRAN va_end(ap);

	return rval;
}

int
bcastl_(va_alist)
	va_dcl
{
	int rval;
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN rval = do_bcast(CL_ABCAST, VA_REF(ap, char *), &ap);

	ENDFROMFORTRAN va_end(ap);

	return rval;
}

int
addrcmp_(a, b)
	address **a, **b;
{
	return addr_cmp(*a, *b);
}

int
addrismine_(a)
	address **a;
{
	return addr_ismine(*a);
}

int
addrisnull_(a)
	address **a;
{
	return addr_isnull(*a);
}

int
addrisequal_(a, b)
	address **a, **b;
{
	return addr_isequal(*a, *b);
}

int
addrprocess_(a)
	address **a;
{
	return (*a)->addr_process;
}

int
addrentry_(a)
	address **a;
{
	return (*a)->addr_entry;
}

int
addrsite_(a)
	address **a;
{
	return (*a)->addr_site;
}

int
addrincarn_(a)
	address **a;
{
	return (*a)->addr_incarn;
}

int
alistlen_(alist)
	address *alist;
{
	return alist_len(alist);
}

int
bccancel_(bcid)
	int *bcid;
{
	return bc_cancel(*bcid);
}

int
bcgetevent_(bcid)
	int *bcid;
{
	event_id *bc_getevent();

	return (int) bc_getevent(*bcid);
}

int
bcpoll_(bcid)
	int *bcid;
{
	return bc_poll(*bcid);
}

int
bcwait_(bcid)
	int *bcid;
{
	return bc_wait(*bcid);
}

int
cbcastl_(va_alist)
	va_dcl
{
	int rval;
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN rval = do_bcast(CL_CBCAST, VA_REF(ap, char *), &ap);

	ENDFROMFORTRAN va_end(ap);

	return rval;
}

void
ccterminatel_(va_alist)
	va_dcl
{
	address *dest;
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN dest = VA_ARG(ap, address *);

	do_ccterminate(dest, &ap);
	ENDFROMFORTRAN va_end(ap);
}

void
ccterminate_(va_alist)
	va_dcl
{
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN do_ccterminate((address *) 0, &ap);
	ENDFROMFORTRAN va_end(ap);
}

void
ccrefuse_()
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
coordcohort_(mp, gaddr, action, gotresult, arg)
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
eidsender_(eid)
	event_id **eid;
{
	address *eid_sender();

	return (int) eid_sender(*eid);
}

int
fbcastl_(va_alist)
	va_dcl
{
	int rval;
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN rval = do_bcast(CL_FBCAST, VA_REF(ap, char *), &ap);

	ENDFROMFORTRAN va_end(ap);

	return rval;
}

int
gbcastl_(va_alist)
	va_dcl
{
	int rval;
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN rval = do_bcast(CL_GBCAST, VA_REF(ap, char *), &ap);

	ENDFROMFORTRAN va_end(ap);

	return rval;
}

void
isisacceptevents_(flag)
	int *flag;
{
	isis_accept_events(*flag);
}

int
isisdefinetype_(formatletter, size, converter)
	int *formatletter, *size;
	vfunc *converter;
{
	is_ifunc(converter);
	return (isis_define_type(*formatletter, *size, converter));
}

void
isisdisconnect_()
{
	isis_disconnect();
}

void
isisentry_(entry, routine, rname)
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
isisentrystacksize_(entry, size)
	int *entry, *size;
{
	isis_entry_stacksize(*entry, *size);
}

int
isisinit_(CLIENTPORT)
	int *CLIENTPORT;
{
	return isis_init(*CLIENTPORT);
}

void
isisinput_(fdes, handler, arg)
	int *fdes;
	char **arg;
	vfunc *handler;
{
	is_ifunc(handler);
	isis_input(*fdes, handler, *arg);
}

void
isisexcept_(fdes, handler, arg)
	int *fdes;
	char **arg;
	vfunc *handler;
{
	is_ifunc(handler);
	isis_except(*fdes, handler, *arg);
}

int
isiswait_(max, imask, omask, emask, tp)
	int *max, *imask, *omask, *emask;
	struct timeval *tp;
{
	return isis_wait(*max, imask, omask, emask, tp);
}

void
isiswaitcancel_(wid)
	int *wid;
{
	isis_wait_cancel(*wid);
}

int
isisoutput_(fdes, handler, arg)
	int *fdes;
	char **arg;
	vfunc *handler;
{
	is_ifunc(handler);
	return (isis_output(*fdes, handler, *arg));
}

int
isisinputsig_(fdes, cond, arg)
	int *fdes;
	condition *cond;
	char **arg;
{
	return (isis_input_sig(*fdes, cond, *arg));
}

int
isisoutputsig_(fdes, cond, arg)
	int *fdes;
	condition *cond;
	char **arg;
{
	return (isis_output_sig(*fdes, cond, *arg));
}

int
isisexceptsig_(fdes, cond, arg)
	int *fdes;
	condition *cond;
	char **arg;
{
	return (isis_except_sig(*fdes, cond, *arg));
}

void
isislogging_(flag)
	int *flag;
{
	isis_logging(*flag);
}

void
isismainloop_(routine, arg0)
	vfunc *routine;
	char **arg0;
{
	isis_mainloop(routine, arg0 ? *arg0 : (char *) 0);
}

void
isisperror_(str)
	char *str;
{
	isis_perror(str);
}

int
isisrexec_(nwanted, gid, sites, prog, args, env, user, passwd, addrs)
	int *nwanted;
	address **gid, **addrs;
	site_id *sites;
	char *prog, **args, **env, *user, *passwd;
{
	return isis_rexec(*nwanted, *gid, sites, prog, args, env, user, passwd, *addrs);
}

int
isissignal_(signo, handler, arg)
	int *signo;
	vfunc *handler;
	char **arg;
{
	is_ifunc(handler);
	return (isis_signal(*signo, handler, *arg));
}

int
isissignalsig_(signo, cp, arg)
	int *signo;
	condition *cp;
	char **arg;
{
	return (isis_signal_sig(*signo, cp, *arg));
}

int
isissleep_(n)
	int *n;
{
	return isis_sleep(*n);
}

int
isissleepms_(ms)
	int *ms;
{
	return isis_sleep_ms(*ms);
}

void
isisstartdone_()
{
	isis_start_done();
}

void
isistask_(routine, rname)
	vfunc *routine;
	char *rname;
{
	is_ifunc(routine);
	isis_task(routine, rname);
}

int
isistimeout_(time, routine, arg0, arg1)
	int *time;
	char **arg0, **arg1;
	vfunc *routine;
{
	is_ifunc(routine);
	return isis_timeout(*time, routine, *arg0, *arg1);
}

int
isistimeoutreschedule_(oldtid, time, routine, arg0, arg1)
	int *oldtid, *time;
	char **arg0, **arg1;
	vfunc *routine;
{
	is_ifunc(routine);
	return isis_timeout_reschedule(*oldtid, *time, routine, *arg0, *arg1);
}

void
isislogentry_(gaddr, entry)
	address **gaddr;
	int *entry;
{
	isis_logentry(*gaddr, *entry);
}

int
logcheckpoint_(gname)
	char *gname;
{
	return log_checkpoint(gname);
}

int
logflush_(gaddr)
	address **gaddr;
{
	return log_flush(*gaddr);
}

int
loghasckpt_(gname)
	char *gname;
{
	return log_has_ckpt(gname);
}

int
logrecovered_(gaddr)
	address **gaddr;
{
	return log_recovered(*gaddr);
}

int
logwrite_(gaddr, mp)
	address **gaddr;
	message **mp;
{
	return log_write(*gaddr, *mp);
}

void
msgdelete_(mp)
	message **mp;
{
	msg_delete(*mp);
}

int
msgread_(sock)
	int *sock;
{
	return (int) msg_read(*sock);
}

int
msgwrite_(sock, mp)
	int *sock;
	message **mp;
{
	return msg_write(*sock, *mp);
}

int
msgfread_(file)
	FILE *file;
{
	return (int) msg_fread(file);
}

int
msgfwrite_(file, mp)
	FILE *file;
	message **mp;
{
	return msg_fwrite(file, *mp);
}

int
msggen_(va_alist)
	va_dcl
{
	va_list ap;
	register message *mp;
	register rval;

	ISIS_ENTER();
	mp = msg_newmsg();
	va_start(ap);
	BEGINFROMFORTRAN rval = msg_doputf(mp, SYSFLD_SCAN, &ap);
	ENDFROMFORTRAN va_end(ap);

	if (rval == 0)
		ISIS_RETURN((int) mp);
	msg_delete(mp);
	ISIS_RETURN(0);
}

int
msgget_(va_alist)
	va_dcl
{
	va_list ap;
	message *mp;
	int rval;

	va_start(ap);
	BEGINFROMFORTRAN mp = VA_ARG(ap, message *);

	rval = msg_dogetf(&mp, 1, SYSFLD_SCAN, &mp->msg_fpointer, &ap);
	ENDFROMFORTRAN va_end(ap);

	return rval;
}

int
msggetfld_(va_alist)
	va_dcl
{
	int one = 1;
	va_list ap;
	message *mp;
	register fname, *fpointer, rval;

	va_start(ap);
	BEGINFROMFORTRAN mp = VA_ARG(ap, message *);
	fname = VA_ARG(ap, int);
	fpointer = VA_REF(ap, int *);

	if (fpointer && *fpointer == 0)
		fpointer = &one;
	rval = msg_dogetf(&mp, 1, fname, fpointer, &ap);
	ENDFROMFORTRAN va_end(ap);

	return rval;
}

int
msggetlen_(mp)
	message **mp;
{
	return msg_getlen(*mp);
}

int
msgisforwarded_(mp)
	message **mp;
{
	return msg_isforwarded(*mp);
}

int
msgdests_(mp)
	message **mp;
{
	return (int) msg_getdests(*mp);
}

int
msgreplyto_(mp)
	message **mp;
{
	return (int) msg_getreplyto(*mp);
}

int
msgsender_(mp)
	message **mp;
{
	return (int) msg_getsender(*mp);
}

int
msggetforwarder_(mp)
	message **mp;
{
	return (int) msg_getforwarder(*mp);
}

int
msggettype_(mp, field, inst)
	message **mp;
	int *field, *inst;
{
	return msg_gettype(*mp, *field, *inst);
}

void
msgincrefcount_(mp)
	message **mp;
{
	msg_increfcount(*mp);
}

void
msgmakelazy_(mp, howlazy)
	message **mp;
	int *howlazy;
{
	msg_makelazy(*mp, *howlazy);
}

int
msgnewmsg_()
{
	return (int) msg_newmsg();
}

int
msgput_(va_alist)
	va_dcl
{
	va_list ap;
	message *mp;
	register rval;

	va_start(ap);
	BEGINFROMFORTRAN mp = VA_ARG(ap, message *);

	rval = msg_doputf(mp, SYSFLD_SCAN, &ap);
	ENDFROMFORTRAN va_end(ap);

	return rval;
}

int
msgputfld_(va_alist)
	va_dcl
{
	va_list ap;
	message *mp;
	register rval, fname;

	va_start(ap);
	BEGINFROMFORTRAN mp = VA_ARG(ap, message *);
	fname = VA_ARG(ap, int);

	rval = msg_doputf(mp, fname, &ap);
	ENDFROMFORTRAN va_end(ap);

	return rval;
}

int
msgrewind_(mp)
	message **mp;
{
	return msg_rewind(*mp);
}

void
newsapost_(slist, subj, mp, back)
	site_id *slist;
	char *subj;
	message **mp;
	int *back;
{
	news_apost(slist, subj, *mp, *back);
}

int
newscancel_(subj)
	char *subj;
{
	return news_cancel(subj);
}

void
newsclear_(slist, subj)
	site_id slist[];
	char *subj;
{
	news_clear(slist, subj);
}

void
newsclearall_(slist, subj)
	site_id slist[];
	char *subj;
{
	news_clear_all(slist, subj);
}

int
newssubscribe_(subj, entry, back)
	char *subj;
	int *entry, *back;
{
	return news_subscribe(subj, *entry, *back);
}

int
pgclient_(gaddr, credentials)
	address **gaddr;
	char *credentials;
{
	return (int) pg_client(*gaddr, credentials);
}

void
pgclientverifier_(gname, routine)
	char *gname;
	ifunc *routine;
{
	is_ifunc(routine);
	pg_client_verifier(gname, routine);
}

int
pgdelete_(gaddr)
	address **gaddr;
{
	return pg_delete(*gaddr);
}

int
pggetlocalview_(gaddr)
	address **gaddr;
{
	return (int) pg_getlocalview(*gaddr);
}

int
pggetview_(gaddr)
	address **gaddr;
{
	return (int) pg_getview(*gaddr);
}

int
pgjoin_(va_alist)
	va_dcl
{
	address *addr, *pg_dojoin();
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN addr = pg_dojoin(&ap);
	ENDFROMFORTRAN va_end(ap);

	return (int) addr;
}

void
xferout_(va_alist)
	va_dcl
{
	int locator;
	va_list ap;
	message *mp;

	va_start(ap);
	ISIS_ENTER();
	BEGINFROMFORTRAN locator = VA_ARG(ap, int);

	mp = msg_newmsg();
	msg_doputf(mp, SYSFLD_SCAN, &ap);
	ENDFROMFORTRAN do_xfer_out(locator, mp);

	va_end(ap);
	msg_delete(mp);
	ISIS_EXIT();
}

void
xferflush_()
{
	xfer_flush();
}

void
xferrefuse_()
{
	xfer_refuse();
}

void
pgjoininhibit_(flag)
	int *flag;
{
	pg_join_inhibit(*flag);
}

void
pgleave_(gaddr)
	address **gaddr;
{
	pg_leave(*gaddr);
}

int
pglookup_(gname)
	char *gname;
{
	return (int) pg_lookup(gname);
}

int
pgmonitor_(gaddr, routine, arg)
	address **gaddr;
	vfunc *routine;
	void **arg;
{
	int rval;

	is_ifunc(routine);
	BEGINFROMFORTRAN rval = pg_monitor(*gaddr, routine, *arg);
	ENDFROMFORTRAN return rval;
}

int
pgmonitorcancel_(pwid)
	int *pwid;
{
	return pg_monitor_cancel(*pwid);
}

int
pgrank_(gaddr, who)
	address **gaddr, **who;
{
	return pg_rank(*gaddr, *who);
}

int
pgrankall_(gaddr, who)
	address **gaddr, **who;
{
	return pg_rank_all(*gaddr, *who);
}

int
pgsignal_(gaddr, signo)
	address **gaddr;
	int *signo;
{
	return pg_signal(*gaddr, *signo);
}

int
pgsubgroup_(gaddr, sgname, incarn, mlist, clist)
	address **gaddr, **mlist, **clist;
	char *sgname;
	int *incarn;
{
	address *pg_subgroup();

	return (int) pg_subgroup(*gaddr, sgname, *incarn, *mlist, *clist);
}

int
pgwatch_(gaddr, who, event, routine, arg)
	address **gaddr, **who;
	int *event;
	void **arg;
	vfunc *routine;
{
	int rval;

	is_ifunc(routine);
	BEGINFROMFORTRAN rval = pg_watch(*gaddr, *who, *event, routine, *arg);
	ENDFROMFORTRAN return rval;
}

int
pgwatchcancel_(wid)
	int *wid;
{
	return pg_watch_cancel(*wid);
}

int
procwatch_(paddr, routine, arg)
	address **paddr;
	vfunc *routine;
	void **arg;
{
	int rval;

	is_ifunc(routine);
	BEGINFROMFORTRAN rval = proc_watch(*paddr, routine, *arg);
	ENDFROMFORTRAN return rval;
}

int
procwatchcancel_(wid)
	int *wid;
{
	return proc_watch_cancel(*wid);
}

void
replyl_(va_alist)
	va_dcl
{
	va_list ap;

	va_start(ap);
	BEGINFROMFORTRAN do_reply_l(&ap);
	ENDFROMFORTRAN va_end(ap);
}

int
rmgrcreate_(rmi)
	rmgr_info *rmi;
{
	return (int) rmgr_create(rmi);
}

int
rmgrgetinfo_(pgname, noblock)
	char *pgname;
	int *noblock;
{
	return (int) rmgr_getinfo(pgname, *noblock);
}

int
rmgrjoin_(rmi)
	rmgr_info *rmi;
{
	return (int) rmgr_join(rmi);
}

int
rmgrlasttofail_(gname, key, oldincarnp, noblock)
	char *gname, *key;
	int *oldincarnp, *noblock;
{
	return rmgr_lasttofail(gname, key, oldincarnp, *noblock);
}

int
rmgrregister_(key)
	char *key;
{
	return rmgr_register(key);
}

int
rmgrrestart_(pgname)
	char *pgname;
{
	return (int) rmgr_restart(pgname);
}

int
rmgrstartlog_(gad, key)
	address **gad;
	char *key;
{
	return rmgr_start_log(*gad, key);
}

int
rmgrstoplog_(gad, key)
	address **gad;
	char *key;
{
	return rmgr_stop_log(*gad, key);
}

int
rmgrunregister_()
{
	return rmgr_unregister();
}

int
rmgrupdate_(key, program, argv, envp)
	char *key, *program, **argv, **envp;
{
	return rmgr_update(key, program, argv, envp);
}

int
sitegetview_()
{
	return (int) site_getview();
}

int
svviewid_(sv)
	sview **sv;
{
	return (*sv)->sv_viewid;
}

int
svslist_(sv, i)
	sview **sv;
	int *i;
{
	return (*sv)->sv_slist[*i];
}

int
svincarn_(sv, i)
	sview **sv;
	int *i;
{
	return (int) (*sv)->sv_incarn[*i];
}

int
svfailed_(sv)
	sview **sv;
{
	return (int) &(*sv)->sv_failed;
}

int
svrecovered_(sv)
	sview **sv;
{
	return (int) &(*sv)->sv_recovered;
}

int
svmonitor_(routine, arg)
	vfunc *routine;
	void **arg;
{
	int rval;

	is_ifunc(routine);
	BEGINFROMFORTRAN rval = sv_monitor(routine, *arg);
	ENDFROMFORTRAN return rval;
}

int
svmonitorcancel_(svid)
	int *svid;
{
	return sv_monitor_cancel(*svid);
}

int
svwatch_(sid, event, routine, arg)
	site_id *sid;
	vfunc *routine;
	int *event;
	void **arg;
{
	int rval;

	is_ifunc(routine);
	BEGINFROMFORTRAN rval = sv_watch(*sid, *event, routine, *arg);
	ENDFROMFORTRAN return rval;
}

int
svwatchcancel_(svid)
	int *svid;
{
	return sv_watch_cancel(*svid);
}

int
tholder_(gaddr, name)
	address **gaddr;
	char *name;
{
	address *t_holder();

	return (int) t_holder(*gaddr, name);
}

int
tonsysstack_(routine, arg)
	ifunc *routine;
	char **arg;
{
	is_ifunc(routine);
	return t_on_sys_stack(routine, *arg);
}

int
tpass_(gaddr, name)
	address **gaddr;
	char *name;
{
	return t_pass(*gaddr, name);
}

int
trequest_(gaddr, name, passonfail)
	address **gaddr;
	char *name;
	int *passonfail;
{
	return t_request(*gaddr, name, *passonfail);
}

void
tscheck_()
{
	t_scheck();
}

void
tfork_(routine, arg)
	vfunc *routine;
	char **arg;
{
	BEGINFROMFORTRAN;
	t_fork(routine, arg ? *arg : (char *) 0);
	ENDFROMFORTRAN;
}

void
tforkurgent_(routine, arg)
	vfunc *routine;
	char **arg;
{
	BEGINFROMFORTRAN;
	t_fork_urgent(routine, arg ? *arg : 0);
	ENDFROMFORTRAN;
}

void
tsig_(cp, rval)
	condition *cp;
	void **rval;
{
	t_sig(cp, *rval);
}

void
tsigall_(cp, rval)
	condition *cp;
	void **rval;
{
	t_sig_all(cp, *rval);
}

void
tsigurgent_(cp, rval)
	condition *cp;
	void **rval;
{
	t_sig_urgent(cp, *rval);
}

void *
tyield_()
{
	return t_yield();
}

void *
twait_(cp)
	condition *cp;
{
	return t_wait(cp);
}

void *
twaitl_(cp, why)
	condition *cp;
	char *why;
{
	return t_wait_l(cp, why);
}

int
xabort_()
{
	return x_abort();
}

int
xbegin_()
{
	return x_begin();
}

int
xcommit_(phases)
	int *phases;
{
	return x_commit(*phases);
}

x_id *
xgetid_()
{
	x_id *x_getid();

	return x_getid();
}

int
xidcmp_(id1, id2)
	x_id *id1, *id2;
{
	return xid_cmp(id1, id2);
}

void
xoutcomes_(xlist, partname)
	x_list *xlist;
	char *partname;
{
	x_list *x_outcomes(), *xlp;

	xlp = x_outcomes(partname);
	bcopy(xlp, xlist, sizeof(x_list) + (sizeof(x_item)) * (xlp->len - 1));
}

void
xoutcomesflush_(partname, outcomes)
	char *partname;
	x_list *outcomes;
{
	x_outcomes_flush(partname, outcomes);
}

int
xterm_(va_alist)
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
	BEGINFROMFORTRAN participant_name = VA_REF(vargs, char *);

	on_prepare = VA_REF(vargs, bfunc *);
	on_commit = VA_REF(vargs, bfunc *);
	on_abort = VA_REF(vargs, bfunc *);
	rval = msg_doputf(data, SYSFLD_SCAN, &vargs);
	ENDFROMFORTRAN va_end(vargs);

	if (rval) {
		msg_delete(data);
		return (-1);
	}
	return x_term_msg(participant_name, on_prepare, on_commit, on_abort, data);
}

int
mysno_()
{
	return my_site_no;
}

int
mysincarn_()
{
	return my_site_incarn;
}

int
mysid_()
{
	return my_site_id;
}

int
mypid_()
{
	return my_process_id;
}

int
myaddress_()
{
	return (int) &my_address;
}

void
myhost_(where)
	char *where;
{
	strcpy(where, my_host);
}

void
sitenames_(where, i)
	char *where;
	int *i;
{
	strcpy(where, site_names[*i]);
}

int
isiserrno_()
{
	return isis_errno;
}

void
isisdir_(where)
	char *where;
{
	strcpy(where, isis_dir);
}

int
isisnsent_()
{
	return isis_nsent;
}

int
isisnreplies_()
{
	return isis_nreplies;
}

int
isissocket_()
{
	return isis_socket;
}

int
isisstate_()
{
	return isis_state;
}

int
gvviewid_(gv)
	register groupview **gv;
{
	return (*gv)->gv_viewid;
}

int
gvincarn_(gv)
	register groupview **gv;
{
	return (*gv)->gv_incarn;
}

int
gvflag_(gv)
	register groupview **gv;
{
	return (*gv)->gv_flag;
}

int
gvgaddr_(gv)
	register groupview **gv;
{
	return (int) &(*gv)->gv_gaddr;
}

void
gvname_(str, gv)
	register groupview **gv;
	register char *str;
{
	strcpy(str, (*gv)->gv_name);
}

int
gvnmemb_(gv)
	register groupview **gv;
{
	return (*gv)->gv_nmemb;
}

int
gvnclient_(gv)
	register groupview **gv;
{
	return (*gv)->gv_nclient;
}

int
gvmember_(gv, n)
	register int *n;
	register groupview **gv;
{
	return (int) &(*gv)->gv_members[*n];
}

int
gvclient_(gv, n)
	register int *n;
	register groupview **gv;
{
	return (int) &(*gv)->gv_clients[*n];
}

int
gvjoined_(gv)
	register groupview **gv;
{
	return (int) &(*gv)->gv_joined;
}

int
gvdeparted_(gv)
	register groupview **gv;
{
	return (int) &(*gv)->gv_departed;
}

void
spoolreplay_(va_alist)
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
spooldiscard_(va_alist)
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
spoolanddiscard_(va_alist)
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
spoolinquire_(sname, sid)
	char *sname;
	int sid;
{
	isis_errno = IE_NOTIMP;
	return (-1);
}

int
spoolcancel_(sname, sid)
	char *sname;
	int sid;
{
	isis_errno = IE_NOTIMP;
	return (-1);
}

int
spoolwait_(sname, sid)
	char *sname;
	int sid;
{
	isis_errno = IE_NOTIMP;
	return (-1);
}

void
spoolplaythrough_(sname, on_off)
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
spoolsetreplaypointer_(sname, spseqn)
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
spoolsetckptpointer_(sname, spseqn)
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
