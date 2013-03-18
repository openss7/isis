/*  $RCSfile: cl_token.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:40 $  */
/*
 *      Based on algorithm by Frank Schmuck
 *	Originally coded by Ken Birman
 *      ISIS distributed systems toolkit: token tool
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

struct token {
	address t_gaddr;
	char t_name[32];
	qnode *t_req;
	int t_flag;
	int t_watch;
	address t_holder;
	token *t_next;
};

#define T_INUSE         0x0001	/* Token is in use here */
#define T_HOLD          0x0002	/* Token is still at this process */
#define T_FAIL          0x0004	/* Current holder failed */
#define T_PASS_ON_FAIL  0x0008	/* Pass if current holder fails */
#define T_SENDING       0x0010	/* Sending this token */

static adesc t_ad = { sizeof(token), sizeof(token), 4 };

#define t_alloc()       ((token*)mallocate(&t_ad))
#define t_free(tp)      mdeallocate((char*)tp,&t_ad)

static token *t_root;
static void t_failed(), token_req(), tk_send(), tk_rcv(), token_pass();
static int send_token();
static void t_newholder();

void
tk_init()
{
	isis_entry(GENERIC_TOKEN_REQ, (vfunc *) token_req, "isis-tokens:token_req");
	isis_entry(GENERIC_TOKEN_PASS, (vfunc *) token_pass, "isis-tokens:token_pass");
	allow_xfers_xd(NULLARG, XD_TOKEN, tk_send, tk_rcv);
	isis_task(t_failed, "isis-tokens:t_failed");
	isis_task(tk_send, "isis-tokens:tk_send");
	isis_task(tk_rcv, "isis-tokens:tk_rcv");
}

token *
map_token(gaddr, name)
	address *gaddr;
	char *name;
{
	register token *tp;
	register groupview *gv;

	tp = t_root;
	while (tp)
		if (addr_isequal(gaddr, &tp->t_gaddr) && strcmp(name, tp->t_name) == 0)
			return (tp);
		else
			tp = tp->t_next;
	gv = pg_getlocalview(gaddr);
	if (gv == 0)
		return ((token *) 0);
	tp = t_alloc();
	tp->t_gaddr = *gaddr;
	tp->t_req = qu_null();
	tp->t_next = t_root;
	t_root = tp;
	strcpy(tp->t_name, name);
	t_newholder(tp, &gv->gv_members[0], -1);
	return (tp);
}

static void
tk_send(locator, gaddr)
	int locator;
	address *gaddr;
{
	register token *tp;
	register n = 0;

	for (tp = t_root; tp; tp = tp->t_next)
		if (addr_isequal(&tp->t_gaddr, gaddr)) {
			register pass_on_fail = 0;

			if (tp->t_flag & T_PASS_ON_FAIL)
				++pass_on_fail;
			xfer_out(n++, "%A[1].%s: %A[1],%d", &tp->t_gaddr, tp->t_name, &tp->t_holder,
				 pass_on_fail);
		}
}

static void
tk_rcv(locator, mp)
	int locator;
	message *mp;
{
	register token *tp;
	address gaddr, holder;
	char *name;
	int pass_on_fail;

	msg_get(mp, "%a.%-s: %a,%d", &gaddr, &name, &holder, &pass_on_fail);
	tp = t_root;
	while (tp)
		if (addr_isequal(&gaddr, &tp->t_gaddr) && strcmp(name, tp->t_name) == 0)
			return;
		else
			tp = tp->t_next;
	tp = t_alloc();
	tp->t_next = t_root;
	t_root = tp;
	tp->t_req = qu_null();
	tp->t_gaddr = gaddr;
	strcpy(tp->t_name, name);
	t_newholder(tp, &holder, pass_on_fail);
}

void
dump_tokens()
{
	register token *tp = t_root;

	while (tp) {
		register qnode *qp;

		print("Token ");
		paddr(&tp->t_gaddr);
		print("..%s held by ", tp->t_name);
		paddr(&tp->t_holder);
		if (tp->t_flag & T_FAIL)
			print(" (failed)");
		if (tp->t_flag) {
			print(" <");
			if (tp->t_flag & T_INUSE)
				print(" inuse");
			if (tp->t_flag & T_HOLD)
				print(" local");
			if (tp->t_flag & T_PASS_ON_FAIL)
				print(" pass-on-fail");
			if (tp->t_flag & T_SENDING)
				print(" sending");
			print(" >");
		}
		print("\n");
		if (tp->t_req->qu_next != tp->t_req)
			print("  ... pending requests: ");
		for (qp = tp->t_req->qu_next; qp != tp->t_req; qp = qp->qu_next) {
			register message *mp = qp->qu_msg;

			paddr(msg_getsender(mp));
			print("[%d] ", msg_getid(mp));
		}
		print("\n");
		tp = tp->t_next;
	}
}

int
t_request(gaddr, name, pass_on_fail)
	address *gaddr;
	char *name;
	int pass_on_fail;
{
	register token *tp;
	int passed;

	ISIS_ENTER();
	tp = map_token(gaddr, name);
	if (tp == (token *) 0) {
		isis_errno = IE_RESTRICTED;
		ISIS_RETURN(-1);
	}
	if ((tp->t_flag & (T_HOLD | T_INUSE)) == T_HOLD) {
		tp->t_flag |= T_INUSE;
		ISIS_RETURN(1);
	}
	if (cbcast(gaddr, GENERIC_TOKEN_REQ, "%A[1],%s,%d", gaddr, name, pass_on_fail,
		   1, "%d", &passed) != 1) {
		isis_errno = IE_RESTRICTED;
		ISIS_RETURN(-1);
	}
	ISIS_RETURN(passed);
}

static void
token_req(mp)
	message *mp;
{
	address gaddr;
	char *name;
	register token *tp;

	if (msg_get(mp, "%a,%-s", &gaddr, &name) != 2)
		panic("token_req");
	tp = map_token(&gaddr, name);
	if (tp == (token *) 0)
		panic("token_req");
	msg_increfcount(mp);
	qu_add_mp(tp->t_req, (int) name, mp, NULLROUTINE);
	if ((tp->t_flag & (T_INUSE | T_HOLD)) == T_HOLD)
		(void) send_token(tp);
}

int
t_pass(gaddr, name)
	address *gaddr;
	char *name;
{
	register token *tp;

	ISIS_ENTER();
	tp = map_token(gaddr, name);
	if (tp == (token *) 0)
		panic("t_pass");
	if ((tp->t_flag & (T_INUSE | T_FAIL)) == 0) {
		isis_errno = IE_NOTALLOWED;
		ISIS_RETURN(-1);
	}
	tp->t_flag &= ~T_INUSE;
	ISIS_RETURN(send_token(tp));
}

static int
send_token(tp)
	register token *tp;
{
	int passed = 0;
	register qnode *qp;

	/* Loop, trying to pass token until qnode empty or success */
	if (tp->t_flag & T_SENDING)
		return (passed);
	tp->t_flag |= T_SENDING;
	if ((tp->t_flag & T_HOLD) == 0)
		panic("send_token: not holder");
	tp->t_flag &= ~T_HOLD;
	while (!passed && (qp = qu_head(tp->t_req))) {
		register message *mp = qp->qu_msg;
		int id, pass_on_fail;

		qu_free(qp);
		id = msg_getid(mp);
		if (msg_get(mp, "%d", &pass_on_fail) != 1)
			panic("token_send: msg_get failed, fpointer %d", mp->msg_fpointer);
		cbcast(&tp->t_gaddr, GENERIC_TOKEN_PASS,
		       "%A[1],%s,%A[1],%d,%d", &tp->t_gaddr, tp->t_name, msg_getsender(mp), id,
		       pass_on_fail, 1, "%d", &passed);
		reply(mp, "%d", passed);
		msg_delete(mp);
	}
	if (passed == 0)
		tp->t_flag |= T_HOLD;
	tp->t_flag &= ~T_SENDING;
	return (passed);
}

static void
token_pass(mp)
	message *mp;
{
	address new_holder, gaddr;
	register address *sender;
	register passed = 1;
	register token *tp;
	int his_msgid, pass_on_fail;
	char *name;

	if (msg_get(mp, "%a,%-s,%a,%d,%d", &gaddr, &name, &new_holder, &his_msgid, &pass_on_fail) !=
	    5)
		panic("token_pass had problems with msg_get");
	tp = map_token(&gaddr, name);
	if (tp == (token *) 0)
		panic("token_pass: group/token-name unknown");
	if (pg_rank(&gaddr, &new_holder) != -1)
		t_newholder(tp, &new_holder, pass_on_fail);
	else
		passed = 0;
	sender = msg_getsender(mp);
	if (addr_ismine(sender))
		reply(mp, "%d", passed);
	else {
		register qnode *qp;

		for (qp = tp->t_req->qu_next; qp != tp->t_req; qp = qp->qu_next) {
			register message *msg = qp->qu_msg;

			sender = msg_getsender(msg);
			if (msg_getid(msg) == his_msgid && addr_isequal(&new_holder, sender)) {
				msg_delete(msg);
				qu_free(qp);
				break;
			}
		}
	}
}

address *
t_holder(gaddr, name)
	address *gaddr;
	char *name;
{
	register token *tp;

	ISIS_ENTER();
	tp = map_token(gaddr, name);
	if (tp == (token *) 0)
		ISIS_RETURN(&NULLADDRESS);
	ISIS_RETURN(&tp->t_holder);
}

static void
ignore(mp)
	message *mp;
{
}

static void
do_send_token(msg, gaddr, how, tp)
	message *msg;
	address *gaddr;
	int how;
	register char *tp;
{
	send_token(tp);
	cc_terminate("");
}

static void
t_failed(tp)
	register token *tp;
{
	if (tp->t_flag & T_PASS_ON_FAIL)
		coord_cohort((message *) 0, &(tp->t_gaddr), do_send_token, ignore, tp);
	else
		tp->t_flag |= T_FAIL;
}

static void
t_newholder(tp, whop, pass_on_fail)
	register token *tp;
	address *whop;
	int pass_on_fail;
{
	address who;

	who = *whop;
	who.addr_entry = 0;
	tp->t_holder = who;
	tp->t_flag = 0;
	if (addr_ismine(&who))
		if (pass_on_fail != -1)
			tp->t_flag |= T_HOLD | T_INUSE;
		else
			tp->t_flag |= T_HOLD;
	if (pass_on_fail)
		tp->t_flag |= T_PASS_ON_FAIL;
	if (tp->t_watch)
		pg_watch_cancel(tp->t_watch);
	if (pass_on_fail)
		tp->t_watch = pg_watch(&tp->t_gaddr, &tp->t_holder, W_FAIL, t_failed, tp);
	else
		tp->t_watch = 0;
}
