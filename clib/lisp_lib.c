/*  $RCSfile: lisp_lib.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:32 $  */
/*
 * Extra ISIS routines needed for lisp. Mostly these are macros in C.
 *  Coded by Robert Cooper
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

address *
msg_getdests_fun(mp)
	message *mp;
{
	return (msg_getdests(mp));
}

void
msg_increfcount_fun(mp)
	message *mp;
{
	msg_increfcount(mp);
}

address *
msg_getsender_fun(mp)
	message *mp;
{
	return (msg_getsender(mp));
}

int
msg_getlen_fun(mp)
	message *mp;
{
	return ((mp)->msg_len);
}

address *
msg_getreplyto_fun(mp)
	message *mp;
{
	return (&(mp)->msg_hdr->hdr_replyto);
}

address *
msg_gettruesender_fun(mp)
	message *mp;
{
	return (&(mp)->msg_hdr->hdr_sender);
}

bool
msg_isforwarded_fun(mp)
	message *mp;
{
	return (!addr_isnull((mp)->msg_forwarder));
}

int
isis_input_fun(fd, func, arg)
	int fd;
	vfunc *func;
	VOID *arg;
{
	return (isis_input(fd, func, arg));
}

int
isis_output_fun(fd, func, arg)
	int fd;
	vfunc *func;
	VOID *arg;
{
	return (isis_output(fd, func, arg));
}

int
isis_except_fun(fd, func, arg)
	int fd;
	vfunc *func;
	VOID *arg;
{
	return (isis_except(fd, func, arg));
}

int
isis_signal_fun(sg, func, arg)
	int sg;
	vfunc *func;
	VOID *arg;
{
	return (isis_signal(sg, func, arg));
}

int
isis_chwait_fun(func, arg)
	vfunc *func;
	VOID *arg;
{
	return (isis_chwait(func, arg));
}

int
isis_input_sig_fun(fd, cond, arg)
	int fd;
	condition *cond;
	VOID *arg;
{
	return (isis_input_sig(fd, cond, arg));
}

int
isis_output_sig_fun(fd, cond, arg)
	int fd;
	condition *cond;
	VOID *arg;
{
	return (isis_output_sig(fd, cond, arg));
}

int
isis_signal_sig_fun(sg, cond, arg)
	int sg;
	condition *cond;
	VOID *arg;
{
	return (isis_signal_sig(sg, cond, arg));
}

int
isis_chwait_sig_fun(cond, arg)
	condition *cond;
	VOID *arg;
{
	return (isis_chwait_sig(cond, arg));
}

bool
addr_isnull_fun(a)
	address *a;
{
	return (addr_isnull(a));
}

int
addr_cmp_fun(a1, a2)
	address *a1, *a2;
{
	return (addr_cmp(a1, a2));
}

condition *
make_condition()
{
	condition *cond;

	cond = (condition *) malloc(sizeof(condition));
	*cond = 0;
	return (cond);
}

char *
get_my_host()
{
	return (my_host);
}

char *
get_site_name(i)
	int i;
{
	return (site_names[i]);
}

int
get_sv_viewid(sv)
	sview *sv;
{
	return (sv->sv_viewid);
}

site_id *
get_sv_slist(sv)
	sview *sv;
{
	return (sv->sv_slist);
}

u_char *
get_sv_incarn(sv)
	sview *sv;
{
	return (sv->sv_incarn);
}

bitvec *
get_sv_failed(sv)
	sview *sv;
{
	return &(sv->sv_failed);
}

bitvec *
get_sv_recovered(sv)
	sview *sv;
{
	return &(sv->sv_recovered);
}

int
get_gv_viewid(gv)
	register groupview *gv;
{
	return (gv)->gv_viewid;
}

int
get_gv_incarn(gv)
	register groupview *gv;
{
	return (gv)->gv_incarn;
}

int
get_gv_flag(gv)
	register groupview *gv;
{
	return (gv)->gv_flag;
}

address *
get_gv_gaddr(gv)
	register groupview *gv;
{
	return &(gv->gv_gaddr);
}

char *
get_gv_name(gv)
	register groupview *gv;
{
	return gv->gv_name;
}

short
get_gv_nmemb(gv)
	register groupview *gv;
{
	return (gv)->gv_nmemb;
}

short
get_gv_nclient(gv)
	register groupview *gv;
{
	return (gv)->gv_nclient;
}

address *
get_gv_joined(gv)
	register groupview *gv;
{
	return &(gv->gv_joined);
}

address *
get_gv_departed(gv)
	register groupview *gv;
{
	return &(gv->gv_departed);
}

address *
get_gv_members(gv)
	register groupview *gv;
{
	return (gv->gv_members);
}

address *
get_gv_clients(gv)
	register groupview *gv;
{
	return (gv->gv_clients);
}

address *
get_x_list_items_id(outcomes, i)
	x_list *outcomes;
	int i;
{
	return (&(outcomes->items[i].id));
}
