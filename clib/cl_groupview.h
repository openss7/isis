/*  $RCSfile: cl_groupview.h,v $ $Revision: 2.30 $ $Date: 90/07/31 10:41:35 $  */
/*
 *	Originally coded by Ken Birman
 *      Groupview as presented to clients
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

#ifndef GROUPVIEWS

#define GROUPVIEWS

struct groupview
{
    int         gv_viewid;             /* View number */
    int         gv_incarn;             /* Incarnation number */
    int         gv_flag;               /* Indicates if this is cached or not */
    address     gv_gaddr;              /* Group address */
    char        gv_name[PG_GLEN];      /* Group name */
    short       gv_nmemb;              /* Number of members */
    short       gv_nclient;            /* Number of clients */
    address     gv_members[PG_ALEN];   /* List of members */
    address     gv_clients[PG_ALEN];   /* List of clients */
    address     gv_joined;             /* New member, if any */
    address     gv_departed;           /* Departed member, if any */
    int         gv_refcount;           /* Reference count */
};

/*** Interface routines  defined in cl_groups.c, cl_pgroup.c, cl_watch.c and cl_join.c ***/
#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
int	pg_addclient	(address *gaddr, address *pname);
int	pg_addmemb  	(address *gaddr, address *pname);
int	pg_delclient	(address *gaddr, address *pname);
int	pg_delete	(address *gaddr);
int	pg_leave	(address *gaddr);
gl_desc *pg_list	(char *gname);
address	*pg_lookup	(char *name);
groupview *pg_getlocalview(address *gaddr);
groupview *pg_getview	(address *gaddrp);
address *pg_local_lookup(char *gname);
int	pg_monitor	(address *gaddr,
                         void (*routine)(groupview *gv, VOID *argp), VOID *arg);
int	pg_monitor_cancel(int pwid);
int	pg_signal	(address *gaddr, int signo);
void	pg_unmonitor	(address *gaddr);
void	allow_xfers_xd	(char *gname, int xd,
                         void (*send_routine)(int, address *),
                         void (*rcv_routine)(int, message *));
int	pg_client	(address *gaddr, char *credentials);
void	pg_client_verifier(char *gname, int (*routine)(char *credentials));
void	pg_join_inhibit	(int flag);
void	pg_join_verifier(char *gname, int (*routine)(char *credentials));
address	*pg_subgroup	(address *gaddr, char *sgname, int incarn, address *mlist, address *clist);
address	*pg_dup     	(address *gaddr, char *sgname);
int	xfer_flush	();
void	xfer_refuse	();
int	pg_detect_failure(address *gaddrp,
                          void (*routine)(address *gaddr, int what, VOID *argp),
                          VOID *arg);
int	pg_watch	(address *gaddrp, address *whop, int event,
                         void (*routine)(address *gaddr, address *paddr,
                                         int what, VOID *argp),
                         VOID *arg);
int	pg_watch_cancel	(int wid);
int	proc_watch	(address *paddrp,
                         void (*routine)(address *paddr, int what, VOID *argp),
                         VOID *arg);
int	proc_watch_cancel(int wid);
#if ( __cplusplus || c_plusplus )
address	*pg_join	(char *gname ...);
void	xfer_out	(int locator, char *fmt ...);
#else
address	*pg_join	();
void	xfer_out	( /* int locator, char *fmt, ... */ );
#endif

#ifdef __cplusplus
		       }
#endif
#else FUN_TYPES

address   *pg_join();		/* Join */
address	  *pg_subgroup();	/* Quick subgroup create */
address	  *pg_dup();		/* Dup */
groupview *pg_getlocalview();   /* Gets a view that should already be known */
groupview *pg_getview	();     /* Gets a view */
address *pg_local_lookup();
address *pg_lookup();
void allow_xfers_xd(), pg_join_verifier(), pg_client_verifier();
#endif
/*** Internal routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
int	pg_monitor_act	(address *gaddr,
                         void (*routine)(groupview *gv, VOID *argp), VOID *arg);
void	pw_free		(pwatch *pw);
ginfo	*add_gname	(char *gname);
ginfo	*add_group	(char *gname, address *gaddr);
void	gi_free		(ginfo *gip);
void	group_unmap	(ginfo *gip);
groupview *isis_gv_alloc	();
void	isis_gv_free	(groupview *gv);
ginfo	*map_gaddr	(address *gaddrp);
ginfo	*map_gname	(char *gname);
void	do_xfer_out	(int locator, struct message *mp);
void	join_block	();
void	join_init	();
address	*pg_dojoin	(va_list *ap);
void	xfer_rcv_small	(message *mp);
int	xfer_rcv_unpack	(ginfo *gip, message *mp);
void	xfer_to_checkpoint(address *gaddr);
void	pg_monitor_dump	();
void	pg_new_view	(groupview *gv);
void	pg_pwatch_invoke(pwatch *pw);
int	cl_pmonitor	(address *addr);
void	do_cl_proc_failed(address *ap);
void	pg_detected_failure(address *gaddrp);
void	proc_monitor_dump();
void	w_free		(wnode *wp);
void	w_init		();
void	w_new_view	(groupview *gv);
#ifdef __cplusplus
}
#endif
#else
void 	pg_pwatch_invoke(), isis_gv_free(), pg_detected_failure();
void	join_block(), xfer_rcv_small(), do_cl_proc_failed();
void    do_xfer_out();
#endif

#endif  GROUPVIEWS
