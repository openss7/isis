/*  $RCSfile: cl_typedefs.h,v $ $Revision: 2.68 $ $Date: 90/08/09 13:45:20 $  */
/*
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

#ifndef CL_TYPEDEFS
#define CL_TYPEDEFS

/* Define ISIS version here since this file is included by clib and protos. */
#define ISIS_VERSION    "2.1"		/* 3 char limit */
#define FULL_VERSION    "ISIS release V2.1, Aug 1990"

#if (  __cplusplus || c_plusplus )
#define VOID void
typedef  void    vfunc();
struct qnode;
struct task;
struct sys_groupview;
struct verify;
struct sview;
struct etree;
struct gnode;
struct event_id;
struct groupview;
struct ginfo;
struct interclient;
struct mdesc;
struct fraghdr;
struct ioq;
struct iclpkt;
struct iovec;
struct bitvec;
struct gl_desc;
struct intersite;
struct pwatch;
struct wnode;
struct svmon;
struct bc_args;
struct cl_watch;
struct x_part;
struct x_info;
struct token;
struct bc_node;
#else
#if     (MIPS|VAX|AIX|GOULD|RT43)
#  define   VOID    char /* Many compilers crash on void* args and assigns! */
#else
#  define   VOID    void
#endif
typedef  VOID    vfunc();
typedef struct qnode qnode;
typedef struct task task;
typedef struct sys_groupview sys_groupview;
typedef struct verify verify;
typedef struct sview sview;
typedef struct etree etree;
typedef struct gnode gnode;
typedef struct event_id event_id;
typedef struct groupview groupview;
typedef struct ginfo ginfo;
typedef struct interclient interclient;
typedef struct mdesc mdesc;
typedef struct fraghdr fraghdr;
typedef struct ioq ioq;
typedef struct iclpkt iclpkt;
typedef struct iovec iovec;
typedef struct bitvec bitvec;
typedef struct gl_desc gl_desc;
typedef struct bc_node bc_node;
typedef struct intersite intersite;
typedef struct pwatch pwatch;
typedef struct wnode wnode;
typedef struct cl_watch cl_watch;
typedef struct svmon svmon;
typedef struct bc_args bc_args;
typedef struct x_part x_part;
typedef struct x_info x_info;
typedef struct token token;
#endif
typedef struct sockaddr_in saddr;
typedef struct qnode *condition;
typedef union address x_id;
typedef int    ifunc();
typedef short  site_id;

typedef unsigned int bool;

#ifndef FALSE
# define FALSE 0
#endif
#ifndef TRUE
# define TRUE 1
#endif

#endif

