/*  $RCSfile: pr_client.h,v $ $Revision: 2.3 $ $Date: 90/06/24 15:15:41 $  */
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
 *
 *
 */
/* Client info data structure */

#include "pr_fdect.h"
#include "pr_address.h"
#include "pr_msgfields.h"

#ifndef CINFO
#define CINFO

#if ! ( __cplusplus || c_plusplus )
typedef struct cinfo cinfo;
#endif

struct cinfo {
	int ci_my_site_no;
	int ci_my_site_incarn;
	int ci_my_site_id;
	int ci_genid;
	char ci_isisdir[128];
	int ci_portno;
};

#define SIG_OVERFLOW    SIGUSR1
#define SIG_DUMP        SIGUSR2

/* Codes used in new_gview */
#define PG_GROW         1
#define PG_LEAVE        2
#define PG_MIGRATE      3

/* Up to 128 local ``id generators'' */
#define GEN_MSGID(n)        (((pr_lastmsg++ & 0xffff) << 15) | (n << 8) | (my_site_no & 0xff))

/* my_genid is always 0 in protos, non-zero in local clients */
#define GENMSGID            GEN_MSGID(my_genid)

#define LOCALFLAG           0x80000000
#define PID_REMOTE          0x00008000

#endif
