/*  $RCSfile: pr_pgroups.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:02 $  */
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
 *      sys_groupview data structure
 */

#define PG_GLEN    128             /* Maximum chars in a sys_groupview name */
#define PG_ALEN    32              /* Maximum number of members and clients */

struct sys_groupview
{
        /* Transmitted part */
        int             pg_viewid;         /* View number */
        int             pg_incarn;         /* Incarnation number */
        address         pg_gid;            /* Group name */
        char            pg_name[PG_GLEN];  /* Group name, if any */
        short           pg_nmemb;          /* Number of members */
        short           pg_nclient;        /* Number of clients */
        address         pg_alist[PG_ALEN]; /* Group members, null terminated */

        /* Not transmitted */
        int             pg_ccount;         /* Counts iterated protocols underway */
        int             pg_flag;           /* Flags */
        condition       pg_newview;        /* Waiting for view to change */
        condition       pg_bwait;          /* Waiting for iteration to end */
        condition       pg_rwait;          /* Waiting before doing refresh */
        condition       pg_hipri;          /* Waiting in iterated addressing */
        condition       pg_lopri;          /* Waiting in iterated addressing */
};

/* Flag values */
#define PG_REFRESH      0x01    /* Set if view is being refreshed */
#define PG_CACHED       0x02    /* Set if view is a cached one */
#define PG_FAILED       0x04    /* Set if process owning this entry has failed */
#define PG_DELSENT      0x08    /* Set if pg_failed already did a delete */
#define PG_VALID        0x10    /* cl_isis.c: view is valid  */
#define PG_INVALID      0x20    /* cl_isis.c: view has been superceeded */
#define PG_FIRST        0x40    /* cl_isis.c: view is first for group (just joined) */
#define PG_STALE        0x80    /* cl_isis.c: vew no longer current */

#define nullpg          ((sys_groupview*)0)
#define pglength(pg)    ((int)&nullpg->pg_alist[(pg)->pg_nmemb+(pg)->pg_nclient+3])

/* The cmd "list" command returns an array of these */
struct gl_desc
{
        char            gl_name[PG_GLEN];
        int             gl_viewid;
        int             gl_nmembers;
        int             gl_nclients;
        bitvec       gl_sites;
        address       gl_addr;
};

/* Used in gbcasts to verify deliverability */
struct verify
{
        address       vi_gid;
        int             vi_viewid;
        bitvec       vi_sites;
};
