/*  $RCSfile: pr_global.h,v $ $Revision: 2.6 $ $Date: 90/07/31 14:00:02 $  */
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
#ifndef PR_GLOBAL
#define PR_GLOBAL

extern  errno;                          /* Errno defined in C */
int     NSITES;                         /* Generate stats output for sites 1..NSITES */
int     my_site_no;                     /* Info about this site */
int     my_site_incarn;                 /* Info about this site */
site_id my_site_id;                     /* This site's id */
int     my_genid;                       /* Always 0 in this version of protos */
int     my_process_id;                  /* Info about this site */
address my_address;                     /* Info about this site */
address my_startup_addr;                /* my_address with RESTART in incarn */
int     my_entry;                       /* Current entry number */
char    my_host[64];                    /* My host name */
char    *isis_dir;                      /* ISIS directory for created files/logs */
int     udp_port[MAX_SITES];            /* Use this if specified */
int     connect_port[MAX_SITES];        /* Use this if specified */
char    site_names[MAX_SITES][64];      /* All known host names */
int     CONJURE;                        /* For magic numbers */
qnode   *as_root;                       /* Associative store */
qnode   *pg_root;                       /* Process group views */
qnode   *time_queue;                    /* Timeout qnode */
qnode   *watching;                      /* Map of wid->watch node */
int     connect_bit;                    /* connect request */
int     net_socket;                     /* For input from the inter-site */
bitvec  input_mask;                     /* all input channels */
bitvec  congested;                      /* Congested channels */
int     ntasks;                         /* Number of active tasks */
int     max_cl;                         /* Maximum open fdes */
int     intersite_congested;            /* Count intersite bytes to detect congestion */
int     as_ndelete, as_nlocdelete;
bitvec  ones;                           /* All ones */
int     pr_harakari;
int     fail_msgno;
int     fail_sender;

address ADDRESS();

#endif
