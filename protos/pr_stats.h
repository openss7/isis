/*  $RCSfile: pr_stats.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:13 $  */
/*
 * Statistics data structure 
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

#define S_CBSTART       0       /* cbcast started */
#define S_CBDONE        1       /* cbcast done */
#define S_ABSTART       2       /* abcast started */
#define S_ABDONE        3       /* abcast done */
#define S_GBSTART       4       /* gbcast started */
#define S_GBDONE        5       /* gbcast done */
#define S_CBSENT        6       /* cbcast inter-site packets sent */
#define S_CBCOUNT       7       /* cbcast messages they contained */
#define S_CBDELIV       8       /* cbcast messages delivered */
#define S_CBDUP         9       /* cbcast messages that were dups */
#define S_GBABORTS      10      /* gbcast aborts */
#define S_ASROUNDS      11      /* Astore rounds */
#define S_SYSCALLS      12      /* Client requests to ISIS */
#define S_CLSENT        13      /* ISIS messages to clients */
#define S_CFAULT        14      /* Cache faults */
#define S_LOOKUP        15      /* Name lookups */
#define S_VCHANGE       16      /* View changes */
#define S_ISENT         17      /* Intersite packets I sent */
#define S_IGOT          18      /* Intersite packets I got */
#define S_IMSGS         19      /* Actual messages accepted after ignoring acks and dups */
#define S_FANOUT        20      /* For computing fanout factor */
#define S_NFORK         21      /* Number of tasks created */
#define S_NSWTCH        22      /* Context switches */
#define S_BBSTART       23      /* bcast started */
#define S_BBDONE        24      /* bcast done */
#define S_CONGEST	25	/* Got congested */

#define S_NSTATS        26

#define IS_CONGEST	0x0001	/* Congested */
#define IS_MEM		0x0002	/* Memory used */
#define IS_MSG		0x0004	/* Message memory used */
#define IS_TASK		0x0008	/* Ntasks */
#define IS_INTER	0x0010  /* intersite backlog */


struct  isis_stats
{
        int             is_stats[S_NSTATS];
        int             is_ntasks;
        int             is_congest;
        int             is_ccount;
        int             is_memuse;
        int             is_msgmem;
        int             is_inter;
        int             is_nlocdelete;
        int             is_ndelete;
        int             is_time;
} isis_stats;

#define EVENT(type)             (isis_stats.is_stats[type]++)
#define EVENTV(type,val)        (isis_stats.is_stats[type] += val)

#define C_NSENT          0      /* Sent to this site */
#define C_NACK           1      /* Acks to this site */
#define C_NGOT           2      /* Packets read from net */
#define C_NGACK          3      /* Acks I received */
#define C_NMSG           4      /* Messages they contained */
#define C_NDUP           5      /* Number that were dups */

#define N_COMM           6

struct  comm_stats
{
        int             c_stats[MAX_SITES][N_COMM];
} comm_stats;

#define CEVENT(site,type)       (comm_stats.c_stats[site][type]++)
#define CEVENTV(site,type,val)  (comm_stats.c_stats[site][type] += val)
