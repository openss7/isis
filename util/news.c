/*
 *	Originally by Frank Schmuck
 *
 *      news.c  --  news service
 *  changes for new ISIS interface:
 *     12-14-87 Frank: changed all bcast's, reply's, and msg_xxx to new version.
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


char news_rcsid[] = "$Revision: 2.6 $$Date: 90/06/20 14:36:41 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/news.c,v $";

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/file.h>
#include "isis.h"

extern sview *site_getview();

static  bool     optv = FALSE, optvv = FALSE;

/*********************************************************************
*
*  subject table 
*
*********************************************************************/

#define MAXSUBJ  128    /* max number of different subjects */
#define MAXSUBS   20    /* max number of subscriptions for same subject */
#define MAXMSG    20    /* max number of news messages held back */

int  stat_subj = 0;     /* status: number of active subjects */
int  stat_subs = 0;     /* status: total number of subsribers */
int  stat_back = 0;     /* status: total number of back issues held */

static  struct st {
            char        subj[SUBJLEN];
            int         seqno;
            int         nmsg;
            message     *msgs[MAXMSG];
            int         xpire[MAXMSG];
            int         nsubs;
            address     subscr[MAXSUBS];
            bool        used;
        } subj_table[MAXSUBJ];


static  struct st  *subj_new()
{
    register struct st   *stp;
    
    for (stp = subj_table; stp < subj_table+MAXSUBJ; stp++) {
        if (! stp->used) {
            stat_subj++;
            stp->seqno = 0;
            stp->nmsg  = 0;
            stp->nsubs = 0;
            stp->used  = TRUE;
            return stp;
        }
    }
    return NULL;
}


void subj_del(stp)
    register struct st   *stp;
{
    if (optvv) 
        printf("news %d: subject deleted (\"%s\")\n", my_site_no, stp->subj);
    stat_subj--;
    stp->used = FALSE;
}

 
static  struct st  *subj_find(subj)
    register char *subj;
{
    register struct st   *stp;
    
    for (stp = subj_table; stp < subj_table + MAXSUBJ; stp++) {
        if (stp->used  &&  strcmp(stp->subj, subj) == 0) return stp;
    }
    return NULL;
}


static  int  subj_finds(stp, adr)
    register struct st  *stp;
    register address    *adr;
{
    register address    *sadr;

    for (sadr = stp->subscr; sadr < stp->subscr + stp->nsubs; sadr++) {
        if (addr_isequal(sadr, adr))
            return  sadr - stp->subscr;
    }
    return -1;
}


/*********************************************************************
*
*  main_task, mh_command
*
*********************************************************************/

void main_task()
/*
 *  this routine is forked off by main()
 */
{
    if (optv) printf("news %d: started\n", my_site_no);
}

static  void  mh_command(mp)
    message *mp;
/*
 *  handler for command messages
 */
{
    int        argc;
    char       *argv[32];
    int        i;
    char       *answer = "ok.\n";
    char       buffer[256];
    struct st  *stp;

    getargs(mp, argc, argv, 32);

    if (strcmp(argv[0], "echo") == 0) {
        printf("news %d:", my_site_no);
        for (i = 0; i < argc; i++) {
            printf(" %s", argv[i]);
        }
        printf("\n");

    } else if (strcmp(argv[0], "status") == 0) {
        sprintf(answer = buffer,
          "%d subject(s), %d subsriber(s), %d back issue(s); %s\n",
          stat_subj, stat_subs, stat_back, optvv? "+vv": (optv? "+v": ""));

    } else if (strcmp(argv[0], "dump") == 0) {
        printf("news %d:  <<subscription table dump>>\n", my_site_no);
        for (stp = subj_table; stp < subj_table+MAXSUBJ; stp++) {
            if (stp->used) {
                printf("    \"%s\"\t seqno = %d, nmsg = %d, nsubs = %d\n",
                  stp->subj, stp->seqno, stp->nmsg, stp->nsubs);
            }
        }

    } else if (strcmp(argv[0], "+v") == 0) {
        optv = TRUE;

    } else if (strcmp(argv[0], "-v") == 0) {
        optv = FALSE;

    } else if (strcmp(argv[0], "+vv") == 0) {
        optv = TRUE;
        optvv = TRUE;

    } else if (strcmp(argv[0], "-vv") == 0) {
        optv = FALSE;
        optvv = FALSE;

    } else if (strcmp(argv[0], "help") == 0) {
        sprintf(answer = buffer,
          "Commands: echo, status, +v, -v, +vv, -vv, help.\n");

    } else {
        sprintf(answer = buffer, "*** %s: unknown command\n", argv[0]);
    }
    reply(mp, "%s", answer);
}

/*********************************************************************
*
*  message handling routines:  mh_subscribe, mh_cancel
*
*********************************************************************/

static  void  mh_subscribe(mp)
    message *mp;
/*
 *  sent by a local client that wants to subscribe to a subject
 */
{
    char        *subj;
    long        entry;
    long        back;
    address     *sender;
    struct st   *stp;
    int         subi, msgi;
    long        answer;
    address     alist[2];

    msg_get(mp, "%-s %d %d", &subj, &entry, &back);
    sender = msg_getsender(mp);
    if (optv) printf("news %d: subscribe(\"%s\", %d, %d)\n",
      my_site_no, subj, entry, back);

    /*** find/create subject entry in subject table ***/
    if ((stp = subj_find(subj)) == NULL) {
        if ((stp = subj_new()) == NULL) {
            fprintf(stderr, 
                "news %d: subject table overflow; can't subscribe \"%s\".\n",
                my_site_no, subj);
            reply(mp, "%d", -1);
            return;
        }
        strncpy(stp->subj, subj, SUBJLEN);
    }
    /*** find/create subscriber entry in subscription list ***/
    if ((subi = subj_finds(stp, sender)) < 0) {
        if (stp->nsubs >= MAXSUBS) {
            fprintf(stderr, 
                "news %d: subscription list overflow; can't subscribe \"%s\".\n",
                my_site_no, subj);
            reply(mp, "%d", -2);
            return;
        }
        stat_subs++;
        subi = stp->nsubs++;
        stp->subscr[subi] = *sender;
    }
    stp->subscr[subi].addr_entry = entry;
    /*** compute how many back issues should be sent ***/
    if (stp->nmsg <= back) {
        answer = (long)stp->nmsg;
        msgi = 0;
    } else {
        answer = (long)back;
        msgi = stp->nmsg - back;
    }
    reply(mp, "%d", answer);
    /*** send back issues ***/
    alist[0] = stp->subscr[subi];
    alist[1] = NULLADDRESS;
    while (answer-- > 0) {
        if (optvv)
            printf("news %d: forwarding back issue \"%s\" to %d/%d:%d/%d\n",
              my_site_no, subj,
              alist->addr_site, alist->addr_incarn, alist->addr_process, alist->addr_entry);
        (void) cbcast_l("lm", alist, stp->msgs[msgi++], 0);
    }
    return;
}


static  void  mh_cancel(mp)
    message *mp;
/*
 *  sent by a local client that wants to cancel a subscription 
 */
{
    char        *subj;
    address     *sender;
    struct st   *stp;
    int         subi;
    long        answer;

    msg_get(mp, "%-s", &subj);
    sender = msg_getsender(mp);
    if (optv) printf("news %d: cancel(\"%s\")\n", my_site_no, subj);

    /*** find subscription in subject table ***/
    if ((stp = subj_find(subj)) == NULL) {
        subi = -1;
    } else {
        subi = subj_finds(stp, sender);
    }
    if (subi < 0) {
        answer = -1;
    } else {
        /*** remove subscription from subscr list ***/
        stat_subs--;
        stp->nsubs--;
        if (stp->nsubs > subi) stp->subscr[subi] = stp->subscr[stp->nsubs];
        if (stp->nsubs == 0  &&  stp->nmsg == 0) {
            subj_del(stp);
        }
        answer = 0;
    }
    reply(mp, "%d", answer);
    return;
}


/*********************************************************************
*
*  message handling routines:  mh_post,  mh_clear
*
*********************************************************************/

static  void  mh_post(mp)
    message *mp;
/*
 *  sent by an isis client that wants to post a message 
 */
{
    char        *subj;
    long        back;
    struct st   *stp;
    int         msgi, i;
    address     alist[MAXSUBS+1];

    msg_getfld(mp, FLD_SUBJ, 0, "%-s", &subj);
    msg_getfld(mp, FLD_BACK, 0, "%d",  &back);
    if (optv) printf("news %d: post(\"%s\", %d)\n", my_site_no, subj, back);

    /*** find/create subject entry in subject table ***/
    if ((stp = subj_find(subj)) == NULL) {
        if ((stp = subj_new()) == NULL) {
            fprintf(stderr, 
                "news %d: subject table overflow; can't post \"%s\".\n",
                my_site_no, subj);
            return;
        }
        strncpy(stp->subj, subj, SUBJLEN);
    }
    /*** increment seqno, delete expired messages ***/
    stp->seqno++;
    msgi = 0;
    for (i = 0; i < stp->nmsg; i++) {
        if (stp->seqno >= stp->xpire[i]) {
            stat_back--;
            msg_delete(stp->msgs[i]);
        } else {
            stp->msgs[msgi++] = stp->msgs[i];
        }
    }
    stp->nmsg = msgi;
    /*** add new posting to message list ***/
    if (back > 0) {
        if (stp->nmsg < MAXMSG) {
            msgi = stp->nmsg++;
        } else {
            /*** discard oldest message ***/
            stat_back--;
            msg_delete(stp->msgs[0]);
            for (i = 1; i < MAXMSG; i++) stp->msgs[i-1] = stp->msgs[i];
            msgi = MAXMSG-1;
        }
        stat_back++;
        msg_increfcount(mp);
        stp->msgs[msgi] = mp;
        stp->xpire[msgi] = stp->seqno + back;
    }
    /*** forward message to all subscribers ***/
    if (stp->nsubs > 0) {
        if (optvv)
            printf("news %d: forwarding posting \"%s\" to", my_site_no, subj);
        for (i = 0; i < stp->nsubs; i++) {
            alist[i] = stp->subscr[i];
            if (optvv)
                printf(" %d/%d:%d/%d",
                  alist[i].addr_site, alist[i].addr_incarn,
                  alist[i].addr_process, alist[i].addr_entry);
        }
        if (optvv)
            printf("\n");
        alist[stp->nsubs] = NULLADDRESS;
        (void) cbcast_l("lm", alist, mp, 0);
    }
    if (stp->nsubs == 0  &&  stp->nmsg == 0) {
        subj_del(stp);
    }

}


static  void  mh_clear(mp)
    message *mp;
/*
 *  sent by an isis client that wants delete all previously posted messages 
 */
{
    char        *subj;
    address     *sender;
    struct st   *stp;
    int         msgi, i;

    msg_getfld(mp, FLD_SUBJ, 0, "%-s", &subj);
    sender = msg_getsender(mp);
    if (optv) printf("news %d: clear(\"%s\")\n", my_site_no, subj);

    /*** find subject in subject table ***/
    if ((stp = subj_find(subj)) == NULL) {
        fprintf(stderr, "news %d: clear: unknown subject \"%s\".\n",
          my_site_no, subj);
        return;
    }
    /*** remove messages posted by this sender ***/
    msgi = 0;
    for (i = 0; i < stp->nmsg; i++) {
        if (addr_isequal(sender, msg_getsender(stp->msgs[i]))) {
            stat_back--;
            msg_delete(stp->msgs[i]);
        } else {
            stp->msgs[msgi++] = stp->msgs[i];
        }
    }
    stp->nmsg = msgi;
    if (stp->nsubs == 0  &&  stp->nmsg == 0) {
        subj_del(stp);
    }
}

static  void  mh_clearall(mp)
    message *mp;
/*
 *  sent by an isis client that wants delete all previously posted messages 
 */
{
    char        *subj;
    struct st   *stp;
    int         msgi, i;

    msg_getfld(mp, FLD_SUBJ, 0, "%-s", &subj);
    if (optv) printf("news %d: clearall(\"%s\")\n", my_site_no, subj);

    /*** find subject in subject table ***/
    if ((stp = subj_find(subj)) == NULL) {
        fprintf(stderr, "news %d: clear: unknown subject \"%s\".\n",
          my_site_no, subj);
        return;
    }
    /*** remove all messages posted on this subject ***/
    for (i = 0; i < stp->nmsg; i++) {
        stat_back--;
        msg_delete(stp->msgs[i]);
    }
    stp->nmsg = 0;
    if (stp->nsubs == 0) {
        subj_del(stp);
    }
}


/*********************************************************************
*
*  main 
*
*********************************************************************/

main(argc, argv)
    int  argc;
    char *argv[];
{
    int  client_port;   /* port number for talking to isis */
    int  i;

    /*** read command line arguments ***/
    client_port = 0;
    i = 0;
    for (i = 1; i < argc; i++) {
        if (*argv[i] >= '0'  &&  *argv[i] <= '9') {
            client_port = atoi(argv[i]);
        } else if (strcmp(argv[i], "+v") == 0) {
            optv = TRUE;
        } else if (strcmp(argv[i], "+vv") == 0) {
            optv = TRUE;
            optvv = TRUE;
        } else {
            fprintf(stderr, "usage: %s [+v[v]] port\n", argv[0]);
            exit(-1);
        }
    }
    if (client_port == 0) {
        fprintf(stderr, "usage: %s port\n", argv[0]);
        exit(-1);
    }

    /*** set up isis stuff ***/
    freopen("/dev/null", "r", stdin);
    my_process_id = NEWS;
    isis_init(client_port);

    isis_entry(MSG_COMMAND,   mh_command,   "mh_command");
    isis_entry(MSG_SUBSRCIBE, mh_subscribe, "mh_subscribe");
    isis_entry(MSG_CANCEL,    mh_cancel,    "mh_cancel");
    isis_entry(MSG_POST,      mh_post,      "mh_post");
    isis_entry(MSG_CLEAR,     mh_clear,     "mh_clear");

    isis_mainloop(main_task, NULLARG);
}

        
