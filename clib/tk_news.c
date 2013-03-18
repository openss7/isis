/*  $RCSfile: tk_news.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:00 $  */
/*
 *      Coded by Frank Schmuck
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
 *      cl_news.c  --  news service: client code
 */

/*  changes for new ISIS interface:
 *  12-14-87 Frank: changed all bcast's and msg_xxx to new version,
 *      except for msg_insertfield's in news_post, news_posta, news_clear,
 *      and news_clear_all. (ther are no reply's in cl_news.c)
 *  02-24-88 Frank: changed all msg_insertfield's to msg_putfld's
 */

#include "isis.h"
#include <stdio.h>
#include <string.h>
#include <sys/file.h>

extern sview *site_getview();


/*********************************************************************
*
*  news_subscribe, news_cancel
*
*********************************************************************/

int  news_subscribe(subj, entry, back)
    char  *subj;
    int   entry; 
    int   back;
{
    int         n;
    long        answer;
    address addr;

    /*** send subscription message to local news service ***/
    addr = ADDRESS(my_site_no, my_site_incarn, NEWS, 0);
    n = cbcast(&addr, MSG_SUBSRCIBE, "%s %d %d", subj, entry, back, 1, "%d", &answer);
    return (n == 1)? (int)answer: -1;
}


int  news_cancel(subj)
    char  *subj;
{
    int         n;
    long        answer;
    address     addr;

    /*** send cancel message to local news service ***/
    addr = ADDRESS(my_site_no, my_site_incarn, NEWS, 0);
    n = cbcast(&addr, MSG_CANCEL, "%s", subj, 1, "%d", &answer);
    return (n == 1)? answer: -1;
}


/*********************************************************************
*
*  news_post,  news_clear
*
*********************************************************************/

static  address  *make_alist(slist, entry)
    site_id  slist[];
    int      entry;
{
        int      i;
static  address  alist[MAX_SITES+1];

    if (slist == NULL) {
        slist = site_getview()->sv_slist;
    }
    for(i = 0; slist[i] != 0; i++) {
        alist[i].addr_site    =  SITE_NO(slist[i]);
        alist[i].addr_incarn  =  SITE_INCARN(slist[i]);
        alist[i].addr_portno  =  0;
        alist[i].addr_process =  NEWS;
        alist[i].addr_entry   =  entry;
    }
    alist[i] = NULLADDRESS;
    return alist;
}


void  news_post(slist, subj, mp, back)
    site_id  slist[];
    char     *subj;
    message  *mp;
    int      back;
{
    /*** add subject and back field to message ***/
    msg_putfld(mp, FLD_SUBJ, "%s", subj);
    msg_putfld(mp, FLD_BACK, "%d", back);

    /*** broadcast message to news services ***/
    (void) cbcast_l("lm", make_alist(slist, MSG_POST), mp, 0);
}

void  news_apost(slist, subj, mp, back)
    site_id  slist[];
    char     *subj;
    message  *mp;
    int      back;
{
    /*** add subject and back field to message ***/
    msg_putfld(mp, FLD_SUBJ, "%s", subj);
    msg_putfld(mp, FLD_BACK, "%d", back);

    /*** broadcast message to news services ***/
    (void) abcast_l("lm", make_alist(slist, MSG_POST), mp, 0);
}


void  news_clear(slist, subj)
    site_id  slist[];
    char     *subj;
{
    message  *mp;

    /*** prepare clear message ***/
    mp = msg_newmsg();
    msg_putfld(mp, FLD_SUBJ, "%s", subj);

    /*** broadcast message to news services ***/
    (void) cbcast_l("lm", make_alist(slist, MSG_CLEAR), mp, 0);
    msg_delete(mp);
}

void  news_clear_all(slist, subj)
    site_id  slist[];
    char     *subj;
{
    message  *mp;

    /*** prepare clear message ***/
    mp = msg_newmsg();
    msg_putfld(mp, FLD_SUBJ, "%s", subj);

    /*** broadcast message to news services ***/
    (void) cbcast_l("lm", make_alist(slist, MSG_CLEARALL), mp, 0);
    msg_delete(mp);
}

