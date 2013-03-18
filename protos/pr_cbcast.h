/*  $RCSfile: pr_cbcast.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:00 $  */
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
 *
 */
#ifndef PR_CBCAST
#define PR_CBCAST

#include "pr.h"
#include "pr_gbcast.h"

struct pbuf_item
{
    message     *msg;
    qnode       *rem_dests; /* dests not received at or sent to from here */
    bitvec   sent;       /* sites received at or sent to from here     */
    bitvec   received;   /* sites known to have received the message   */
    short       piggy_flag;
    short       refcount;
    qnode       *wakeup;    /* qnode of wait_structs for this message     */
};

extern  adesc   pbuf_adesc;
#define pbuf_alloc()   ((pbuf_item *) mallocate (&pbuf_adesc))
int     pbuf_free();

qnode  *pbufs, *pb_itemlist, *idlists, *piggylists;

int         pr_cbcast(), cb_sendpkt(), cb_recvpkt(), cb_deliver(),
            cb_finddests(), idlist_add(), cb_addtopbuf(), cb_init();
int         cb_updatepbuf(), cb_makeplist(), cb_makelist(),
            cb_cleanup(), pbuf_free();
qnode       *pbuf_find();
pbuf_item   *cb_createpbitem();

#endif
