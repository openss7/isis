/*  $RCSfile: pr_fdect.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:22 $  */
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
#ifndef     FD
#define     FD

#define     MAKE_SITE_ID(site_no,incarn) \
                                    ((site_id) ((((site_no) & 0xff) << 8) | \
                                    ((incarn) & 0xff)))
#define     SITE_NO(s_id)           (site_id)(((s_id) & 0xff00) >> 8)
#define     SITE_INCARN(s_id)       ((s_id) & 0xff)
#define     INCARN_INCR(s_id)       (((s_id) & ~0x7f) | \
                                    ((((s_id) & 0x7f) + 1) &  0x7f))
#define     INCARN_DECR(s_id)       (((s_id) & ~0x7f) | \
                                    ((((s_id) & 0x7f) - 1) &  0x7f))
#define     RECOVERY_INCARN         0x80
#define     ILLEGAL_INCARN          0x81
#define     DOWN_INCARN             0xff

struct sview
{
            int                     sv_viewid;
            site_id                 sv_slist[MAX_SITES + 1];
            u_char                  sv_incarn[MAX_SITES + 1];
            bitvec               sv_failed;
            bitvec               sv_recovered;
};

#define     SITE_IS_UP(s_no,inc)    (current_view.sv_incarn[s_no] == (inc))
#define     RECOV_VIEWID            0
#define     VIEW_NO(id)             ((id) >> 8)
#define     VIEW_IS_GT(id1,id2)     (VIEW_NO (id1) > VIEW_NO (id2))
#define     INCR_VIEWID(id)         (((((id) >> 8) + 1) << 8) | \
                                                       (my_site_no & 0xff))

extern sview       current_view;
extern int         fd_coordinator, exists_proposal, exists_old_proposal,
                     sent_old_proposal;
extern int         proposed_viewid, old_proposed_viewid;
extern site_id     proposed_slist[MAX_SITES + 1], old_proposed_slist[MAX_SITES + 1];
extern bitvec      proposed_failed, proposed_recovered,
                       old_proposed_failed, old_proposed_recovered;
extern qnode       *pending_failures, *pending_recoveries, *replies_wanted;
extern char        fd_forked, participant_failed;
extern condition   got_all_replies;
site_id     fd_growslist();
int         fd_notifyview();

u_char      sview_nlocks[MAX_SITES];
extern bitvec   sview_wlocks, sview_rlocks, sview_want_wlocks;
extern condition   wait_r_lock, wait_w_lock;

qnode       *watch_queue[MAX_SITES];
int         wmagic ();

extern int         lastviewid;

#define     FD_TOTAL                1
#define     FD_PARTIAL              2
#define     FD_SHRINK               1
#define     FD_GROW                 2
#define     FD_PROPOSED             1
#define     FD_CURRENT              2
#define     FD_TIMEOUT              -1
#define     FD_TIMEOUTVAL           2000

#endif      FD
