/*  $RCSfile: tk_news.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:01 $  */
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
/* cl_news.h:  news service */

#define MSG_SUBSRCIBE   1
#define MSG_CANCEL      2
#define MSG_POST        3
#define MSG_CLEAR       4
#define MSG_CLEARALL    5

#define SUBJLEN         80
#define FLD_SUBJ        SYSFLD_UNSPEC1
#define FLD_BACK        SYSFLD_UNSPEC2
#define FLD_ENTRY       1

/*** Interface Routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	void news_apost(site_id * slist, char *subj, struct message *mp, int back);
	int news_cancel(char *subj);
	void news_clear(site_id * slist, char *subj);
	void news_clear_all(site_id * slist, char *subj);
	void news_post(site_id * slist, char *subj, struct message *mp, int back);
	int news_subscribe(char *subj, int entry, int back);
#ifdef __cplusplus
}
#endif
#else

extern int news_subscribe( /* char *subj; int entry, back; */ );
extern int news_cancel( /* char *subj; */ );
extern void news_post( /* site_id slist[]; char *subj; message *mp; int back; */ );
extern void news_apost( /* site_id slist[]; char *subj; message *mp; int back; */ );
extern void news_clear( /* site_id slist[]; char *subj; */ );
extern void news_clear_all( /* site_id slist[]; char *subj; */ );

#endif
