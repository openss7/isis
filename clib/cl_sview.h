/*  $RCSfile: cl_sview.h,v $ $Revision: 2.16 $ $Date: 90/06/24 14:53:57 $  */
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

/*  interface routines. */

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
sview	*site_getview	();
int	sv_monitor_cancel(int svid);
int	sv_watch_cancel	(int svid);

#if  ( __cplusplus || c_plusplus )
int	sv_monitor	(v_routine_vs, VOID *arg);
int	sv_watch	(int sid, int event, v_routine_vs, VOID *arg);
#else
int	sv_monitor	(void (*routine)(VOID *arg0), VOID *arg1);
int	sv_watch	(int sid, int event, void (*routine)(VOID *arg0),
                         VOID *arg1);
#endif
#ifdef __cplusplus
}
#endif
#endif

/*** Internal routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
void	site_monitor_dump();
void	sv_doecall	(svmon *svm);
void	sv_dovcall	(svmon *svm);
void	sv_free		(svmon *svm);
void	sv_init		();
void	sv_new_sview	(message *mp);
#ifdef __cplusplus
}
#endif
#else

void	sv_new_sview();
#endif
