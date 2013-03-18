/*  $RCSfile: tk_rmgr.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:10 $  */
/*
 *	Originally coded by Frank Schmuck
 *      cl_rmgr.h:  recovery manager
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

/*********************************************************************
*
*  Process Restart Tool
*
*********************************************************************/

#define RMLEN 40

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	int rmgr_update(char *key, char *program, char **argv, char **envp);
	int rmgr_register(char *key);
	int rmgr_unregister();
#ifdef __cplusplus
}
#endif
#else
extern int rmgr_update( /* char *key, *program, *argv[], *envp[]; */ );
extern int rmgr_register( /* char *key; */ );
extern int rmgr_unregister();
#endif

/* error codes */
#define RM_ELOCKED    1		/* the rmgr.rc file is locked */
#define RM_ENOTFOUND  2		/* pgroup not found in rmgr.rc file */

/*********************************************************************
*
*  Pgroup View Log Tool
*
*********************************************************************/

typedef struct {
	int rm_mode;			/* restart mode (see below) */
	char rm_key[RMLEN];		/* key */
	groupview rm_view;		/* local view before proc/site crash */
} rmgr_info;

typedef struct {
	char rl_key[RMLEN];		/* key */
	groupview rl_view;		/* local view log */
} rmgr_viewlog;

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	int rmgr_start_log(address * gad, char *key);
	int rmgr_stop_log(address * gad, char *key);
	int rmgr_lasttofail(char *gname, char *key, int *old_incarn_p, int noblock);
	rmgr_info *rmgr_getinfo(char *pgname, int noblock);
#ifdef __cplusplus
}
#endif
#else
extern int rmgr_start_log( /* address gaddr; char *key */ );
extern int rmgr_stop_log( /* address gaddr; char *key */ );
extern rmgr_info *rmgr_getinfo( /* char *gname; int noblock */ );
extern int rmgr_lasttofail( /* char *gname; char *key; int old_incarn_p; int noblock */ );
#endif

/* restart mode: bitvec*tor in which the following flags are set: */
#define RM_LOG    0x01		/* pgview was read from view-log file into rm_view */
#define RM_RECENT 0x02		/* rm_view is the most recent view of the group */
#define RM_SURE   0x04		/* RM_RECENT info is reliable */

/*********************************************************************
*
*  Pgroup Restart Tool
*
*********************************************************************/
extern int rmgr_optv;

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	address *rmgr_create(rmgr_info * rmi);
	address *rmgr_join(rmgr_info * rmi);
	address *rmgr_restart(char *pgname);
	void rmgr_mh_rmnews(message * mp);
	void rmgr_mh_rmup(message * mp);
	char *rmgrbuf_init(char *buf, char *fmt, char *arg);
#ifdef __cplusplus
}
#endif
#else
extern address *rmgr_create( /* rmgr_info *rmi; */ );
extern address *rmgr_join( /* rmgr_info *rmi; message *mp; */ );
extern address *rmgr_restart( /* char *pgname; */ );

extern void rmgr_mh_rmnews();
extern void rmgr_mh_rmup();
#endif
