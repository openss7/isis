/*  $RCSfile: cl_dump.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:20:48 $  */
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

/* Dump interface routines. */

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
void	cl_dump( /* int level, char *why, ... */ );
void	isis_logging	(int flag);
void	paddr		(address *addr);
void	paddrs		(address *addrs);
void	peid		(event_id eid);
void	pmsg		(message *msg);
void	psid		(int sid);
#ifdef __cplusplus
	      }
#endif
#else

void cl_dump(), isis_logging(), paddr(), peid(), pmsg(), psid();

#endif

/*** Internal routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
address	atoaddr	(char *str);
char	*callername	(int n_levels);
void	dump_cond	(qnode *qp);
void	dump_sid	(qnode *qp);
void	dump_sview	(sview *sv);
void	dump_task	(qnode *qp);
#ifdef __cplusplus
}
#endif

#else

void dump_task();

#endif

