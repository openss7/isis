/*  $RCSfile: cl_coord.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:20:45 $  */
/*
 *  Coordinator cohort interface routines.
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

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
void	cc_refuse	();
void	cc_terminate	();
void	cc_terminate_msg(message *msg);
int	coord_cohort	(message *msg, address *gaddr,
                         void (*action)(message *msg, address *gaddr,
                                       int how, VOID *arg),
                         void (*got_result)(message *msg),
                         VOID *arg);
int	coord_cohort_l	(message *msg, address *gaddr,
                         void (*action)(message *msg, address *gaddr,
                                       int how, VOID *arg),
                         void (*got_result)(message *msg),
                         VOID *arg,
                         address *(*croutine)(address *gaddr, address *senderp,
                                              address *players, VOID *arg));
#ifdef __cplusplus
}
#endif
#else

void cc_refuse();

#endif

/*** Internal routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
void	cc_panic	(message *msg, address *who, int msgid, char *why);
void	cc_result	(message *msg);
qnode	*cc_watching	(address *sender, int msgid, address *coord);
void	coord_init	();
void	cohort	(address *gaddr, address *paddr, int event, condition *cond);
address	*coordinator	(address *gaddr, address *senderp, address *players,
                         void *arg);
void	do_ccterminate	(address *dest, va_list *ap);
#ifdef __cplusplus
}
#endif
#else

void    cc_result();
void 	cohort(), do_ccterminate();

#endif
