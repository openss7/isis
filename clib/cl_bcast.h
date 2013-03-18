/*  $RCSfile: cl_bcast.h,v $ $Revision: 2.103 $ $Date: 90/09/11 15:37:03 $  */
/*
 * Broadcast interface routines.
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
 */


#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
void	abortreply	(message *msg);
int	abortreply_l	(message *msg, int errno);
int	bc_cancel	(int bcid);
event_id *bc_getevent	(int bcid);
int	bc_poll		(int bcid);
int	bc_wait		(int bcid);
void	flush		();
void	forward		(message *fmsg, address *to, int ent, message *cmsg);
int	nullreply	(message *msg);
#if ( __cplusplus || c_plusplus )
int	abcast		(address *dest, int entry, char *out_fmt ...);
int	bcast		(address *dest, int entry, char *out_fmt ...);
int	cbcast		(address *dest, int entry, char *out_fmt ...);
int	fbcast		(address *dest, int entry, char *out_fmt ...);
int	gbcast		(address *dest, int entry, char *out_fmt ...);
int	abcast_l	(char *option_string ...);
int	bcast_l		(char *option_string ...);
int	cbcast_l	(char *option_string ...);
int	fbcast_l	(char *option_string ...);
int	gbcast_l	(char *option_string ...);
void	reply		(message *in_msg, char *out_format ...);
void	reply_l		(char *option_string, message *in_msg ...);
#else
int	abcast	();
int	abcast_l	();
int	bcast		();
int	bcast_l		();
int	cbcast		();
int	cbcast_l	();
int	fbcast		();
int	fbcast_l	();
int	gbcast		();
int	gbcast_l	();
void	reply		();
void	reply_l		();
#endif

#ifdef __cplusplus
}
#endif
#else
int	nullreply	();
#endif

/*** Internal routines ***/

#if FUN_TYPE
#ifdef __cplusplus
extern "C" {
#endif
void	BCAST		(bc_args *bc);
int	bc_free		(bc_args *bc);
int	do_bcast	(int pro, char *opstr, va_list *ap);
void	do_reply	(va_list *ap);
void	do_reply_l	(va_list *ap);
address	*eid_sender	(event_id *eid);
int	gbcast_grow	();
#ifdef __cplusplus
}
#endif
#else

void	BCAST		();
void	do_reply	();
void	do_reply_l	();

#endif

/* Bcast options */
#define ISISBC_LIST          0x0002
#define ISISBC_TASK          0x0004
#define ISISBC_CC            0x0008
#define ISISBC_MSEND         0x0010
#define ISISBC_MRCV          0x0020
#define ISISBC_LAZY          0x0040
#define ISISBC_REPLY         0x0080
#define ISISBC_BYPASS        0x0100
#define ISISBC_EXCLUDE       0x0200
#define ISISBC_TIMEOUT       0x0400
#define ISISBC_NONVSYNC      0x0800
#define ISISBC_GROW          0x1000

#define CL_MBCAST	-1		/* Special-case in bypass */
