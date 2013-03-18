/*  $RCSfile: spooler.h,v $ $Revision: 2.97 $ $Date: 90/09/07 16:54:23 $  */
 /* 
 *      Originally coded by Ken Birman 
 *      Spooler definitions file
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
 

#define SP_BASE                 10

#define SP_INFINITY             0x80000000	/* Same code for +/- infinity */

/* Special, map to SP_BCAST <type> */
#define SP_GBCAST		-1
#define SP_ABCAST		-2
#define SP_CBCAST		-3

/* Special, map to SP_TIME <atime> */
#define SP_ATIME		-4
#define SP_RTIME		-5

/* Keywords for spooling a message */
#define SP_SPSEQN               (SP_BASE+0)
#define SP_EXPIRES		(SP_BASE+1)
#define SP_KEYWORDS		(SP_BASE+2)
#define SP_TIME                 (SP_BASE+3)
#define SP_SPOOLER		(SP_BASE+4)
#define SP_SENDER		(SP_BASE+5)
#define SP_PREPEND              (SP_BASE+6)
#define SP_NETWORK              (SP_BASE+7)
#define SP_BCAST                (SP_BASE+8)

#define SP_MAX                  (SP_BASE+9)

/* Spooler entry points */
#define SP_SPOOL                1
#define SP_REPLAY               2
#define SP_DISCARD              3
#define SP_SPOOL_AND_DISCARD    4
#define SP_PREPEND_AND_DISCARD  5
#define SP_CANCEL               6
#define SP_WAIT                 7
#define SP_ADVISE               8
#define SP_LOOKUP               9
#define SP_INQUIRE              11
#define SP_SET_RPOINTER         12
#define SP_PLAY_THROUGH         13
#define SP_SET_CPOINTER         14
#define SP_LH_SRCERR            15
#define SP_LH_DSTERR            16


/* Long-haul entry points start here */
#define LH_BASE			20
#define REG_ENTRY               (LH_BASE + 1)
#define WEIGHT_ENTRY            (LH_BASE + 2)
#define STARTUP_ENTRY           (LH_BASE + 3)
#define UPDATE_LOWER_BOUND      (LH_BASE + 4)
#define UPDATE_OLDEST_INMESS    (LH_BASE + 5)
#define PARTNER_FAILED          (LH_BASE + 6)
#define COMSTATE_ADD_ENTRY      (LH_BASE + 7)
#define RESPOOL_REQUEST         (LH_BASE + 8)
#define LH_JOIN_REQUEST         (LH_BASE + 9)
#define LH_CBCAST_REQUEST       (LH_BASE + 10)
#define LH_JOIN                 (LH_BASE + 11)
#define LH_CBCAST               (LH_BASE + 12)
#define LH_XFER                 (LH_BASE + 13)
#define LH_XFER_REQUEST         (LH_BASE + 14)
#define LH_ABCAST               (LH_BASE + 15)
#define LH_ABCAST_REQUEST       (LH_BASE + 16)
#define LH_CONVID_REQUEST       (LH_BASE + 17)
#define LH_XFER_RCV_FAILURE     (LH_BASE + 18)
#define LH_XFER_SND_FAILURE     (LH_BASE + 19)
#define LH_COMSTATE_UPDATE      (LH_BASE + 20)
#define LH_MAX_ENTRIES          20

#define SP_ON                   0
#define SP_OFF                  1

#define LH_COPY 0
#define LH_MOVE 1

#define spool_getseqn(mp)       (msg_getfield(mp, SYSFLD_SPSEQN, 1, 0)? \
                                  *(int*)msg_getfield(mp, SYSFLD_SPSEQN, 1, 0): 0)

#define get_fname_and_holder(mp,lname,holder) \
  lname = (char*) 0; holder = (char *) 0; \
  (msg_getfld(mp, SYSFLD_LH_FILEREP, 0, "%+s%+s", &lname, &holder))

#define lh_get_error_report(mp,src,dst,net,err) \
src = dst = net = (char*) 0; err = 0; \
(msg_get(mp, "%-s%-s%-s%d", &src, &dst, &net, &err));


int	spool_in_replay;	/* Set when delivery of a "replayed message" is occuring */


/*  Spooler interface routines. */

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
int	spool_cancel	(char *sname, int sid);
int	spool_inquire	(char *sname, int sid);
void	spool_play_through(char *sname, int on_off);
void	spool_set_ckpt_pointer(char *sname, int spseqn);
void	spool_set_replay_pointer(char *sname, int spseqn);
int	spool_wait	(char *sname, int sid);
#if ( __cplusplus || c_plusplus )
void	spool		(char *sname, int entry, char *fmt ...);
void	spool_and_discard(char *sname, int entry, char *fmt ...);
void	spool_discard	(char *sname, int pat ...);
void	spool_m		(char *sname, int entry, message *msg, int sp_key,
                         ...);
void	spool_m_and_discard(char *sname, int entry, message *msg, int sp_key,
                            ...);
void	spool_replay	(char *sname, int sp_pat ...);
#else
void	spool		();
void	spool_and_discard();
void	spool_discard	();
void	spool_m		();
void	spool_m_and_discard();
void	spool_replay	();
#endif

#ifdef __cplusplus
}
#endif

#else

void	spool		();
void	spool_and_discard();
int	spool_cancel	();
void	spool_discard	();
int	spool_inquire	();
void	spool_m		();
void	spool_m_and_discard();
void	spool_play_through();
void	spool_replay	();
void	spool_set_ckpt_pointer();
void	spool_set_replay_pointer();
int	spool_wait	();

#endif

/*** Internal routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
void	do_spool	(va_list *ap, int m_mode);
void	do_spool_and_discard(va_list *ap, int m_mode);
void	do_spool_discard(va_list *ap);
void	do_spool_replay	(va_list *ap);
void	sp_init		();
void	sp_send	(int action, char *sname, message *sp_msg);
void	sp_control_send(int entry, message *sp_msg);
#ifdef __cplusplus
}
#endif

#else

void	do_spool();
void	do_spool_and_discard();
void	do_spool_discard();
void	do_spool_replay	();
void	sp_init();
void	sp_send();
void	sp_control_send();

#endif
