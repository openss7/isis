/*  $RCSfile: tk_xaction.h,v $ $Revision: 2.14 $ $Date: 90/06/24 14:49:57 $  */
/* cl_xaction.h, By Robert Cooper */
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
/* The x_id struct is defined in cl_typedefs.h */

#define xid_cmp(id1, id2) addr_cmp(id1, id2)
  /* Returns 0 if id1 and id2 are equal,
           > 0 if id1 is "greater than" id2,
       and < 0 if id1 is "less than" id2.
     x_id_cmp is NOT A SERIALIZATION ORDERING on transactions!!!
     It's intended for equality tests, and as a key in sorting and table
     lookup.
  */

/* Of the following defines, only X_PREPARE, X_COMMIT, and X_ABORT are seen by
   client programmers. The rest are used internally by the transaction tool. */
#define X_COMPUTING -1/* Transaction in progress (i.e. after x_begin but before
                        x_commit or x_abort). */
#define X_PREPARE   0 /* In phase 1 of a 2 phase commit. */
#define X_COMMIT    1 /* Committed, but not necessarily all participants
                        know this yet. */
#define X_ABORT     2 /* Aborted, but not necessarily all participants know yet. */
#define X_FINISHED  3 /* All participants know the outcome, so we can
                         delete information about this transaction. */

/* An x_item describes the eventual outcome of a transaction to a participant
   that failed during transaction termination. */
typedef struct {
    x_id id; 
    int outcome;    /* One of X_COMMIT or X_ABORT. */
    message *info;  /* Participant supplied data. */
} x_item;

/* An x_list is returned by the x_outcomes routine and contains x_items for
   every transaction for which the participant may not have been informed
   of the outcome. */
typedef struct {
    int len;
    x_item items[1];
} x_list;

#define xlog_subdir "translogs"   /* Subdirectory for transaction logs. */
#define xmgr_service "XMGR-service" /* Used for group name and log file name. */
#define XR_SAVE_OUTCOME 1        /* Messages handled by recovery manager. */
#define XR_GET_OUTCOME  2

#define LR_CHECK_DONE    1	 /* Sent by xmgr to check if lmgr is done, accelerates startup */

/*** Transaction interface routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif

x_id *x_getid(); 
int x_begin();
int x_commit(int phases);
int x_abort();
int x_term_msg(char *participant_name,
               bool (*on_prepare)(x_id *id),
               bool (*on_commit)(x_id *id),
               bool (*on_abort)(x_id *id),
               message *data);
x_list *x_outcomes(char *part_name);
void x_outcomes_flush (char *part_name, x_list *outcomes);
#if ( __cplusplus || c_plusplus )
int x_term( char *participant_name,
            ifunc *on_prepare, ifunc *on_commit, ifunc *on_abort,
            char *fmt ... );
#else
int x_term();
#endif

#ifdef __cplusplus
}
#endif

#else

x_id *x_getid(); 
int x_begin();
int x_commit();
int x_abort();
int x_term();
int x_term_msg();
x_list *x_outcomes();
void x_outcomes_flush ();

#endif
/*** Internal routines ***/

void x_init();
void dump_trans();

