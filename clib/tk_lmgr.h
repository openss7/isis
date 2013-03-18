/*  $RCSfile: tk_lmgr.h,v $ $Revision: 2.14 $ $Date: 90/06/24 14:50:07 $  */
/*
 *	Originally coded by Ken Kane
 *      Logging structures 
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

#define L_VERLEN	10		/* Length of log manager format version */
					/* number.  This value should remain */
					/* fixed accross all versions. */
#define L_VERSION	"LMF2.1\0\0\0\0"/* Log manager format version number. */
					/* This value should be updated only */
					/* after changes to the log file format. */
					/* Its length should always equal that */
					/* of L_VERLEN. */

#if ! ( __cplusplus || c_plusplus )
typedef struct req_nod  req_nod;
typedef struct log_nod  log_nod;
typedef struct log_hdr  log_hdr;
#endif

struct  req_nod                 /* Node for buffering logged messages */
{
        message *msg;           /* Request or log entry message */
        long tstamp;            /* Timestamp */
        int entry;              /* Message entry point */
        req_nod *prev, *next;   /* Previous and next nodes on write qnode */
};

struct  log_hdr                 /* Log header placed at start of every log */
{
        int     is_ckpt;        /* Is there a checkpoint in this log? */
        int     ckpt_blks;      /* Number of blocks in checkpoint */
        int     ckpt_len;       /* Space occupied by checkpoint */
	char	version[L_VERLEN+1]; /* Log file format version number */
};

struct  log_nod                 /* Logging information for each logged group */
{
        char    *fname;         /* Unique process/pgroup key */
        char    *l_name;        /* Name of file containing log */
        FILE    *l_fp;          /* Log file pointer */
        int     (*end_replay)();/* End of replay processing routine */
        int     replay_entry;   /* Entry point for manual replay */
        int     I_recovered;    /* Did this process recover this group? */
        int     flush_type;     /* Auto or manual flush */
        int     wri_off;        /* Offset to write new log entries */
        long    last_ts;        /* Last timestamp of a completed flush block */
        int     log_reqs;       /* Number of requests in stable log */
        int     req_q_len;      /* Number of requests on request qnode */
        int     next_req;       /* Next request number for replay */
        int     next_ts;        /* Timestamp of next request to replay */
        int     end_blk;        /* End of current block for replay */
	int	count_thresh;	/* Number of requests to buffer before flush */
	int	reqs_thresh;	/* Number of requests to log before checkpoint */
	int	len_thresh;	/* Size of log file at which to checkpoint */
	time_t	last_flush;	/* Time message buffer was last flushed */
	unsigned timer;		/* Freq. in secs to flush buffer.  0=no timer */
	int	inhibited;	/* Is checkpointing inhibited for this group? */
	int	cold_start;	/* Ignore contents of log file when beginning? */
        req_nod *req_q_hd;      /* Request qnode head */
        req_nod *req_q_tl;      /* Request qnode tail */
	log_nod *next;		/* Next log node */
	log_nod *prev;		/* Previous log node */
        log_hdr l_hdr;          /* Log header */
};

/* External variables for logging */

extern int log_in_replay;       /* Flag for testing if a process is in the
                                   middle of replaying its logs */
/*** Logging interface routines. ***/

#ifdef __STDC

int	log_checkpoint	(char *gname);
int	log_flush	(address *gaddr);
int	log_recovered	(address *gaddr);
int	log_write	(address *gaddr, message *msg);
int	log_disable_ckpt(char *gname);
int	log_enable_ckpt  (char *gname);

#else

extern int      log_checkpoint          ( /* char *gname */ );
extern int      log_flush               ( /* address gaddr */ );
extern int      log_recovered           ( /* address gaddr */ );
extern int      log_write               ( /* address gaddr, message *msg */ );
extern int	log_disable_ckpt	( /* char *gname */ );
extern int	log_enable_ckpt		( /* char *gname */ );

#endif

/*** Internal routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
int	log_action	(char *gname, int *new_incarn_p);
int	log_end_log_msg	();
int	log_has_ckpt	(char *gname);
log_nod *log_init(char *fname, char *gname, int replay_entno,
                  int flush_type, int (*end_of_replay)(void), 
		  int cold_start, int ignore_old);
int	log_cntthresh	(char *gname, int cnt);
int	log_reqsthresh	(char *gname, int cnt);
int	log_lenthresh	(char *gname, int cnt);
int	log_timer	(char *gname, unsigned freq);
int	log_remove	(char *gname);
int	log_replay_ckpt	(char *gname);
int	log_replay_msgs	();
int	log_start_recovery(char *gname);
int	log_write_msg	(ginfo *gip, message *msg, int entry);
void	logging_out	(message *msg);
int     log_cntthresh(char *gname, int nmbuf);
int     log_reqsthresh(char *gname, int nmdsk);
int     log_lenthresh(char *gname, int lflen);
int     log_timer(char *gname, unsigned timer);
#ifdef __cplusplus
}
#endif
#else 

extern log_nod  *log_init               ( /* char *fname, *gname, 
                                             int replay_entno, flush_type,
                                             int (*end_of_replay)() */ );
extern int	log_cntthresh		( /* char *gname, int cnt */ );
extern int	log_reqsthresh		( /* char *gname, int cnt */ );
extern int	log_lenthresh		( /* char *gname, int cnt */ );
extern int	log_timer		( /* char *gname, unsigned freq */ );
extern int      log_action              ( /* char *gname */ );
extern int      log_remove              ( /* char *gname */ );
extern int      log_start_recovery      ( /* char *gname */ );
extern int      log_has_ckpt            ( /* char *gname */ );
extern int      log_replay_ckpt         ( /* char *gname */ );
extern int      log_write_msg           ( /* ginfo *gip, message *msg,
                                             int entry */ );
extern void     logging_out             ( /* message *msg */ );
extern int      log_end_log_msg();
extern int      log_replay_msgs();
#endif

/* Constants used by the log manager and its applications */

#define L_NOCKPT        0       /* No checkpoint present in this log */
#define L_CKPT          1       /* Checkpoint present in this log */
#define L_MANUAL        0       /* Group is being logged manually */
#define L_AUTO          1       /* Group is being logged automatically */
#define L_WARM		0	/* Group should be started from log file */
#define L_ERROR		0	/* Error if log manager finds old format log */
#define L_COLD		1	/* Group should ignore contents of log file */
#define L_INIT          0       /* pg_join should initialize this group */
#define L_JOIN          1       /* pg_join should join, not create, this group */
#define L_RECOVER       2       /* pg_join should recover this group from log */
#define DEF_CNT_THRESH  8       /* Default number of requests to buffer before */
                                /* automatically flushing log of messages */
#define DEF_REQS_THRESH 250     /* Default number of requests to write in log before */
                                /* automatically forcing a checkpoint */
#define DEF_LEN_THRESH  1048576 /* Default log size at which checkpoint is taken */
#define DEF_TIMER	0	/* Default freq. in secs to flush log */
#define END_REPLAY      -1      /* End of replay marker */

/* Locations of log files and temporary files for log manager */

#define LOG_SUBDIR      "logs"
#define LOG_TTMPLT      "log_tempXXXXXX"
#define LOG_TSPEC       "log_temp*"
