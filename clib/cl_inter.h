/*  $RCSfile: cl_inter.h,v $ $Revision: 2.79 $ $Date: 90/08/14 10:46:25 $  */
/*
 *	Originally coded by Ken Birman
 *
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
#ifndef CL_INTERSITE
#define CL_INTERSITE

#ifdef  ISIS_SYS
# include <sys/ioctl.h>
# include <sys/socket.h>
#if(HPUX)
# include <time.h>
#else
# include <sys/time.h>
#endif
# include <netdb.h>
# include <signal.h>
# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <fcntl.h>
#endif				/* ISIS_SYS */

/*
 *      Configuration parameters. In principle, most can be changed for each
 *      ISIS site and values need not match up.  However, MAXWSIZE is
 *      assumed to be the same system-wide and is limited by the number of
 *      bits in a long integer
 */

/* Window sizes... output side can be varied but may not exceed maxwsize */
#define WSIZE           32	/* window size, power of 2, >= 2 */
#define MAXWSIZE        32	/* Largest wsize can ever be */
#define WMASK           (WSIZE-1)	/* For computing mod WSIZE */
#define MAXWMASK        (MAXWSIZE-1)	/* For computing mod MAXWSIZE */

#define SMLPKT          256	/* Packets are rarely smaller than this */
#define MAXMSGS         128	/* Don't piggyback more than 128 messages per iclpkt */
#define PK_HIWAT        (2*8192)	/* Block when bytes in window reaches this level */
#define PK_LOWAT        8192	/* Restart when drains to this level */
#define MAX_IOVLEN      16	/* System limit on iovec lengths */
#define MAX_UDP_IOVLEN  10	/* Limit for UDP, which has problems on SUNs */
#define RCVSIZE         32*1024+32	/* Receive buffer size */
#define SNDSIZE         10*1024+32	/* Send buffer size */

struct iclpkt {
	short pk_size;			/* Length of iclpkt in bytes */
	char pk_state;			/* State flags */
	char pk_cnt;			/* Counts messages in this message */
	short pk_nret;			/* Number of retransmissions */
	long pk_rtwhen;			/* Retransmit when we reach this time */
	long pk_whensent;		/* Value of ``now'' when iclpkt was sent */
	message *pk_iclpkt;		/* The iclpkt itself */
	qnode *pk_contents;		/* Messages IN this iclpkt */
};

/* pk_state bits */
#define PK_INUSE        0x0001	/* Contains iclpkt */
#define PK_SENT         0x0002	/* Contains iclpkt */
#define PK_ACKED        0x0004	/* Set if acked */

/* Intersite information, added to message */
struct interclient {
	address ic_from;		/* Sender */
	address ic_dest;		/* Dest */
	short ic_seqn;			/* Seqn for this iclpkt */
	short ic_aseqn;			/* Seqn acknowledged */
	int ic_abits;			/* Ack bits */
};

/* Describes a message on a congestion qnode */
struct mdesc {
	message *md_msg;		/* pointer to the message */
	void (*md_cb) ();		/* Callback routine */
	char *md_arg0;			/* Argument */
	char *md_arg1;			/* Argument */
};

/* Fragmented message description */
struct fraghdr {
	int fr_nfrags;			/* Number of fragments, not including stab */
	int fr_msglen;			/* Total length of data part */
};

struct ioq {
	char io_state;			/* A few state bits */
	char io_iseqn;			/* Last input I saw (initialize to 0) */
	char io_nseqn;			/* Next output seqn to use */
	char io_oseqn;			/* Oldest unacked output seqn */
	int io_nslots;			/* Number of free slots in output window */
	int io_nbytes;			/* Number of pending output bytes */
	int io_rtdelay;			/* Delay for iclpkt retransmission */
	int io_backed;			/* Number of backlogged bytes */
	int io_ndups;			/* Number of duplicate copies received */
	int io_nacks;			/* Number of acks received */
	int io_ndata;			/* Number of data iclpkts received */
	int io_nmsgs;			/* Number of messages received */
	int io_nsent;			/* Number of messages sent */
	int io_nret;			/* Number of retransmissions */
	int io_abits;			/* Acknowledgement field, tied to WSIZE above */
	int io_lastin;			/* Time of last input */
	int io_lastout;			/* Time of last output */
	int io_wantack;			/* Bytes that want to be acked */
	int io_probefreq;		/* Probe frequency if non-zero */
	int io_probetimeout;		/* Probe timeout */
	saddr io_address;		/* Destination address */
	fraghdr io_fhdr;		/* Fragment header, if assembling a message */
	block_desc *io_blk_desc;	/* Corresponding block descriptor */
	char *io_fbody;			/* Buffer to assemble it in */
	char *io_fptr;			/* Pointer into buffer */
	address io_claddr;		/* Current client address */
	qnode *io_backlog;		/* Backlog, if any */
	iclpkt *io_accum;		/* Accumulating here */
	iclpkt io_out[WSIZE];		/* Output iclpkts and associated infomation */
	message *io_in[MAXWSIZE];	/* Receive iclpkt */
	qnode *io_wantflush;		/* Flush wanted from... */
	qnode *io_spool;		/* Spool, for use when input is inhibited due to flush */
	qnode *io_reflist;		/* List of groups for which ``opened'' */
};

/* io_state bits */
#define IO_INUSE        0x01	/* Queue allocation was done */
#define IO_ALIVE        0x02	/* This channel is alive */
#define IO_DEAD         0x04	/* This channel is dead */
#define IO_ESTAB        0x08	/* Connection established */
#define IO_ACCUM        0x10	/* Accumulating lazy iclpkts */
#define IO_URGENT       0x20	/* Accumulating urgent iclpkts */
#define IO_ASSEM        0x80	/* Reassembling a fragment */

/* Normal sequence numbers are 7-bit numbers.  Special values: */
#define SEQ_ACK               0x80	/* Acknowledgement only */

extern char staging_buf[MAXMSGLEN];

/* Tank is a list of input messages to be processed */
#define tankstats 1

#define TANK_EMPTY      0
#define TANK_ATEONE     1
#define TANK_OTHER      2
#define TANK_FULL       3

#if ! ( __cplusplus || c_plusplus )
typedef struct msg_tank msg_tank;
#endif

struct msg_tank {
	qnode *head;			/* Queue (FIFO) of pending messages. */
	long max_size;			/* Bound on number of bytes of stored messages. */
	long size;			/* Current number of bytes of stored messages. */
#ifdef tankstats
	long max_bytes_used;		/* Maximum bytes ever used. */
	long nr_msgs1;
	long nr_msgs2;
#endif					/* tankstats */
};

int tankcount[MAX_SITES];		/* Number from each sender */

extern char hknown[MAX_SITES];
extern saddr addr_by_site[MAX_SITES];

#define default_max_size 50000	/* bytes. */

/*** Internal routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	saddr *get_addr_by_address(address * addr, int portno);
	void dump_interclient();
	int intercl_deliver();
	int intercl_do_input();
	void intercl_drain_any_input();
	void intercl_gotmsg(ioq * ioqp, message * mp, int pn);
	void intercl_init();
	void intercl_died(qnode * qp);
	void intercl_newview(int first_view, groupview * gv);
	int intercl_trysend(address * paddr, message * mp);
	int intercl_quiet();
	void net_send(address * gaddr, int exmode, message * mp, address * to,
		      void (*callback) (address * to, VOID * carg0, VOID * carg1),
		      VOID * arg0, VOID * arg1);
	void isis_receipt(message * mp, address * from, int pn);
	void intercl_isalive(address * gaddr, address * paddr, saddr * physaddr);
	void intercl_sweep();
	void intercl_unblock(groupview * bv, address * gaddr);
	int intercl_wantflush(address * addr, address * gaddr, int viewid);
	void isis_sendmsg(int sock, saddr * dst, int dlen, message * mp);
	void io_free(ioq * ioqp);
	void lazy_flush();
	int peek_ack(interclient * ic);
	message *udp_rpc(int fdes, char *name, saddr * addr, int sid, int port, message * mp);
#ifdef __cplusplus
}
#endif
#else				/* FUN_TYPES */

saddr *get_addr_by_address();
void intercl_sweep(), lazy_flush();
void net_send();
void intercl_gotmsg();
void intercl_drain_any_input();
void isis_sendmsg();
message *udp_rpc();

#endif				/* FUN_TYPES */

#endif				/* CL_INTERSITE */
