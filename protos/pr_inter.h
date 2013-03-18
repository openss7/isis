/*  $RCSfile: pr_inter.h,v $ $Revision: 2.19 $ $Date: 90/08/13 16:07:16 $  */
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
 *
 *
 */
#ifndef PR_INTERSITE
#define PR_INTERSITE
/*
 *      Intersite message data structure, used mostly by pr_udp.h
 */

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

/* Other limits (can be varied) and constants (should probably not be changed) */

#define SMLPKT          64	/* Packets are rarely smaller than this */
#define MAXMSGS         128	/* Don't piggyback more than 128 messages per packet */
#define PK_HIWAT       16384	/* Block when unacked bytes in window reaches this level */
#define PK_LOWAT        4096	/* Restart when drains to this level */
#define MAX_IOVLEN      16	/* System limit on iovec lengths */
#define MAX_UDP_IOVLEN  4	/* Limit for UDP, which has problems on SUNs */
#define RCVSIZE         32*1024+32	/* Receive buffer size */
#define SNDSIZE         10*1024+32	/* Send buffer size */

struct packet {
	short pk_size;			/* Length of packet in bytes */
	char pk_state;			/* State flags */
	char pk_cnt;			/* Counts messages in this message */
	short pk_nret;			/* Number of retransmissions */
	long pk_rtwhen;			/* Retransmit when we reach this time */
	long pk_whensent;		/* Value of ``now'' when packet was sent */
	message *pk_packet;		/* The packet itself */
	qnode *pk_contents;		/* Messages IN this packet */
};

/* pk_state bits */
#define PK_INUSE        0x0001	/* Contains packet */
#define PK_SENT         0x0002	/* Contains packet */
#define PK_ACKED        0x0004	/* Set if acked */

#include "pr_intersite.h"

/* Describes a message on a congestion qnode */
struct mdesc {
	message *md_msg;		/* pointer to the message */
	int (*md_cb) ();		/* Callback routine */
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
	long io_nslots;			/* Number of free slots in output window */
	long io_nbytes;			/* Number of pending output bytes */
	long io_rtdelay;		/* Delay for packet retransmission */
	long io_backed;			/* Number of backlogged bytes */
	long io_ndups;			/* Number of duplicate copies received */
	long io_nacks;			/* Number of acks received */
	long io_ndata;			/* Number of data packets received */
	long io_nmsgs;			/* Number of messages received */
	long io_nsent;			/* Number of messages sent */
	long io_nret;			/* Number of retransmissions */
	long io_abits;			/* Acknowledgement field, tied to WSIZE above */
	long io_lastin;			/* Time of last input */
	long io_lastout;		/* Time of last output */
	long io_wantack;		/* Number of unacked bytes for this ioq */
	saddr io_address;		/* Destination address */
	fraghdr io_fhdr;		/* Fragment header, if assembling a message */
	block_desc *io_blk_desc;	/* Corresponding block descriptor */
	char *io_fbody;			/* Buffer to assemble it in */
	char *io_fptr;			/* Pointer into buffer */
	site_id io_sid;			/* Current site id */
	site_id io_dead;		/* Dead site-id or null */
	qnode *io_backlog;		/* Backlog, if any */
	packet *io_accum;		/* Accumulating here */
	packet io_out[WSIZE];		/* Output packets and associated infomation */
	message *io_in[MAXWSIZE];	/* Receive packet */
};

/* io_state bits */
#define IO_INUSE        0x01	/* Queue allocation was done */
#define IO_ALIVE        0x02	/* This channel is alive */
#define IO_DEAD         0x04	/* This channel is dead */
#define IO_ESTAB        0x08	/* Connection established */
#define IO_ACCUM        0x10	/* Accumulating lazy packets */
#define IO_URGENT       0x20	/* Accumulating urgent packets */
#define IO_TOLDFD       0x40	/* I suspect he's dead */
#define IO_ASSEM        0x80	/* Reassembling a fragment */

/* Normal sequence numbers are 7-bit numbers.  Special values: */
#define SEQ_DEAD        0x81	/* "You are dead" */
#define SEQ_ACK         0x82	/* Acknowledgement only */

#define SEQ_SPCL        0x80	/* All special seq. numbers have 080 set */

qnode *IOQS;				/* IOQ entries in use */
char staging_buf[STAGEBUFSIZE];

#endif				/* PR_INTERSITE */
