/*  $RCSfile: pr_msgtank.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:01 $  */
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
 */
/* A message tank is a bounded qnode of pending messages. The bound is
   on the total bytes of message storage. */

#define tankstats 1

#define TANK_EMPTY      0
#define TANK_ATEONE     1
#define TANK_OTHER      2
#define TANK_FULL       3

typedef struct msg_tank {
	qnode *head;			/* Queue (FIFO) of pending messages. */
	long max_size;			/* Bound on number of bytes of stored messages. */
	long size;			/* Current number of bytes of stored messages. */
#ifdef tankstats
	long max_bytes_used;		/* Maximum bytes ever used. */
	long nr_msgs1;
	long nr_msgs2;
#endif					/* tankstats */
} msg_tank;

int tankcount[MAX_SITES];		/* Number from each sender */

#define default_max_size 50000	/* bytes. */

msg_tank *msg_tank_create();
extern int msg_tank_enqueue();
message *msg_tank_dequeue();
extern int msg_tank_set_max();
extern int msg_tank_print();
