/*  $RCSfile: wanClient.c,v $ $Revision: 2.4 $ $Date: 90/06/13 15:02:22 $  */
/*
 *  ISIS V2.0, export restrictions apply
 *
 *  Test the spooler and long-haul merged package.
 *
 * Coded by Mesaac Makpangou
 * Copyright 1990 ISIS Distributed Systems Inc.  Permission granted for
 * unrestricted use in commercial or research settings.  Rights to
 * develop derivative versions of this module reserved.
 */

#include   <stdio.h>
#include   <ctype.h>
#include   <string.h>
#include   "isis.h"
#include   "spooler.h"

#define    testGroupName "test_group"
#define    CHECKPOINT_MESS                   1
#define    LH_SPOOL_MESS                     2
#define    LH_CBCAST_MESS                    3
#define    LH_REMOTE_FILE_XFER               4
#define    SP_ID                             1
#define    DELAY                             500
#define    MAX_FILE_NAME                     128

void test_lh_spool(), test_lh_cbcast(), test_send_file(), printPrompt();
void interactive_test();
int currentMessageID;
int clientID;
char *pathName, *destCluster;
int std_input_wid;

static condition for_ever_and_ever = 0;

do_nothing_at_all()
{
	t_wait_l(&for_ever_and_ever, "main loop hanging");
}

void
main(argc, argv)
	int argc;
	char **argv;
{
	char type[20];
	int portNumber = -1;
	char *argument, c;

	if (argc != 2)
		goto usage;
	argument = *++argv;
	c = *argument;
	if (isdigit(c))
		portNumber = atoi(*argv);
	else {
	      usage:
		panic("\n Usage: wanClient port_number\n");
	}
	clientID = getpid();
	currentMessageID = 0;
	isis_init(portNumber);
	isis_task(test_lh_cbcast, "TEST_LHCBCAST_TASK");
	isis_task(test_lh_spool, "TEST_LHSPOOL_TASK");
	isis_task(test_send_file, "SEND_FILE_TASK");
	isis_task(interactive_test, "INTERACTIVE_TEST_TASK");
	printPrompt('c');
	std_input_wid = isis_input(0, interactive_test, NULLARG);
	isis_mainloop(do_nothing_at_all, 0);
}

/* If delai is non-zero, a new lh_msg is spooled every delai millisecondes */
void
test_lh_spool(delai)
	int delai;
{
	spool(testGroupName, LH_SPOOL_MESS, "%d%d%s%d%s", 3, clientID, testGroupName,
	      ++currentMessageID, "key", SP_NETWORK, destCluster, SP_ID, currentMessageID,
	      SP_KEYWORDS, "key", 0, 0);
	if (delai)
		isis_timeout(delai, test_lh_spool, (VOID *) delai, NULLARG);
}

/* If delai is non-zero, a new lh_msg is cbcasted every delai millisecondes */
void
test_lh_cbcast(delai)
	int delai;
{
	char clientHost[40];
	message *msg;

	gethostname(clientHost, 40);
	msg = msg_gen("%d%d%d%s%s", delai, ++currentMessageID, clientID, clientHost, testGroupName);
	lh_cbcast_m(testGroupName, LH_CBCAST_MESS, destCluster, msg);
	if (delai)
		isis_timeout(delai, test_lh_cbcast, (VOID *) delai, NULLARG);
}

/* factor is used to computed the fragment size during lh_xfer perf mesurement */
void
test_send_file(factor)
{
	char clientHost[40];
	message *msg;

	gethostname(clientHost, 40);
	msg =
	    msg_gen("%d%d%d%s%s", factor, ++currentMessageID, clientID, clientHost, testGroupName);
	lh_file_move(testGroupName, LH_REMOTE_FILE_XFER, destCluster, msg, pathName);
}

void
test_copy_file(factor)
{
	char clientHost[40];
	message *msg;

	gethostname(clientHost, 40);
	msg =
	    msg_gen("%d%d%d%s%s", factor, ++currentMessageID, clientID, clientHost, testGroupName);
	lh_file_copy(testGroupName, LH_REMOTE_FILE_XFER, destCluster, msg, pathName);
}

void
printPrompt(role)
	char role;
{
	if (role == 'c')
		printf("wanClient: (c,s,m,x,h,q,d) ");
	else
		printf("[WanServer]: ");
	fflush(stdout);
}

void
clhelp()
{
	printf("c: lh_cbcast\n");
	printf("s: lh_spool\n");
	printf("m: move a file\n");
	printf("x: copy a file\n");
	printf("h: this help\n");
	printf("q: quit\n");
	printf("d: set default transfer directories\n");
}

void
interactive_test()
{
	int fragment = 1;
	char buf[50];
	char interrupt[10];

	isis_wait_cancel(std_input_wid);
      yourChoice:
	scanf("%s", interrupt);
	if (strlen(interrupt) > 1) {
		printf("******** Invalid code, try again ********\n");
		printPrompt('c');
		goto yourChoice;
	}
	if (isalpha(interrupt[0])) {
		switch (interrupt[0]) {
		case 'q':
			exit(0);
			break;
		case 'h':
			clhelp();
			printPrompt('c');
			goto yourChoice;
			break;
		case 'd':
			pathName = (char *) calloc(1, MAX_FILE_NAME);
			printf("Src dir (absolute): ");
			scanf("%s", buf);
			lh_file_xfer_srcdir(buf);
			printf("Dst dir (absolute): ");
			scanf("%s", buf);
			lh_file_xfer_dstdir(buf);
			break;
		case 'm':
		case 'x':
			pathName = (char *) calloc(1, MAX_FILE_NAME);
			printf("File Name: ");
			scanf("%s", pathName);
		case 'c':
		case 's':
			destCluster = (char *) calloc(1, PG_GLEN);
			printf("destination cluster: [all | local | specific]: ");
			scanf("%s", destCluster);
			switch (interrupt[0]) {
			case 'm':
			case 'x':
				printf("long haul fragment size (kbytes): ");
				scanf("%d", &fragment);
				if (interrupt[0] == 'm')
					t_fork(test_send_file, (VOID *) fragment);
				else
					t_fork(test_copy_file, (VOID *) fragment);
				break;
			case 'c':
				t_fork(test_lh_cbcast, 0);
				break;
			case 's':
				t_fork(test_lh_spool, 0);
				break;
			}
			break;
		default:
			printf("******** Invalid code, try again ********\n");
			goto yourChoice;
			break;
		}
	}
	printPrompt('c');
	std_input_wid = isis_input(0, interactive_test, NULLARG);
}
