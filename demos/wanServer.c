/*  $RCSfile: wanServer.c,v $ $Revision: 2.4 $ $Date: 90/06/13 15:02:13 $  */
/*
 *  ISIS V2.0, export restrictions apply
 *
 *  Test the spooler and long-haul package.
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
#define    SRC_ERR                           5
#define    DST_ERR                           6

address *testGaddr;
void replayAll(), rcvMess(), rcv_lh_cbcast(), rcv_remote_file(),
  src_err(), dst_err();


void
main(argc, argv)
     int argc;
     char **argv;
{
  char *argument, c;
  int portNumber = -1;

  if (argc != 2)
    goto usage;
  argument = *++argv;
  c = *argument;
  if (isdigit(c))
    portNumber = atoi(*argv);
  else
    {
    usage:
      panic("\n Usage: wanServer port_number ");
    }
  isis_init(portNumber);
  isis_entry(LH_SPOOL_MESS, rcvMess, "rcvSimpleMess");
  isis_entry(LH_CBCAST_MESS, rcv_lh_cbcast, "rcv_lh_cbcast");
  isis_entry(LH_REMOTE_FILE_XFER, rcv_remote_file, "rcv_remote_file");
  isis_entry(SRC_ERR, src_err, "src_err");
  isis_entry(DST_ERR, dst_err, "dst_err");
  lh_file_srcerr_handler(testGroupName, SRC_ERR);
  lh_file_dsterr_handler(testGroupName, DST_ERR);
  isis_task(replayAll, "init_replay_of_all");
  isis_mainloop(replayAll, NULLARG);
}
  
void
replayAll()
{
  testGaddr = pg_join(testGroupName, 0);
  if (addr_isnull(testGaddr))
    panic("Join of TestGroup failed \n");
  wset_join(testGroupName);
  spool_replay(testGroupName, 0);
  printf("Wan server ready\n");
}

void
rcvMess(m)
     message *m;
{
  int sp_type, messSeqn, messSpSeqn, clID;
  char dest[40], key[40];

  messSpSeqn = spool_getseqn(m);
  msg_get(m, "%d%d%s", &sp_type, &clID, dest);
  printf("Spooled message: dest <%s>; client ID: <%d>; spool sequence <%d> \n", dest, clID, messSpSeqn);
  switch(sp_type)
    {
    default:
      break;
    case 1:
      msg_get(m, "%d", &messSeqn);
      printf("Message spooled with SP_ID: %d\n", messSeqn);
      break;
    case 2:
      msg_get(m, "%s", key);
      printf("Message spooled with KEYWORD %s\n", key); 
      break;
    case 3:
      msg_get(m, "%d%s", &messSeqn, key);
      printf("Message spooled with SP_ID %d and KEWORD %s \n", messSeqn, key);
      break;
    }
}

void
rcv_remote_file(m)
     message *m;
{
  int messID, clID;
  char *clientHost, *holder, *lname;
  int ch_size;

  msg_get(m, "%d%d%d%-s", &ch_size, &messID, &clID, &clientHost);
  get_fname_and_holder(m,lname,holder);
  printf("Received file <%s@%s>\n", lname, holder);
  printf("File sender = <%d@%s> ; fragment size used was %d\n", clID, clientHost, ch_size);
}

void src_err(m)
     message * m;
{
  char *srcnam, *dstname, *dstnet;
  int error;
  lh_get_error_report(m,srcnam,dstname,dstnet,error);
  printf("transfer of %s to %s at %s failed with error %d\n",
	srcnam, dstname, dstnet, error); 
}

void dst_err(m)
     message * m;
{
  char *srcnam, *dstname, *srcnet;
  int error;
  lh_get_error_report(m,srcnam,dstname,srcnet,error);
  msg_get(m, "%-s%-s%-s%d", srcnam, dstname, srcnet, error);
  printf("transfer of %s to %s from %s failed with error %d\n",
	srcnam, dstname, srcnet, error); 
}

void
rcv_lh_cbcast(m)
     message *m;
{
  char *appliName, *clientHost;
  int messID, clID, delai;

  msg_get(m, "%d%d%d%-s%-s", &delai, &messID, &clID, &clientHost, &appliName);
  printf("Received lh_cbcast from <%d@%s>, messageID: <%d>]\n", clID, clientHost, messID);
}


