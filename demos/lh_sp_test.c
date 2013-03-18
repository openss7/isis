/*  $RCSfile: lh_sp_test.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:17 $  */
/*
 *  Test the spooler and long-haul merged packadge.
 *  This program requires the specification of the ISIS application port number.
 *  To test long-haul spooling, a file giving the list of networks is also required (-n option).
 *  You could specify the local network name using -l option.
 *
 *  Coded by Mesaac Makpangou
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

#include   <stdio.h>
#include   <ctype.h>
#include   <string.h>
#include   "isis.h"
#include   "spooler.h"

#define    MAXTESTGROUPS                     5   /* maximum number of groups allowed for this test */
                                      
#define    RCV_SIMPLE_MESS                   1
#define    RCV_MESS_WITH_SEQN                2
#define    RCV_MESS_WITH_KEY                 3
#define    RCV_MESS_WITH_KEY_AND_SEQ         4
#define    CHECKPOINT                        5
#define    SP_ID                             1   /* Example of numeric SP_OPTION */

/* Structure describing the state of replay of a specific group */
struct groupComState
{
  char name[40];              /* Group name */
  address *gaddr;             /* Group address */
  int messID;                 /* Message (user-defined) identifier */
  int spseqn;                 /* Spool sequence number */
};
struct groupComState groupList[MAXTESTGROUPS];
int currentMessageID;

/* Necessary only when using the long_haul package */
#define    MAXNETS                           5
struct netConDesc
{
  char name[40];
  int messID;
};
struct netConDesc *netList;
int numberOfNets;
int thisNetIndex;

void *calloc();
void printPrompt(), printNetView(), printGroupViews(); 

/**********************************************************************
 *  Client procedures
 *********************************************************************/
void
sendSimpleMess(sender, countptr, nb, groupName, net)
     char *groupName, *net, *sender;
     int *countptr, nb;
{
  int val;
  while (nb)
    {
      val = ++*countptr;
      if (net != (char*) 0)
	spool(groupName, RCV_SIMPLE_MESS, "%d%s%s", 0, sender, groupName,
	      SP_NETWORK, net, 0);
      else
	spool(groupName, RCV_SIMPLE_MESS, "%d%s%s", 0, sender, groupName, 0);
      --nb;
    }
}

void
sendNumberedMess(sender, countptr, nb, groupName, net)
     char *groupName, *net, *sender;
     int *countptr, nb;
{
  int val;
  while (nb)
    {
      val = ++*countptr;
      if (net != (char*) 0)
	spool(groupName, RCV_MESS_WITH_SEQN, "%d%s%s%d", 1, sender, groupName, val,
	    SP_NETWORK, net, SP_ID, val, 0);
      else
	spool(groupName, RCV_MESS_WITH_SEQN, "%d%s%s%d", 1, sender, groupName, val,
	      SP_ID, val, 0);
      --nb;
    }
}

void
sendKeyedMess(sender, countptr, nb, groupName, net, key)
     char *groupName, *sender, *net, *key;
     int *countptr, nb;
{
  int val;
  while (nb)
    {
      val = ++*countptr;
      if (net != (char*) 0)
	spool(groupName, RCV_MESS_WITH_SEQN, "%d%s%s%s", 2, sender, groupName, key,
	    SP_NETWORK, net, SP_KEYWORDS, key, 0, 0);
      else
	spool(groupName, RCV_MESS_WITH_SEQN, "%d%s%s%s", 2, sender, groupName, key,
	      SP_KEYWORDS, key, 0, 0);
      --nb;
    }
}

void
send_K_N_mess(sender, countptr, nb, groupName, net, key)
     char *groupName, *net, *key, *sender;
     int *countptr, nb;
{
  int val;
  while (nb)
    {
      val = ++*countptr;
      if (net != (char*) 0)
	spool(groupName, RCV_MESS_WITH_SEQN, "%d%s%s%d%s", 3, sender, groupName, val, key,
	    SP_NETWORK, net, SP_ID, val, SP_KEYWORDS, key, 0, 0);
      else
	spool(groupName, RCV_MESS_WITH_SEQN, "%d%s%s%d%s", 3, sender, groupName, val, key,
	      SP_ID, val, SP_KEYWORDS, key, 0, 0);
      --nb;
    }
}

void
clMenu()
{
  printf("\n\n");
  printf("     h: Shows the client menu \n");
  printf("     q: quit \n");
  printf("     p: Prints the net view \n");
  printf("\n");
  printf("     1: Sends with no SP_OPTION \n");
  printf("     2: Sends with SP_ID \n");
  printf("     3: Sends SP_KEYWORDS \n");
  printf("     4: conjonction of 2 and 3 \n");
}

void
clientMenu()
{
  char *net, *sender;
  char key[40];
  char group[40];
  char destNet[40];
  int i, type_test, nb, *countptr;
  char interrupt[10];
  nb = 10;
  clMenu();

 yourChoice:
  printPrompt('c');
  scanf("%s", interrupt);
  if (strlen(interrupt) > 1)
    {
      printf("******** Invalid code, try again ********\n");
      goto yourChoice;
    }
  if (isalpha(interrupt[0]))
    switch(interrupt[0])
      {
      case 'q':
	exit(0);
	break;
      case 'p':
	printNetView();
	break;
      case 'h':
	clMenu();
	goto yourChoice;
	break; 
      default:
	printf("******** Invalid code, try again ********\n");
	goto yourChoice;
	break;
      }
  else
    {
      type_test = atoi(interrupt);
      
      /* Setting the destinator network name and the appropriate message counter */
      if(netList != (struct netConDesc*) 0)
	{
	enterNet:
	  printf("\n destnator's network name: ? ");
	  scanf("%s", destNet);
	  for (i = 0; ((i < MAXNETS) && strcmp(netList[i].name, destNet)); i++);
	  if (i == MAXNETS)
	    {
	      printf("unknown network; try aigain \n");
	      goto enterNet;
	    }
	  else
	    {
	      net = netList[i].name;
	      countptr = &netList[i].messID;
	      if (thisNetIndex != -1)
		sender = netList[thisNetIndex].name;
	      else
		sender = "anonymousNet";
	    }
	}
      else
	{
	  net = (char*) 0;
	  countptr = &currentMessageID;
	  sender = "localhost";
	}
      
      /* Setting the destinator group */
      printf("\n Destinator group name: ? ");
      scanf("%s", group);
      
      /* Switching to the appropriate action */
      switch(type_test)
	{
	default:
	  printf("******** Invalid code, try again ********\n");
	  break;
	case 1:
	  sendSimpleMess(sender, countptr, nb, group, net);
	  break;
	case 2:
	  sendNumberedMess(sender, countptr, nb, group, net);
	  break;
	case 3:
	  printf("Select the key ?");
	  scanf("%s", key);
	  sendKeyedMess(sender, countptr, nb, group, net, key);
	  break;
	case 4:
	  printf("Select the key ?");
	  scanf("%s", key);
	  send_K_N_mess(sender, countptr, nb, group, net, key);
	  break;
	}
    }
  goto yourChoice;
}

/******************************************************************************
 * Server procedures
 ******************************************************************************/
void
rcvMess(m)
     message *m;
{
  int i, sp_type, messSeqn, messSpSeqn;
  char source[40], dest[40], key[40];

  sp_type = 0; /* Provisoire */

  /* Getting the message components */
  messSpSeqn = spool_getseqn(m);
  msg_get(m, "%d%s%s", &sp_type, source, dest);

  /* Updating the networks view */
  if (netList != (struct netConDesc*) 0)
    {
      for (i = 0; ((i < numberOfNets) && !strcmp(netList[i].name, source)); i++);
      if (i < MAXNETS)
	{
	  if (i == numberOfNets)
	    {
	      printf("%s yet known in this network. We add it \n", source);
	      strcpy(netList[numberOfNets].name, source);
	      netList[numberOfNets].messID = messSeqn;
	      numberOfNets++;
	    }
	  if (netList[i].messID < messSeqn)
	    netList[i].messID = messSeqn;
	}
      else
	printf("Too much networks involved; no more ressource. Message ignored \n");
    }

  /* Updating the group view */
  for (i = 0; ((i < MAXTESTGROUPS) && strcmp(groupList[i].name, dest)); i++);
  if (i < MAXTESTGROUPS)
    {
      if (groupList[i].spseqn < messSpSeqn)
	groupList[i].spseqn = messSpSeqn;
    }
  else
    panic("Unpredictable event occurs");

  /* Show the content of the message */
  printf("spsegn: %d, type: %d, source: %s, dest: %s \n",  messSpSeqn, sp_type, source, dest);
  switch(sp_type)
    {
    default:
      break;
    case 1:
      msg_get(m, "%d", &messSeqn);
      groupList[i].messID = messSeqn;
      printf("Message spooled with SP_ID: %d \n", messSeqn);
      break;
    case 2:
      msg_get(m, "%s", key);
      printf("Message spooled with KEYWORD %s \n", key); 
      break;
    case 3:
      msg_get(m, "%d%s", &messSeqn, key);
      groupList[i].messID = messSeqn;
      printf("Message spooled with SP_ID %d and KEWORD %s \n", messSeqn, key);
      break;
    }
  printPrompt('s');

}
  
void
replayAll(gid)
     int gid;
{
  spool_replay(groupList[gid].name, 0);
}

void
replayKeyedMess(gid, key)
     int gid;
     char *key;
{
  spool_replay(groupList[gid].name, SP_KEYWORDS, key, 0, 0);

}

void
replaySelectedMess(gid, lower, upper)
    int gid, lower, upper;
{
  spool_replay(groupList[gid].name, SP_ID, lower, upper, 0);

}

void
replayKeyedSelectedMess(gid, lowerBound, upperBound, key)
     int gid, lowerBound, upperBound;
     char *key;
{
  spool_replay(groupList[gid].name, SP_ID, lowerBound, upperBound, SP_KEYWORDS, key, 0, 0);
}

void
rcvCheckpoint(m)
     message *m;
{
  char checkmess[80];
  int spseqn;
  msg_get(m, "%s%d", checkmess, &spseqn);
  printf("... checkpoint message...:  %s, spseqn: %d \n", checkmess, spseqn);
  printPrompt('s');
}

/* Discards all messages and appends a checkpoint message */
void
checkpoint(gid)
      int gid;
{
  spool_set_replay_pointer(groupList[gid].name, groupList[gid].spseqn);
  spool_and_discard(groupList[gid].name, CHECKPOINT, "%s%d", "complete checkpoint: ",
		    groupList[gid].spseqn, SP_KEYWORDS, "CHECKPOINT", 0, 0);
}

/* Discards all messages with SP_ID < messID, then appends a checkpoint message */
void
selected_check(gid, messID)
      int gid, messID;
{
  spool_set_replay_pointer(groupList[gid].name, groupList[gid].spseqn);
  spool_and_discard(groupList[gid].name, CHECKPOINT, "%s%d%d", "selected checkpoint: ",
		    groupList[gid].spseqn, messID, SP_KEYWORDS, "CHECKPOINT", 0, 0,
		    SP_ID, SP_INFINITY, messID, 0);
}

/* Discards all messages with key "key", then appends a checkpoint message */
void
keyed_check(gid, key)
     int gid;
     char *key;
{
  spool_set_replay_pointer(groupList[gid].name, groupList[gid].spseqn);
  spool_and_discard(groupList[gid].name, CHECKPOINT, "%s%d%s","keyed checkpoint: ",
		    groupList[gid].spseqn, key, SP_KEYWORDS, "CHECKPOINT", key, 0, 0,
		    SP_KEYWORDS, key, 0, 0);
}

/* Conjonction of the two previous primitives */
void
keyed_selected_check(gid, messID, key)
     int gid, messID;
     char *key;
{
  spool_set_replay_pointer(groupList[gid].name, groupList[gid].spseqn);
  spool_and_discard(groupList[gid].name, CHECKPOINT, "%s%d%d%s", "keyed and selected checpoint: ",
		    groupList[gid].spseqn, messID, key, SP_KEYWORDS, "CHECKPOINT", key, 0, 0,
		    SP_KEYWORDS, key, 0, SP_ID, SP_INFINITY, messID, 0);
}

int initPhase;
void
svMenu()
{
  printf("     h: Shows the server menu \n");
  printf("     p: Shows current communication state \n");
  printf("     d: Desables play_through \n");
  printf("     e: Re-enable play_through \n");
  printf("     q: quit \n");
  printf("\n");
  printf("     0: Set replay pointer \n");
  printf("     1: Request replay of all messages \n");
  printf("     2: Request replay of messages matching SP_ID bounds \n");
  printf("     3: Request replay of messages with the appropiate keyword \n");
  printf("     4: conjonction of 2 and 3 \n");
  printf("\n");
  printf("     5: Discards all messages \n");
  printf("     6: Discards all messages with only SP_ID option  \n");
  printf("     7: Discards messages with appropriate key \n");
  printf("     8: Conjonction of 6 and 7  \n");
  printf("     9: Change checkpoint pointer \n");
}

void
serverMenu()
{
  char interruptStr[20];
  char group[40];
  char key[40];
  int  i, type_test, gid, ptrVal;
  char car;

  if (initPhase)
    {
      svMenu();
      initPhase = 0;
      printPrompt('s');
    }
  else
    {
      isis_input(0, NULLROUTINE, NULLARG);
    }
 selection:
  scanf("%s", interruptStr);
  if (strlen(interruptStr) > 1)
    {
      printf(" ************** Invalid code : try again  ***********: \n\n");
      svMenu();
      printPrompt('s');
      goto selection;
    }
  car = interruptStr[0];
  if(isalpha(car))
    switch(car)
      {
      case 'q':
	printGroupViews();
	for (i = 0; i < MAXTESTGROUPS; i++)
	  if (groupList[i].spseqn != 0)
	    {
	      printf("Setting replay pointer of spool: ** %s ** \n", groupList[i].name);
	      spool_set_replay_pointer(groupList[i].name, groupList[i].spseqn);
	      spool_play_through(group, SP_OFF);
	    }
	exit(0);
	break;
      case 'p':
	printNetView();
	printf("\n");
	printGroupViews();
	printPrompt('s');
	break;
      case 'h':
	svMenu();
	printPrompt('s');
	goto selection;
	break;
      case 'd':
	printf("Group name: ? ");
	scanf("%s", group);
	spool_play_through(group, SP_OFF);
	printPrompt('s');
	break;
      case 'e':
	printf("Group name: ? ");
	scanf("%s", group);
	spool_play_through(group, SP_ON);
	printPrompt('s');
	break;
      default:
	printf(" ************** Invalid code : try again  ***********: \n\n");
	svMenu();
	printPrompt('s');
	goto selection;
	break;
      }
  else if (isdigit(car))
    {
      type_test = atoi(&car);

      printf("Group name: ? ");
      scanf("%s", group);

      gid = -1;
      for (i = 0; ((i < MAXTESTGROUPS) && strcmp(groupList[i].name, group)); i++)
	if (addr_isnull(groupList[i].gaddr) && (gid == -1))
	  gid = i;
      if (i < MAXTESTGROUPS)
	gid = i;
      else
	{
	  if ((i == MAXTESTGROUPS) && (gid == -1))
	    panic("no available ressource for the new test group");
	  if (i == MAXTESTGROUPS)
	    {
	      groupList[gid].gaddr = pg_join(group, 0);
	      if (addr_isnull(groupList[gid].gaddr))
		panic("join group %s fails \n", group);
	      else 
		{
		  strcpy(groupList[gid].name, group);
		  groupList[gid].spseqn = 0;
		}
	    }
	}

      switch(type_test)
	{
	default:
	  printf(" ************** Invalid code : try again  ***********: \n\n");
	  svMenu();
	  printPrompt('s');
	  goto selection;
	  break;
	case 0:
	  printf("Setting replay pointer of spool: ** %s ** \n", groupList[gid].name);
	  if (!groupList[gid].spseqn)
	    {
	      printf("Current spool sequence numeber is zero. Give the initial value ? ");
	      scanf("%d", &ptrVal);
	    }
	  else
	    ptrVal = groupList[gid].spseqn;
	  spool_set_replay_pointer(groupList[gid].name, ptrVal);
	  printPrompt('s');
	  break;
	case 1:
	  replayAll(gid);
	  break;
	case 2:
	  replaySelectedMess(gid, 0, groupList[gid].messID);
	  break;
	case 3:
	  printf("What key ? ");
	  scanf("%s", key);
	  replayKeyedMess(gid, key);
	  break;
	case 4:
	  printf("What key ? ");
	  scanf("%s", key);
	  replayKeyedSelectedMess(gid, 0, groupList[gid].messID, key);
	  break;
	      
	case 5:
	  checkpoint(gid);
	  printPrompt('s');
	  break;
	case 6:
	  selected_check(gid, groupList[gid].messID);
	  printPrompt('s');
	  break;
	case 7:
	  printf("What key ? ");
	  scanf("%s", key);
	  keyed_check(gid, key);
	  printPrompt('s');
	  break;
	case 8:
	  printf("What key ? ");
	  scanf("%s", key);
	  keyed_selected_check(gid, groupList[gid].messID, key);
	  printPrompt('s');
	  break;
	case 9:
	  printf("Setting checkpoint pointer of spool: ** %s ** \n", groupList[gid].name);
	  printf("Enter the new checkpoint pointer value ? ");
	  scanf("%d", &ptrVal);
	  spool_set_ckpt_pointer(groupList[gid].name, ptrVal);
	  printPrompt('s');
	  break;
	}
    }
  isis_input(0, serverMenu, NULLARG);
}

/*****************************************************************************
 * Common procedures to both  client and server
 *****************************************************************************/
void
printPrompt(role)
     char role;
{
  if (role == 'c')
    printf("[lh_sp_client]: ? ");
  else
    {
      printf("\n[lh_sp_server]: ? ");
    }
}

void
printNetView()
{
  int i;
  if (netList != (struct netConDesc*) 0)
    {
      printf("\n partner name    messID \n");
      for (i = 0; i < numberOfNets; i++)
	printf("     %s     :     %d     :\n", netList[i].name, netList[i].messID);
    }
}

void
printGroupViews()
{
  int i;
  printf("Group name  : spseqn : messID : \n");
  for (i = 0; (i < MAXTESTGROUPS); i++)
    if (!addr_isnull(groupList[i].gaddr))
      printf("  %s   :  %d  :  %d  :\n", groupList[i].name,
	     groupList[i].spseqn, groupList[i].messID);
}

void
menu()
{
  char svType;
 yourChoice:
  printf("\n Test of the spooler and long_haul package \n");
  printf(" Two roles are provided: Server (s) or Client (c) \n");
  printf("\n your choice: <c | s> : ? ");
  scanf("%c", &svType);
  switch(svType)
    {
    default:
      goto yourChoice;
      break;
    case 'c':
      clientMenu();
      break;
    case 's':
      initPhase = 1;
      serverMenu();
      break;
    }
}

void
main(argc, argv)
     int argc;
     char **argv;
{
  int i, cc;
  FILE *fd;
  char *netNameList = 0;
  char *s = "";
  int portNumber = -1;
  thisNetIndex = -1;
  netList = (struct netConDesc*) 0;

  while(argc-- > 1)
    {
      register char *argument = *++argv;
      register c = *argument;
      if (isdigit(c))
	portNumber = atoi(*argv);
      else
	if(c == '-')
	  switch(*++argument)
	    {
	    default:
	      goto usage;
	    case 'n':
	      if (*(netNameList = ++argument) == 0)
		{
		  --argc;
		  netNameList = *++argv;
		}
	      if ((fd = fopen(netNameList, "r")) == (FILE*)0)
		{
		  perror(netNameList);
		  exit(0);
		}
	      break;
	    case 'l':
	      if (*(s = ++argument) == 0)
		{
		  --argc;
		  s = *++argv;
		}
	      break;
	    }
      else
	{
	usage:
	  panic("\n Usage: lh_sp_test port_number [-n netFile] [-l local_net_name] ");
	}
    }

  if (portNumber == -1)
    goto usage;
  
  /* Initializing the network structure */
  if (netNameList != (char*) 0)
    {
      netList = (struct netConDesc*) calloc(MAXNETS, sizeof(struct netConDesc));
      i = 0;
      while((cc = fscanf(fd, "%s", netList[i].name) != EOF))
	{
	  netList[i].messID = 0;
	  if (!strcmp(netList[i].name, s))
	    thisNetIndex = i;
	  i++;
	}
      numberOfNets = i;
      for (; i < MAXNETS; i++)
	{
	  strcpy(netList[i].name, "");
	  netList[i].messID = 0;
	}
    }
  else
    currentMessageID = 0;

  /* Initializing the structure describing the state of com with different groups */
  for (i = 0 ; i < MAXTESTGROUPS; i++)
    {
      strcpy(groupList[i].name, "");
      groupList[i].messID = 0;
      groupList[i].spseqn = 0;
      groupList[i].gaddr = (address*) 0;
    }

  /* Starting isis application */
  isis_init(portNumber);
  isis_entry(RCV_SIMPLE_MESS, rcvMess, "rcvSimpleMess");
  isis_entry(RCV_MESS_WITH_SEQN, rcvMess, "rcvNumberedMess");
  isis_entry(RCV_MESS_WITH_KEY, rcvMess, "rcvKeyedMess");
  isis_entry(RCV_MESS_WITH_KEY_AND_SEQ, rcvMess, "rcvNumbered_and_keyed_Mess");
  isis_entry(CHECKPOINT, rcvCheckpoint, "rcvCheckpoint");

  isis_task(menu, "Test_mainTask");
  isis_task(serverMenu, "serverMenu");

  isis_mainloop(menu, NULLARG);
}
