/*  $RCSfile: long_haul.c,v $ $Revision: 2.37 $ $Date: 90/09/17 09:11:19 $  */
/*
 *      New ISIS Long-Haul server
 *
 * Coded by Mesaac Makpangou
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

#define ISIS_SYS
#include <string.h>
#include "isis.h"
#include "spooler.h"
#include "long_haul.h"

/***************************************************************************************
 * LONG HAUL GLOBAL VARIABLES
 ***************************************************************************************/
int thisSiteIndex;			/* Local host's index within the local net's hostTab */
int thisNetIndex;			/* Index of the local net (or cluster) within netList */
int serverSock;				/* Passive socket to accept incoming connections */
int serverWid;				/* Wait identifier on the server socket.... v2 ... */
int currentMessageID;			/* Send side */
int outOFband;				/* Number of file fragments sent */
int default_port;			/* Default port number used to request connection */
int numberOfNets;			/* Number of clusters described in the clusters file */
struct r_netView *netList;		/* run-time nets' view */
struct conDesc *inConList;		/* List of incoming connections */
struct backupCon *backupList;		/* List of incoming connections, from known nets */
struct memberLoad *weightTab;		/* Table of load per member */
struct comState *comStateTab;		/* Communication state with all nets */
struct conDesc *outConList;		/* List of outgoing connections */
address *affectation;			/* The affection table stating each member's charge */
address *interLanGaddr;			/* The inter-LANs group address */
LISTE *applList;			/* Liste of the descricptors of WAN applications */
LISTE *filesList;			/* Listes of files being received by this process */
struct sockaddr_in thisSite_sockAddr;
bitvec all_ones;			/* All bits of this bitvec are set to 1 */
bitvec global_convID;			/* global conv ID -- one made of all clusters -- */
int enable_garbage;			/* Enable the garbage collection */
condition connectionsAllReady;		/* Used to signal application waiting that all clusters are 
					   already connected */

/* group name and entry for spooling error reports to */
static char srcerrgroup[100];
static char dsterrgroup[100];
static int srcerrentry = -1;
static int dsterrentry = -1;

extern int diemsg;

/*****************************************************
 *  INITIALIZATION OF THE LONG-HAUL PACKAGE
 *****************************************************/
void
long_haul_init(fd)
	FILE *fd;
{
	int cc, i, j, p, goahead, kk, btries;
	char hostName[MAXNAMELENGTH], type, *s1, *s2;
	struct hostent *hp;
	struct in_addr *addr1, *addr2;

	signal(SIGPIPE, SIG_IGN);
	for (i = 0; i < MAXBITS; i++)
		bis(&all_ones, i);

	/* Allocation of the run-time nets description */
	netList = (struct r_netView *) calloc(MAX_CLUSTERS, sizeof(struct r_netView));
	if (netList == (struct r_netView *) 0)
		panic("Error, while allocating the net view structure. \n");
	for (i = 0; i < MAX_CLUSTERS; i++) {
		bzero(netList[i].netName, MAXNAMELENGTH);
		for (j = 0; j < MAX_ACCESS_POINTS; j++) {
			netList[i].hostTab[j].portNumber = 0;
			bzero((char *) &netList[i].hostTab[j].hostAddr, sizeof(struct in_addr));
		}
	}
	if (fscanf(fd, "%d", &default_port) < 1)
		panic("Cant't get default port from the networks file");
	i = 0;
	while ((cc = fscanf(fd, "%d", &i) != EOF)) {
		if ((cc = fscanf(fd, "%s", netList[i].netName)) < 1)
			panic("Cant't get netname from the networks file");
		j = 0;
		goahead = 1;
		while (goahead) {
			if (fscanf(fd, "%s", hostName) < 1)
				panic("Cant't get hostname from the networks file");
			if (goahead = strcmp(hostName, "0")) {
				type = hostName[0];
				s1 = strchr(hostName, ':');
				s2 = strchr(hostName, '/');
				if (((type != 'N') && (type != 'A')) || (s1 == (char *) 0)
				    || (s2 == (char *) 0))
					panic("Error, bad entry in the networks file \n");
				netList[i].hostTab[j].portNumber = (p =
								    atoi(s2 +
									 1)) ? p : default_port;
				if (!netList[i].hostTab[j].portNumber)
					panic
					    ("Error, unspecified port number while default_port is null \n");
				s2[0] = '\0';
				switch (type) {
				case 'N':
					hp = gethostbyname(s1 + 1);
					if (hp == (struct hostent *) 0)
						panic
						    ("Error, gethostbyname returns null pointer \n");
					bcopy(hp->h_addr,
					      (char *) &(netList[i].hostTab[j].hostAddr),
					      hp->h_length);
					break;
				case 'A':
					bcopy(s1 + 1, (char *) &netList[i].hostTab[j].hostAddr,
					      sizeof(struct in_addr));
					break;
				default:
					break;
				}
				j++;
			} else
				netList[i].hostTab[j].portNumber = 0;
		}
		i++;
	}
	numberOfNets = i;

	/* Determining thisSiteIndex and thisNetIndex. */
	gethostname(hostName, MAXNAMELENGTH);
	hp = gethostbyname(hostName);
	if (hp == (struct hostent *) 0)
		panic("Error, gethostbyname returns null pointer for this host!\n");
	addr1 = (struct in_addr *) hp->h_addr;
	thisSiteIndex = -1;
	thisNetIndex = -1;
	for (kk = 0; ((kk < MAX_CLUSTERS) && (thisNetIndex == -1)); kk++)
		for (i = 0; ((i < MAX_ACCESS_POINTS) && (thisSiteIndex == -1)); i++) {
			addr2 = (struct in_addr *) &netList[kk].hostTab[i].hostAddr;
			if (addr1->s_addr == addr2->s_addr) {
				thisSiteIndex = i;
				thisNetIndex = kk;
			}
		}
	if (thisSiteIndex == -1)
		panic("Error, the local site is not found in the nets file \n");

	/* Allocation and initialization of structures */
	outConList = (struct conDesc *) calloc(MAX_CLUSTERS, sizeof(struct conDesc));
	if (outConList == (struct conDesc *) 0)
		panic("Error, can't allocate the outConList");
	inConList = (struct conDesc *) calloc(MAX_CLUSTERS, sizeof(struct conDesc));
	if (inConList == (struct conDesc *) 0)
		panic("Error, can't reserve for the list of incoming connection. \n");
	backupList = (struct backupCon *) calloc(MAX_CLUSTERS, sizeof(struct backupCon));
	if (backupList == (struct backupCon *) 0)
		panic("Error, can't allocate the backup list ");
	weightTab = (struct memberLoad *) calloc(MAX_ACCESS_POINTS, sizeof(struct memberLoad));
	if (weightTab == (struct memberLoad *) 0)
		panic("Error, can't allocate the weigtTab table ");
	affectation = (address *) calloc(MAX_CLUSTERS, sizeof(address));
	if (affectation == (address *) 0)
		panic("Error, can't allocate the affectation table ");
	comStateTab = (struct comState *) calloc(MAX_CLUSTERS, sizeof(struct comState));
	if (comStateTab == (struct comState *) 0)
		panic("Error, can't allocate the comState table ");
	for (i = 0; i < MAX_CLUSTERS; i++) {
		outConList[i].sock = outConList[i].hostIndex = outConList[i].wid = -1;
		strcpy(outConList[i].partner, netList[i].netName);
		inConList[i].sock = inConList[i].hostIndex = inConList[i].wid = -1;
		bzero(inConList[i].partner, MAXNAMELENGTH);
		backupList[i].hostIndex = -1;
		backupList[i].procAddr = NULLADDRESS;
		backupList[i].con = (struct conDesc *) 0;
		comStateTab[i].oldestInMess = comStateTab[i].lowerBound = -1;
		strcpy(comStateTab[i].partnerNet, netList[i].netName);
		affectation[i] = NULLADDRESS;
	}
	for (i = 0; i < MAX_ACCESS_POINTS; i++)
		weightTab[i].procAddr = NULLADDRESS;
	applList = newListe();
	for (j = 0; j < MAX_CLUSTERS; j++)
		if ((j != thisNetIndex) && strcmp(netList[j].netName, ""))
			bis(&global_convID, j);
	filesList = newListe();
	isis_define_type(messDesc_format, sizeof(messDesc), PDESCBUF_converter);
	/* Creation of the passive socket to listen for connection requests */
	serverSock = Socket(AF_INET, SOCK_STREAM, 0);
	bzero((char *) &thisSite_sockAddr, sizeof(struct sockaddr_in));
	thisSite_sockAddr.sin_family = AF_INET;
	thisSite_sockAddr.sin_port = htons(netList[thisNetIndex].hostTab[thisSiteIndex].portNumber);
	bcopy((char *) hp->h_addr, (char *) &thisSite_sockAddr.sin_addr, hp->h_length);
	i = -1;
	btries = 0;
	while ((i < 0) && (btries < 40)) {
		i = Bind(serverSock, (struct sockaddr *) &thisSite_sockAddr,
			 sizeof(struct sockaddr_in));
		if (i < 0) {
			if (db_spool)
				printf("LH: coudn't do initial bind, sleeping....\n");
			btries++;
			sleep(5);
		}
	}
	if (i < 0)
		panic("giving up...\n");
	if (Listen(serverSock, MAX_CLUSTERS) == -1) {
		panic("LH: cant' continue");
	}
	/* Joining the inter lan communication group */
	interLanGaddr = pg_join(INTERLANGNAME,
				PG_INIT, initializer,
				PG_MONITOR, updateGView, 1,
				PG_XFER, 0, sendBackupList, rcvBackupList,
				PG_XFER, 1, sendWeightTab, rcvWeightTab,
				PG_XFER, 2, sendAffectation, rcvAffectation,
				PG_XFER, 3, sendComState, rcvComState,
				PG_XFER, 4, sendApplList, rcvApplList, 0);
	if (addr_isnull(interLanGaddr))
		panic("Join of interLanGaddr failed");
	serverWid = isis_input(serverSock, acceptProc, NULLARG);
	if (!pg_rank(interLanGaddr, &my_address)) {
		if (db_spool)
			print("LH -->: This member is the oldest one. It starts the timer\n");
		isis_timeout(STARTUPDELAY, sendStartup, NULLARG, NULLARG);
	}
}

/*****************************************************
 *   STATE INITIALIZATION AND  TRANSFER
 *****************************************************/
void
initializer()
{
}

/* State transfer: Send and receive procedures for domaine 0 */
void
sendBackupList(indicator, gaddr)
	int indicator;
	address *gaddr;
{
	int i;

	for (i = 0; i < MAX_CLUSTERS; i++)
		xfer_out(i + 1, "%d%A[1]", backupList[i].hostIndex, &backupList[i].procAddr);
}

void
rcvBackupList(s_ind, m)
	int s_ind;
	message *m;
{
	int i = s_ind - 1;

	msg_get(m, "%d%A[1]", &backupList[i].hostIndex, &backupList[i].procAddr);
}

/* State transfer: Send and receive for domaine 1 */
void
sendWeightTab(indicator, gaddr)
	int indicator;
	address *gaddr;
{
	int i;

	for (i = 0; i < MAX_ACCESS_POINTS; i++)
		xfer_out(i + 1, "%A[1]%d", &weightTab[i].procAddr, weightTab[i].weight);
}

void
rcvWeightTab(s_ind, m)
	int s_ind;
	message *m;
{
	int i = s_ind - 1;

	msg_get(m, "%A[1]%d", &weightTab[i].procAddr, &weightTab[i].weight);
}

/* State transfrer: send and receive procedures for domain 2 */
void
sendAffectation(indicator, gaddr)
	int indicator;
	address *gaddr;
{
	int i;

	for (i = 0; i < MAX_CLUSTERS; i++)
		xfer_out(i + 1, "%A[1]", &affectation[i]);
}

void
rcvAffectation(s_ind, m)
	int s_ind;
	message *m;
{
	int i = s_ind - 1;

	msg_get(m, "%A[1]", &affectation[i]);
}

/* State transfer: Send and receive for domaine 3 */
void
sendComState(indicator, gaddr)
	int indicator;
	address *gaddr;
{
	int i;

	for (i = 0; i < MAX_CLUSTERS; i++)
		xfer_out(i + 1, "%s%d%d", comStateTab[i].partnerNet,
			 comStateTab[i].oldestInMess, comStateTab[i].lowerBound);
}

void
rcvComState(s_ind, m)
	int s_ind;
	message *m;
{
	int i;

	i = s_ind - 1;
	msg_get(m, "%s%d%d", comStateTab[i].partnerNet,
		&comStateTab[i].oldestInMess, &comStateTab[i].lowerBound);
}

/* State transfer: Send and receive for domaine 4 */
void
sendApplList(indicator, gaddr)
	int indicator;
	address *gaddr;
{
	int i;
	LIEN *applien;
	appliEntry *app;

	i = 1;
	for (applien = applList->head; applien != (LIEN *) 0; applien = applien->next) {
		app = (appliEntry *) applien->item;
		xfer_out(i, "%s%A[MAX_ACCESS_POINTS]", app->name, app->has_copy);
	}
}

void
rcvApplList(s_ind, m)
	int s_ind;
	message *m;
{
	appliEntry *appl;
	LIEN *applien;
	char *vide = "";

	appl = newEntry(vide);
	msg_get(m, "%s%A[MAX_ACCESS_POINTS]", appl->name, appl->has_copy);
	applien = newLien(appl);
	lh_append(applList, applien);
}

/*********************************************
 *     ACCEPT AND CONNECT PROCEDURES
 *********************************************/

void
acceptProc()
{
	int addrsize, alloc_sock, i, j, goahead, ind;
	struct sockaddr_in *addrCl;
	struct in_addr *addr1, *addr2;

	addrCl = (struct sockaddr_in *) calloc(1, sizeof(struct sockaddr_in));
	addrsize = sizeof(struct sockaddr_in);
	bzero(addrCl, sizeof(struct sockaddr_in));
	alloc_sock = Accept(serverSock, (struct sockaddr *) addrCl, &addrsize);
	if (alloc_sock == -1) {
		panic("can't contine");
	}
	if (db_spool) {
		printf("accepted a connection from %d at %s\n",
		       ntohs(addrCl->sin_port), inet_ntoa(addrCl->sin_addr));

	}
	ind = -1;
	for (i = 0; i < MAX_CLUSTERS; i++)
		if (inConList[i].sock == -1) {
			ind = i;
			inConList[i].sock = alloc_sock;
			break;
		}
	if (ind == -1)
		panic("No available entry in inConList. Check your configuration \n");

	goahead = 1;
	for (i = 0; ((i < MAX_CLUSTERS) && goahead); i++) {
		j = 0;
		addr1 = (struct in_addr *) &addrCl->sin_addr;
		while (goahead && netList[i].hostTab[j].portNumber && (j < MAX_ACCESS_POINTS)) {
			addr2 = (struct in_addr *) &netList[i].hostTab[j].hostAddr;
			if (addr1->s_addr == addr2->s_addr) {
				inConList[ind].hostIndex = j;
				strcpy(inConList[ind].partner, netList[i].netName);
				backupList[i].con = &inConList[ind];
				gbcast(interLanGaddr, REG_ENTRY, "%d%d%A[1]", i, j, &my_address, 0);
				goahead = 0;
			} else
				j++;
		}
	}
	if (goahead) {
		inConList[ind].hostIndex = -1;
		strcpy(inConList[ind].partner, "");
		gbcast(interLanGaddr, WEIGHT_ENTRY, "%A[1]", &my_address, 0);
	}
	inConList[ind].wid = isis_input(inConList[ind].sock, lh_receive, &inConList[ind]);
	/* t_fork((vfunc*) lh_receive, (VOID*) &inConList[ind]); */
}

/* This procedure requests a connection establishment between
 * the local site, and a remote partner.
 */
int
connectTo(netIndex, hostIndex)
	int netIndex;
	int hostIndex;
{
	int cl_sock, my_cc, cc;
	struct sockaddr_in *addrCl, *addrSv;
	int tries;

	tries = 0;
      restart:
	cl_sock = Socket(AF_INET, SOCK_STREAM, 0);
	addrCl = (struct sockaddr_in *) calloc(1, sizeof(struct sockaddr_in));
	bzero(addrCl, sizeof(struct sockaddr_in));
	addrCl->sin_family = AF_INET;
	addrCl->sin_port = htons(0);
	/* bcopy((char*) &netList[thisNetIndex].hostTab[thisSiteIndex].hostAddr, (char*)
	   &addrCl->sin_addr, sizeof(struct in_addr)); */
	cc = Bind(cl_sock, (struct sockaddr *) addrCl, sizeof(struct sockaddr_in));
	if (cc == -1)
		panic("Error during a binding operation\n");
#ifdef SO_SNDBUF
	{
		int bufsize = 32000;

		cc = setsockopt(cl_sock, SOL_SOCKET, SO_SNDBUF, (char *) &bufsize, sizeof(bufsize));
		if (cc < 0) {
			perror("setsockopt SO_SNDBUF: ");
			fprintf(stderr, "continuing anyway...\n");
		}
		cc = setsockopt(cl_sock, SOL_SOCKET, SO_RCVBUF, (char *) &bufsize, sizeof(bufsize));
		if (cc < 0) {
			perror("setsockopt SO_RCVBUF: ");
			fprintf(stderr, "continuing anyway...\n");
		}
	}
#endif
	addrSv = (struct sockaddr_in *) calloc(1, sizeof(struct sockaddr_in));
	bzero(addrSv, sizeof(struct sockaddr_in));
	addrSv->sin_family = AF_INET;
	addrSv->sin_port = htons(netList[netIndex].hostTab[hostIndex].portNumber);
	bcopy((char *) &netList[netIndex].hostTab[hostIndex].hostAddr,
	      (char *) &addrSv->sin_addr, sizeof(struct in_addr));
	my_cc = Connect(cl_sock, (struct sockaddr *) addrSv, sizeof(struct sockaddr_in));
	if (my_cc == -1) {
		if (db_spool) {
			printf("Failed to connect to port %d at %s\n",
			       ntohs(addrSv->sin_port), inet_ntoa(addrSv->sin_addr));
		}
		close(cl_sock);
		free((char *) addrCl);
		free((char *) addrSv);
		if (errno == EADDRINUSE) {
			if (tries++ < 20) {
				if (db_spool)
					printf("Address in use, retrying...\n");
				goto restart;
			} else {
				if (db_spool)
					printf("Address in use too many times, giving up...\n");
			}
		}
		return (my_cc);
	}
	return (cl_sock);
}

/***********************************************
 * Updating the communication state table
 ************************************************/
void
updateComState(m)
	register message *m;
{
	register i, ind;
	int ack;
	char *netName;

	ind = -1;
	msg_get(m, "%-s%d", &netName, &ack);
	for (i = 0; ((i < MAX_CLUSTERS) && strcmp(comStateTab[i].partnerNet, netName)); i++)
		if (!strcmp(comStateTab[i].partnerNet, "") && (ind == -1))
			ind = i;
	if ((i == MAX_CLUSTERS) && (ind == -1))
		panic("No available entry in comState table. Check your configuration. \n");
	else if (i == MAX_CLUSTERS) {
		strcpy(comStateTab[ind].partnerNet, netName);
		comStateTab[ind].oldestInMess = -1;
		comStateTab[ind].lowerBound = -1;
	} else
		comStateTab[i].lowerBound = ack;
}

void
updateLowerBound(m)
	register message *m;
{
	int i, lowerBound;
	char *netName;

	msg_get(m, "%-s%d", &netName, &lowerBound);
	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(netName, netList[i].netName)) {
			comStateTab[i].lowerBound = lowerBound;
			break;
		}
}

void
updateOldestInMess(m)
	register message *m;
{
	int messID;
	register i;
	char *netName;

	msg_get(m, "%-s%d", &netName, &messID);
	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(netName, netList[i].netName)) {
			comStateTab[i].oldestInMess = messID;
			break;
		}
}

void
rcvSpoolReq(msg)
	register message *msg;
{
	message *wan_msg;
	char *netName;
	int messID, ack;
	register i;

	msg_get(msg, "%m%d%d%-s", &wan_msg, &messID, &ack, &netName);

#ifdef NOSPOOLER
	user_rcvMess(wan_msg);
#else
	respool(wan_msg);
#endif				/* NOSPOOLER */
	msg_delete(wan_msg);
	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(netName, netList[i].netName)) {
			comStateTab[i].oldestInMess = messID;
			comStateTab[i].lowerBound = ack;
			break;
		}
}

/***************************************************
 *    DEALING WITH INCOMING CONNECTIONS.
 ***************************************************/
void
registerCon(m)
	message *m;
{
	int netIndex, hostIndex, i, goahead, cc;
	address sender_address;

	if (db_spool)
		print("LH -->: Registering a backup connection \n");

	msg_get(m, "%d%d%A[1]", &netIndex, &hostIndex, &sender_address);
	backupList[netIndex].hostIndex = hostIndex;
	backupList[netIndex].procAddr = sender_address;
	goahead = 1;
	for (i = 0; ((i < MAX_ACCESS_POINTS) && goahead); i++)
		if (cc = addr_isequal(&weightTab[i].procAddr, &sender_address)) {
			goahead = 0;
			weightTab[i].weight++;
		}
	if (i == MAX_ACCESS_POINTS)
		panic("Error, inconsistent weightTab is found. Check your configuration \n");
}

void
updateLoad(m)
	message *m;
{
	int i, cc, goahead;
	address sender_address;

	msg_get(m, "%A[1]", &sender_address);
	goahead = 1;
	for (i = 0; ((i < MAX_ACCESS_POINTS) && goahead); i++)
		if (cc = addr_isequal(&weightTab[i].procAddr, &sender_address)) {
			goahead = 0;
			weightTab[i].weight++;
		}
	if (i == MAX_ACCESS_POINTS)
		panic("Error, inconsistent weightTab is found. Check your config \n");
}

/*****************************************************
 *    SET UP OF CONNECTIONS WITH REMOTE NETWORKS
 ******************************************************/
void
sendStartup()
{
	if (db_spool)
		print("LH -->: Broadcasting the start up message to current members\n");
	gbcast(interLanGaddr, STARTUP_ENTRY, "%A[1]", &my_address, 0);
}

void
receiveStartup(m)
	message *m;
{
	address sender_address;

	if (db_spool)
		print("LH -->:Receiving a startup message from the oldest member\n");
	msg_get(m, "%A[1]", &sender_address);
	dispatcherProc();
}

void
dispatcherProc()
{
	int i, j, weight, elu, cc;

	for (i = 0; i < MAX_CLUSTERS; i++)
		if (strcmp(netList[i].netName, "") && addr_isnull(&affectation[i])
		    && !addr_isnull(&backupList[i].procAddr)) {
			affectation[i] = backupList[i].procAddr;
			if (addr_ismine(&backupList[i].procAddr)) {
				outConList[i].sock = backupList[i].con->sock;
				outConList[i].hostIndex = backupList[i].hostIndex;
				cc = sendInitialMess(i, outConList[i].sock);
				if (!cc)
					startReplay(i);
				else {
					if (db_spool)
						print
						    ("LH -->: Can't init a connection with network <%s> \n",
						     netList[i].netName);
					if (outConList[i].wid != -1) {
						isis_wait_cancel(outConList[i].wid);
						outConList[i].wid = -1;
					}
					close(outConList[i].sock);
					signalConFailure(i, outConList[i].hostIndex,
							 outConList[i].sock);
				}
			}
		} else if (strcmp(netList[i].netName, "") && addr_isnull(&affectation[i])) {
			weight = MAX_CLUSTERS + 1;
			elu = -1;
			for (j = 0; j < MAX_ACCESS_POINTS; j++)
				if ((weight > weightTab[j].weight)
				    && !addr_isnull(&weightTab[j].procAddr)) {
					weight = weightTab[j].weight;
					elu = j;
					if (!weight)
						break;
				}
			if (elu == -1)
				panic("Problem with weightTab management \n");
			affectation[i] = weightTab[elu].procAddr;
			weightTab[elu].weight++;
			if (addr_ismine(&affectation[i]))
				t_fork((vfunc *) establishier, (VOID *) i);
		}
}

void
establishier(netIndex)
	int netIndex;
{
	int code, currentHostIndex, cc, firstPass, nb;

	/* Connection with the local network or cluster */
	if (netIndex == thisNetIndex) {
		outConList[netIndex].sock = connectTo(netIndex, thisSiteIndex);
		if (outConList[netIndex].sock == -1)
			panic("LH -->: Can't establish a local tcp connection with this host!! \n");
		outConList[netIndex].hostIndex = thisSiteIndex;
		startReplay(netIndex);
	} else {
		for (nb = 0; ((nb < MAX_ACCESS_POINTS) && netList[netIndex].hostTab[nb].portNumber);
		     nb++) ;
		if (currentHostIndex = (thisNetIndex % nb))
			firstPass = 1;
		else
			firstPass = 0;

		/* Successives retries loop */
		cc = -1;
	      re_start:
		while (!addr_ismine(&backupList[netIndex].procAddr) &&
		       netList[netIndex].hostTab[currentHostIndex].portNumber &&
		       (currentHostIndex < MAX_ACCESS_POINTS) && (cc == -1)) {
			cc = connectTo(netIndex, currentHostIndex);
			currentHostIndex++;
		}
		if ((cc == -1) && firstPass) {
			firstPass = 0;
			currentHostIndex = 0;
			if (db_spool)
				printf("going to re_start....\n");
			goto re_start;
		}
		if (cc != -1) {
			outConList[netIndex].sock = cc;
			outConList[netIndex].hostIndex = --currentHostIndex;
			code = sendInitialMess(netIndex, outConList[netIndex].sock);
			if (!code) {
				outConList[netIndex].wid =
				    isis_input(outConList[netIndex].sock,
					       lh_receive, &outConList[netIndex]);
				/* t_fork((vfunc*) lh_receive, (VOID*) &outConList[netIndex]); */
				startReplay(netIndex);
			} else {
				if (db_spool)
					print("LH -->: sendInitMess failed \n");
				if (outConList[netIndex].wid != -1) {
					isis_wait_cancel(outConList[netIndex].wid);
					outConList[netIndex].wid = -1;
				}
				close(outConList[netIndex].sock);
				signalConFailure(netIndex, outConList[netIndex].hostIndex,
						 outConList[netIndex].sock);
			}
		} else if (addr_ismine(&backupList[netIndex].procAddr)) {
			outConList[netIndex].sock = backupList[netIndex].con->sock;
			outConList[netIndex].hostIndex = backupList[netIndex].hostIndex;
			startReplay(netIndex);
		} else {
			if (db_spool)
				print
				    ("LH -->: <%s> still not responding..., next try in %d ms \n\n",
				     netList[netIndex].netName, retryDelay);
			isis_timeout(retryDelay, (vfunc *) establishier, (VOID *) netIndex,
				     NULLARG);
		}
	}
}

/************************************************
 *        MONITORING THE INTERLAN GROUP 
 ************************************************/
void
updateGView(gview, dummy)
	register groupview *gview;
	int dummy;
{
	register i;

	if (!addr_isnull(&gview->gv_joined)) {
		for (i = 0; ((i < MAX_ACCESS_POINTS) && (!addr_isnull(&weightTab[i].procAddr)));
		     i++) ;
		weightTab[i].procAddr = gview->gv_joined;
		weightTab[i].weight = 0;
	} else if (!addr_isnull(&gview->gv_departed)) {
		for (i = 0; i < MAX_CLUSTERS; i++) {
			if (addr_isequal(&affectation[i], &gview->gv_departed)) {
				affectation[i] = NULLADDRESS;
				outConList[i].sock = -1;
				outConList[i].hostIndex = -1;
			}

			if (addr_isequal(&backupList[i].procAddr, &gview->gv_departed)) {
				backupList[i].procAddr = NULLADDRESS;
				backupList[i].hostIndex = -1;
			}
			treatApplMgrFailure(&gview->gv_departed);
		}

		for (i = 0; ((i < MAX_ACCESS_POINTS) &&
			     addr_cmp(&weightTab[i].procAddr, &gview->gv_departed)); i++) ;
		if (i == MAX_ACCESS_POINTS)
			panic("Can't find an old member within the weightTab \n");
		weightTab[i].procAddr = NULLADDRESS;
		weightTab[i].weight = 0;
		dispatcherProc();
	}
}

void
treatPartnerFailure(m)
	message *m;
{
	int i, j, hostIndex, ind, chargeOff;
	char partnerName[MAXNAMELENGTH];
	address sender;

	msg_get(m, "%A[1]%s%d%d%d", &sender, partnerName, &hostIndex, &chargeOff, &ind);
	for (j = 0; ((j < MAX_ACCESS_POINTS) && !addr_isequal(&weightTab[j].procAddr, &sender));
	     j++) ;
	if (j == MAX_ACCESS_POINTS)
		panic("Error, can't find the sender member within the weightTab. \n");
	weightTab[j].weight -= chargeOff;
	for (i = 0; ((i < MAX_CLUSTERS) && strcmp(partnerName, netList[i].netName)); i++) ;
	if (i < MAX_CLUSTERS) {
		if (!addr_isnull(&backupList[i].procAddr) && (backupList[i].hostIndex == hostIndex))
			backupList[i].procAddr = NULLADDRESS;
		if (ind) {
			if (db_spool)
				print("LH -->: Partner failure \n");
			affectation[i] = NULLADDRESS;
			dispatcherProc();
		}
	}
}

/*********************************************
 *           Inter-clusters PROTOCOL
 **********************************************/
int
lhmsg_isValid(messID, source)
	register messID;
	register char *source;
{
	register i;

	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(source, comStateTab[i].partnerNet))
			if (comStateTab[i].oldestInMess < messID)
				return (0);
			else {
				if (db_spool)
					print("lhmsg_isValid(%d): duplicate\n", messID);
				return (-1);
			}
	panic("Receiving a message from an unknown net: <%s> in comStateTab \n", source);
}

int
sendInitialMess(netIndex, sock)
	int netIndex;
	int sock;
{
	message *msg;
	int code;

	msg = msg_newmsg();
	msg_put(msg, "%s", netList[thisNetIndex].netName);
	msg_putfld(msg, SYSFLD_LHOPCODE, "%d", INITOP);
	msg_putfld(msg, LH_HDR, "%d%d", 0, comStateTab[netIndex].oldestInMess);
	if ((code = msg_write(sock, msg)) == -1) {
		msg_delete(msg);
		return (-1);
	} else {
		msg_delete(msg);
		return (0);
	}
}

void
signalConFailure(netIndex, hostIndex, sock)
	int netIndex;
	int hostIndex;
	int sock;
{
	int indicator, chargeOff, osock, i;

	chargeOff = 1;
	osock = -1;
	indicator = 0;

	if (sock == outConList[netIndex].sock) {
		outConList[netIndex].sock = outConList[netIndex].hostIndex = -1;
		stopReplay(netList[netIndex].netName);
		indicator = 1;
	} else
	    if (addr_ismine(&affectation[netIndex]) &&
		(outConList[netIndex].hostIndex == hostIndex)) {
		if (outConList[netIndex].wid != -1) {
			isis_wait_cancel(outConList[netIndex].wid);
			outConList[netIndex].wid = -1;
		}
		close(outConList[netIndex].sock);
		osock = outConList[netIndex].sock;
		outConList[netIndex].sock = outConList[netIndex].hostIndex = -1;
		chargeOff++;
		indicator = 1;
		stopReplay(netList[netIndex].netName);
	}
	for (i = 0; i < MAX_CLUSTERS; i++) {
		if (inConList[i].sock == sock) {
			inConList[i].sock = inConList[netIndex].hostIndex = -1;
			if (inConList[i].wid != -1) {
				isis_wait_cancel(inConList[i].wid);
				inConList[i].wid = -1;
			}
		}
	}
	if (addr_ismine(&backupList[netIndex].procAddr) &&
	    (backupList[netIndex].hostIndex == hostIndex)) {
		if ((sock != backupList[netIndex].con->sock) &&
		    (osock != backupList[netIndex].con->sock)) {
			chargeOff++;
			if (backupList[netIndex].con->wid != -1) {
				isis_wait_cancel(backupList[netIndex].con->wid);
				backupList[netIndex].con->wid = -1;
			}
			close(backupList[netIndex].con->sock);
			backupList[netIndex].con->sock = backupList[netIndex].con->hostIndex = -1;
			strcpy(backupList[netIndex].con->partner, "");
		}
		backupList[netIndex].con = (struct conDesc *) 0;
	}
	flush();
	if (db_spool)
		print("LH -->: Signalling partner failure. The signaller has %d charge off \n",
		      chargeOff);

	gbcast(interLanGaddr, PARTNER_FAILED, "%A[1]%s%d%d%d", &my_address,
	       netList[netIndex].netName, hostIndex, chargeOff, indicator, 0);
}

void
lh_send(netName, m)
	register char *netName;
	register message *m;
{
	char *path, *dest;
	int cc, opcode, position = 0;

	msg_getfld(m, SYSFLD_LHOPCODE, &position, "%d", &opcode);
	if (opcode == LH_XFER_OP) {
		position = 0;
		if ((cc = msg_getfld(m, SYSFLD_FILE, &position, "%-s%-s", &path, &dest)) == 2)
			do_send_file(netName, m, path, dest);
		else {
			if (db_spool)
				print("Unspecified file name in lh_send \n");
			/* isis_error() */ ;
		}
	} else
		do_lh_send(netName, m, 0);
	msg_delete(m);
}

static int trigger = 0;

#define SEQHISTLEN 128		/* Power of 2 required */

#define INC(i)     ((i+1)&(SEQHISTLEN-1))
#define DEC(i)     ((i+SEQHISTLEN-1)&(SEQHISTLEN-1))

static majorseq[SEQHISTLEN] = { -1 };

static oobseq[SEQHISTLEN];
static seqentry = 1;
static infile = 0;

void
do_lh_send(netName, m, InFile)
	register char *netName;
	register message *m;
{
	register messageID, netIndex, i, cc;

	if (!strcmp(netName, localNet))
		netIndex = thisNetIndex;
	else {
		for (i = 0; ((i < MAX_CLUSTERS) && strcmp(netName, netList[i].netName)); i++) ;
		if (i < MAX_CLUSTERS)
			netIndex = i;
		else {
			if (db_spool)
				print("LH: --> Request sending to an unreachable network: %s \n",
				      netName);
			/* isis_error(); */
			panic("LH: --> Request sending to an unreachable network: %s \n", netName);
		}
	}
	if (outConList[netIndex].sock >= 0) {
#ifdef NOSPOOLER
		panic("spooler no longer supports NOSPOOLER mode");
#endif
		if (InFile == 0) {
			int spseqn = spool_getseqn(m);

			if (spseqn < currentMessageID) {
				register n, i = seqentry;

				for (n = 0; n < SEQHISTLEN; n++) {
					i = DEC(i);
					if (majorseq[i] < spseqn)
						break;
				}
				outOFband = oobseq[i];
				seqentry = INC(i);
				if (n == SEQHISTLEN)
					panic("LH: checkpoint/restart didn't find OO seqn.");
				else if (db_spool)
					print
					    ("LH: do_lh_send detected replay, reset message seqns to %d %d\n",
					     spseqn, outOFband);
			} else {
				if (infile == 1) {
					majorseq[seqentry] = currentMessageID;
					oobseq[seqentry] = outOFband;
					seqentry = INC(seqentry);
				}
			}
			currentMessageID = spseqn;
		} else
			++outOFband;
		infile = InFile;
		messageID = currentMessageID + outOFband;
		msg_putfld(m, LH_HDR, "%d", messageID);
		msg_putfld(m, LH_HDR, "%d", comStateTab[netIndex].oldestInMess);
		cc = msg_write(outConList[netIndex].sock, m);
		if (db_spool && (trigger == diemsg))
			print("LH -->: Simulating line failure\n");
		if (((cc == -1) && (errno == EPIPE)) || (trigger++ == diemsg)) {
			if (db_spool)
				print
				    ("LH -->: Broken pipe detected. The partner is assumed disconnected \n");
			if (outConList[netIndex].wid != -1) {
				isis_wait_cancel(outConList[netIndex].wid);
				outConList[netIndex].wid = -1;
			}
			close(outConList[netIndex].sock);
			signalConFailure(netIndex, outConList[netIndex].hostIndex,
					 outConList[netIndex].sock);
		}
	} else if (db_spool)
		print("LH: ignoring -ve socket number - recent failure?\n");
}

/* Boby of the per connection input task */
void
lh_receive(con)
	struct conDesc *con;
{
	char source[MAXNAMELENGTH];
	int opcode, position, messID, ack, origid, error, op;
	register i;
	register message *m;
	char *path, *fname, *sname;

#ifdef NO_GROUP_DELIVERY
	message *msg;
#endif				/* NO_GROUP_DELIVERY */

	m = msg_read(con->sock);
	if (m == (message *) 0) {
		if (db_spool)
			print("LH -->: msg_read returns 0. The remote partner is down \n");
		if (con->wid != -1) {
			isis_wait_cancel(con->wid);
			con->wid = -1;
		}
		close(con->sock);
		for (i = 0; ((i < MAX_CLUSTERS) && strcmp(netList[i].netName, con->partner)); i++) ;
		if (i < MAX_CLUSTERS)
			signalConFailure(i, con->hostIndex, con->sock);
	} else {
		position = 0;
		msg_getfld(m, SYSFLD_LHOPCODE, &position, "%d", &opcode);
		position = 0;
		if (msg_getfld(m, LH_HDR, &position, "%d%d", &messID, &ack) != 2 && db_spool)
			print("... didn't find an LH_HDR!\n");
		switch (opcode) {
		default:
			break;
		case INITOP:
			if (db_spool)
				print("lh_receive: got INITOP (messID %d ack %d)\n", messID, ack);
			msg_get(m, "%s", source);
			if (!strcmp(con->partner, "")) {
				strcpy(con->partner, source);
				gbcast(interLanGaddr, COMSTATE_ADD_ENTRY, "%A[1]%s%d",
				       &my_address, con->partner, ack, 0);
			}
			break;
		case RESPOOL_OP:
			if (db_spool)
				print("lh_receive: got RESPOOL_OP (messID %d ack %d)\n", messID,
				      ack);
			if (!lhmsg_isValid(messID, con->partner)) {
				cbcast(interLanGaddr, RESPOOL_REQUEST, "%m%d%d%s", m, messID, ack,
				       con->partner, 0);
			}
			break;
		case CREATEWSPOOL_OP:
			if (db_spool)
				print("lh_receive: got CREATEWSPOOL_OP (messID %d ack %d)\n",
				      messID, ack);
			if (!lhmsg_isValid(messID, con->partner)) {
				cbcast(interLanGaddr, LH_JOIN_REQUEST, "%m", m, 0);
			}
			break;
		case LH_CBCAST_OP:
			if (db_spool)
				print("lh_receive: got LH_CBCAST_OP (messID %d ack %d)\n", messID,
				      ack);
			if (!lhmsg_isValid(messID, con->partner)) {
				cbcast(interLanGaddr, LH_CBCAST_REQUEST, "%m%d%d%s", m, messID, ack,
				       con->partner, 0);
			}
			break;
		case LH_ABCAST_OP:
			if (db_spool)
				print("lh_receive: got LH_ABCAST_OP (messID %d ack %d)\n", messID,
				      ack);
			if (!lhmsg_isValid(messID, con->partner)) {
				cbcast(interLanGaddr, LH_ABCAST_REQUEST, "%m%d%d%s", m, messID, ack,
				       con->partner, 0);
			}
			break;
		case LH_XFER_OP:
			if (db_spool)
				print("lh_receive: got LH_XFER_OP (messID %d ack %d)\n", messID,
				      ack);
			if (!lhmsg_isValid(messID, con->partner)) {
				position = 0;
				msg_getfld(m, SYSFLD_FILE, &position, "%-s", &path);
				rcvFileHdr(m, con->partner);
				cbcast(interLanGaddr, LH_COMSTATE_UPDATE, "%d%d%s", messID, ack,
				       con->partner, 0);
			}
			break;
		case LH_XFER_ACK:
			if (db_spool)
				print("lh_receive: got LH_XFER_ACK (messID %d ack %d)\n", messID,
				      ack);
			if (!lhmsg_isValid(messID, con->partner)) {
				msg_get(m, "%-s%-s%d%d%d", &fname, &sname, &error, &origid, &op);
				if (db_spool)
					printf("got file ack %s %s %d %d %d\n",
					       fname, sname, error, origid, op);
				if ((op == LH_MOVE) && (error == 0)) {
					if (db_spool)
						printf("unlinking %s\n");
					if (unlink(fname) == -1) {
						if (db_spool)
							printf("couldn't unlink %s\n", fname);
					}
				}
				cbcast(interLanGaddr, LH_COMSTATE_UPDATE, "%d%d%s", messID, ack,
				       con->partner, 0);
				if (error != 0) {
					if (srcerrentry != -1) {
						cbcast(interLanGaddr, LH_XFER_SND_FAILURE,
						       "%s%s%s%d", fname, sname, con->partner,
						       error, 0);
					}
				}
			}
			break;
		case RCV_FRAG_OP:
			if (db_spool)
				print("lh_receive: got RCV_FRAG_OP (messID %d ack %d)\n", messID,
				      ack);
			if (!lhmsg_isValid(messID, con->partner)) {
				rcvFragment(m, con->partner);
				cbcast(interLanGaddr, LH_COMSTATE_UPDATE, "%d%d%s", messID, ack,
				       con->partner, 0);
			}
			break;
		}
		msg_delete(m);
	}
}

/**********************************
 * Test connectivity status
 ***********************************/
int
allConReady()
{
	register i;

	for (i = 0; i < MAX_CLUSTERS; i++)
		if (strcmp(netList[i].netName, "") && (outConList[i].sock == -1))
			return (0);
	return (1);
}

int
isClusterName(name)
	register char *name;
{
	register i;

	for (i = 0; ((i < MAX_CLUSTERS) && strcmp(netList[i].netName, name)); i++) ;
	if (i == MAX_CLUSTERS)
		return (-1);
	else
		return (i);
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                    LONG HAUL PROTOCOLS
 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/***************************************************
 *   Converter procedure of the type z (messDesc).
 ***************************************************/
void
PDESCBUF_converter(desc)
	messDesc *desc;
{
	register i;

	msg_convertlong(&desc->messID);
	msg_convertlong(&desc->senderID);
	msg_convertlong(&desc->destID);
	for (i = 0; i < ISIS_BVL; i++)
		msg_convertlong(&desc->visitState.bv_data[i]);
}

/*******************************************************
 *  Lists manipulation an related operations procedures
 *********************************************************/
LIEN *
newLien(data)
	VOID *data;
{
	register LIEN *l;

	l = (LIEN *) calloc(1, sizeof(LIEN));
	if (l == (LIEN *) 0)
		panic("newLien failed: calloc returns a null pointer");
	l->next = l->previous = (LIEN *) 0;
	l->item = data;
	return (l);
}

LISTE *
newListe()
{
	register LISTE *list;

	list = (LISTE *) calloc(1, sizeof(LISTE));
	if (list == (LISTE *) 0)
		panic("newListe failed: calloc returns a null pointer \n");
	list->head = list->last = (LIEN *) 0;
	return (list);
}

messDesc *
newDesc()
{
	register messDesc *d;

	d = (messDesc *) calloc(1, sizeof(messDesc));
	if (d == (messDesc *) 0)
		panic("newDesc failed: calloc returns a null pointer \n");
	return (d);
}

localMessDesc *
newLocalDesc()
{
	localMessDesc *ldesc;

	ldesc = (localMessDesc *) calloc(1, sizeof(localMessDesc));
	if (ldesc == (localMessDesc *) 0)
		panic("calloc returns a null pointer \n");
	ldesc->nb_waitedMess = 0;
	return (ldesc);
}

PDESCBUF_item *
newPDESCBUF()
{
	register PDESCBUF_item *pbuf;

	pbuf = (PDESCBUF_item *) calloc(1, sizeof(PDESCBUF_item));
	if (pbuf == (PDESCBUF_item *) 0)
		panic("calloc returns a null pointer \n");
	pbuf->count = 0;
	pbuf->desc = (messDesc *) 0;
	return (pbuf);
}

void
lh_append(listptr, l)
	register LISTE *listptr;
	register LIEN *l;
{
	if (listptr->head == (LIEN *) 0) {
		listptr->head = l;
		listptr->last = l;
		l->next = (LIEN *) 0;
		l->previous = (LIEN *) 0;
	} else {
		listptr->last->next = l;
		l->next = (LIEN *) 0;
		l->previous = listptr->last;
		listptr->last = l;
	}
}

void
lh_remove(listptr, l)
	register LISTE *listptr;
	register LIEN *l;
{
	if (l == listptr->head) {
		listptr->head = l->next;
		if (l->next != (LIEN *) 0)
			l->next->previous = (LIEN *) 0;
		else
			listptr->last = listptr->head;
	} else {
		if (l == listptr->last) {
			listptr->last = l->previous;
			l->previous->next = (LIEN *) 0;
		} else {
			l->previous->next = l->next;
			l->next->previous = l->previous;
		}
	}
	l->previous = l->next = (LIEN *) 0;
}

void
freeLIEN(listptr, l)
	register LISTE *listptr;
	register LIEN *l;
{
	if (l != (LIEN *) 0) {
		lh_remove(listptr, l);
		l->item = (VOID *) 0;
		free((char *) l);
	}
}

messDesc *
allocDescTab(listptr, lengthptr)
	register LISTE *listptr;
	int *lengthptr;
{
	register LIEN *lien;
	register sz;
	register messDesc *descTabPtr = (messDesc *) 0;

	for (lien = listptr->head, sz = 0; lien != (LIEN *) 0; lien = lien->next, sz++) ;
	if (sz) {
		descTabPtr = (messDesc *) calloc(sz, sizeof(messDesc));
		if (descTabPtr == (messDesc *) 0)
			panic("Can't allocate %d messDesc \n", sz);
	}
	*lengthptr = sz;
	return (descTabPtr);
}

void
printDesc(d)
	messDesc *d;
{
	print("LH -->: Desc info: <%d,%d,%d> : <%d%d%d%d> \n", d->senderID, d->messID, d->destID,
	      d->visitState.bv_data[0], d->visitState.bv_data[1], d->visitState.bv_data[2],
	      d->visitState.bv_data[3]);
}

/*********************************************************
 * WAN APPLICATIONS MANAGENENT RELATED PROCEDURES
 *********************************************************/
appliEntry *
newEntry(name)
	char *name;
{
	register appliEntry *appl;
	register j;

	appl = (appliEntry *) calloc(1, sizeof(appliEntry));
	if (appl == (appliEntry *) 0)
		panic("Can't allocate a WAN application descriptor \n");
	strcpy(appl->name, name);
	appl->nextMessID = 1;
	appl->enableGarbage = 0;
	appl->waitMessQPtr = newListe();
	appl->deliverableQPtr = newListe();
	appl->PDESCBUF = newListe();
	appl->flag = 0;
	for (j = 0; j < MAX_CLUSTERS; j++)
		if (netList[j].hostTab[0].portNumber) {
			appl->PWDESC[j] = newListe();
			appl->rcvDesc[j] = newListe();
			appl->PCWaitingMess[j] = newListe();
			appl->waitTab[j] = 1;
		}
	return (appl);
}

void
freeEntry(app)
	register appliEntry *app;
{
	register j;
	register localMessDesc *ld;
	register PDESCBUF_item *pbuf;
	register messDesc *d;

	while (app->waitMessQPtr->head != (LIEN *) 0) {
		ld = (localMessDesc *) app->waitMessQPtr->head->item;
		msg_delete(ld->mptr);
		free((char *) ld);
		freeLIEN(app->waitMessQPtr, app->waitMessQPtr->head);
	}
	while (app->deliverableQPtr->head != (LIEN *) 0) {
		ld = (localMessDesc *) app->deliverableQPtr->head->item;
		msg_delete(ld->mptr);
		free((char *) ld);
		freeLIEN(app->deliverableQPtr, app->deliverableQPtr->head);
	}
	while (app->PDESCBUF->head != (LIEN *) 0) {
		pbuf = (PDESCBUF_item *) app->PDESCBUF->head->item;
		d = (messDesc *) pbuf->desc;
		free((char *) d);
		free((char *) pbuf);
		freeLIEN(app->PDESCBUF, app->PDESCBUF->head);
	}
	for (j = 0; j < MAX_CLUSTERS; j++) {
		while (app->PWDESC[j]->head != (LIEN *) 0) {
			freeLIEN(app->PWDESC[j], app->PWDESC[j]->head);
		}
		while (app->PCWaitingMess[j]->head != (LIEN *) 0) {
			freeLIEN(app->PCWaitingMess[j], app->PCWaitingMess[j]->head);
		}
		while (app->rcvDesc[j]->head != (LIEN *) 0) {
			freeLIEN(app->rcvDesc[j], app->rcvDesc[j]->head);
		}
	}
	free((char *) app);
}

void
addNewAppli(appl)
	register appliEntry *appl;
{
	register LIEN *applien;
	register groupview *gv;
	register j;

	if (db_spool)
		print("LH: --> Adding a new WAN application <%s>\n", appl->name);
	gv = pg_getview(interLanGaddr);
	for (j = gv->gv_nmemb + 1; j < MAX_ACCESS_POINTS; j++)
		appl->has_copy[j] = NULLADDRESS;
	bcopy((char *) gv->gv_members, (char *) appl->has_copy,
	      sizeof(address) * (gv->gv_nmemb + 1));
	applien = newLien(appl);
	lh_append(applList, applien);
	appl->flag = 1;
}

LIEN *
isAppli(name)
	register char *name;
{
	register LIEN *ap;
	register appliEntry *appl;

	for (ap = applList->head; ap != (LIEN *) 0; ap = ap->next) {
		appl = (appliEntry *) ap->item;
		if (!strcmp(appl->name, name))
			break;
	}
	return (ap);
}

/* Associated with the entry point LH_JOIN */
void
set_join(join_msg)
	message *join_msg;
{
	char *applName;
	register LIEN *applien;
	register i;
	register message *lh_join_msg;

	msg_get(join_msg, "%-s", &applName);
	if ((applien = isAppli(applName)) == (LIEN *) 0)
		for (i = 0; i < MAX_CLUSTERS; i++)
			if (netList[i].hostTab[0].portNumber) {
				lh_join_msg = msg_newmsg();
				msg_put(lh_join_msg, "%s", applName);
				msg_putfld(lh_join_msg, SYSFLD_SPSCAN, "%s%d", applName,
					   WAN_DUMMY_ENTRY);
				msg_putfld(lh_join_msg, SYSFLD_NETWORK, "%s", netList[i].netName);
				msg_putfld(lh_join_msg, SYSFLD_LHOPCODE, "%d", CREATEWSPOOL_OP);
				spool_msg(lh_join_msg);
				msg_delete(lh_join_msg);
			}
}

void
localjoin(applName)
	register char *applName;
{
	register appliEntry *app;
	register LIEN *applien;

	if ((applien = isAppli(applName)) == (LIEN *) 0) {
		app = newEntry(applName);
		addNewAppli(app);
	} else {
		if (db_spool)
			print("LH: --> <%s> WAN is already registered in this cluster\n", applName);
	}
}

void
lh_rcv_join(msg)
	register message *msg;
{
	char *applName;
	message *join_msg;

	msg_get(msg, "%m", &join_msg);
	msg_get(join_msg, "%-s", &applName);
	localjoin(applName);
	msg_delete(join_msg);
}

void
terminatedAppl(applName)
	char *applName;
{
	/* INVOKED WHEN ALL MEMBERS HAVING A COPY OF THIS APPL HAVE FAILED */
}

void
treatApplMgrFailure(member)
	address *member;
{
	appliEntry *app;
	register LIEN *applien;
	int i, encore = 0;

	for (applien = applList->head; applien != (LIEN *) 0; applien = applien->next) {
		app = (appliEntry *) applien->item;
		for (i = 0; i < MAX_ACCESS_POINTS; i++)
			if (!addr_cmp(member, &app->has_copy[i])) {
				app->has_copy[i] = NULLADDRESS;
				if (db_spool)
					print("LH: --> One of the manager of <%s> has failed\n",
					      app->name);
			} else if (!addr_isnull(&app->has_copy[i]))
				encore++;
		if (!encore)
			terminatedAppl(app->name);
	}
}

void
appliCrashed(msg)
	message *msg;
{
	/* ASSUMING THAT terminatedAppl notifies all clusters, this procedure will treat the
	   reception of such notification */
}

/* Associate with LH_CONVID_REQUEST entry */
void
giveConvID(msg)
	register message *msg;
{
	char *applName;
	char *participant;
	register i, cc;
	bitvec *convid;

	msg_get(msg, "%-s", &applName);
	convid = (bitvec *) calloc(1, sizeof(bitvec));
	bclr(convid);
	while ((cc = msg_get(msg, "%-s", &participant)) == 1)
		if ((i = isClusterName(participant)) != -1)
			bis(convid, i);
	reply(msg, "%B", convid);
}

/**********************************************************************
 *   Main procedures involved in the lh_cbcast implementation
 **********************************************************************/
/******************************
 * Sending related procedures *
 ******************************/
void
addPDESCBUF(appl, clusterID, convID)
	register appliEntry *appl;
	int clusterID;
	bitvec *convID;
{
	register j;
	register PDESCBUF_item *pbuf;
	register LIEN *l;
	register messDesc *desc;

	/* Creating the current message descriptor */
	desc = newDesc();
	desc->messID = appl->nextMessID;
	desc->senderID = thisNetIndex;
	desc->destID = clusterID;
	bclr(&desc->visitState);
	if (convID != (bitvec *) 0) {
		bisv(&desc->visitState, convID);
	} else {
		if (clusterID == ALLCLUSTERS) {
			bisv(&desc->visitState, &global_convID);
		} else {
			if (clusterID != thisNetIndex)
				bis(&desc->visitState, clusterID);
		}
	}

	/* Updating PDESCBUF and PWDESCs lists */
	pbuf = newPDESCBUF();
	pbuf->desc = desc;
	l = newLien(pbuf);
	lh_append(appl->PDESCBUF, l);
	for (j = 0; j < MAX_CLUSTERS; j++)
		if ((j != thisNetIndex) && netList[j].hostTab[0].portNumber) {
			l = newLien(appl->PDESCBUF->last);
			lh_append(appl->PWDESC[j], l);
			pbuf->count++;
		}
	l = newLien(appl->PDESCBUF->last);
	lh_append(appl->rcvDesc[thisNetIndex], l);
	pbuf->count++;
}

void
getPred(appl, destID, descTab)
	register appliEntry *appl;
	register destID;
	register messDesc *descTab;
{
	register LIEN *l, *l1, *ll;
	register LISTE *listptr;
	register i;
	PDESCBUF_item *pbuf;

	listptr = appl->PWDESC[destID];
	for (l = listptr->head, i = 0; l != (LIEN *) 0; l = l1, i++) {
		l1 = l->next;
		ll = (LIEN *) l->item;
		pbuf = (PDESCBUF_item *) ll->item;
		descTab[i] = *pbuf->desc;
		if (!bit(&pbuf->desc->visitState, thisNetIndex)) {
			lh_remove(listptr, l);
			pbuf->count--;
			if (!pbuf->count) {
				appl->enableGarbage++;
				enable_garbage++;
			}
		}
	}
}

/* Associated with the entry point LH_CBCAST */
void
lh_cbcast(msg)
	register message *msg;
{
	register j;
	register messDesc *predTab;
	message *lh_msg;
	register LIEN *l;
	register appliEntry *appl;
	int pos, nb_pred, entry;
	char *applName, *destName;
	bitvec convID;

	bclr(&convID);
	pos = 0;
	msg_getfld(msg, SYSFLD_NETWORK, &pos, "%-s", &destName);
	pos = 0;
	msg_getfld(msg, SYSFLD_SPSCAN, &pos, "%-s%d", &applName, &entry);
	if ((l = isAppli(applName)) == (LIEN *) 0)
		reply(msg, "%d", UNKNOWN_APPL);
	else {
		appl = (appliEntry *) l->item;
		if (!strcmp(destName, "all")) {
			pos = 1;
			msg_getfld(msg, SYSFLD_NETWORK, &pos, "%B", &convID);
			bisv(&convID, &global_convID);
			addPDESCBUF(appl, ALLCLUSTERS, &convID);
			for (j = 0; j < MAX_CLUSTERS; j++)
				if (bit(&convID, j) || (j == thisNetIndex)) {
					nb_pred = 0;
					predTab = allocDescTab(appl->PWDESC[j], &nb_pred);
					lh_msg = msg_newmsg();
					msg_put(lh_msg, "%m", msg);
					msg_putfld(lh_msg, SYSFLD_LHOPCODE, "%d", LH_CBCAST_OP);
					msg_putfld(lh_msg, LH_CB_HDR, "%d%d%s", appl->nextMessID,
						   thisNetIndex, applName);
					if (nb_pred) {
						getPred(appl, j, predTab);
						msg_putfld(lh_msg, LH_CB_HDR, "%Z", predTab,
							   nb_pred);
						free((char *) predTab);

					}
#ifdef NOSPOOLER
					lh_send(netList[j].netName, lh_msg);
#else
					msg_putfld(lh_msg, SYSFLD_SPSCAN, "%s%d", applName, entry);
					msg_putfld(lh_msg, SYSFLD_NETWORK, "%s",
						   netList[j].netName);
					spool_msg(lh_msg);
					msg_delete(lh_msg);
#endif				/* NOSPOOLER */
				}
		} else {
			j = thisNetIndex;
			if (strcmp(destName, "local")) {
				for (j = 0;
				     ((j < MAX_CLUSTERS) && strcmp(destName, netList[j].netName));
				     j++) ;
				if (j == MAX_CLUSTERS)
					reply(msg, "%d", UNKNOWN_DEST);
			}
			addPDESCBUF(appl, j, (bitvec *) 0);
			nb_pred = 0;
			predTab = allocDescTab(appl->PWDESC[j], &nb_pred);
			lh_msg = msg_newmsg();
			msg_put(lh_msg, "%m", msg);
			msg_putfld(lh_msg, SYSFLD_LHOPCODE, "%d", LH_CBCAST_OP);
			msg_putfld(lh_msg, LH_CB_HDR, "%d%d%s", appl->nextMessID, thisNetIndex,
				   applName);
			if (nb_pred) {
				getPred(appl, j, predTab);
				msg_putfld(lh_msg, LH_CB_HDR, "%Z", predTab, nb_pred);
				free((char *) predTab);

			}
#ifdef NOSPOOLER
			lh_send(netList[j].netName, lh_msg);
#else
			msg_putfld(lh_msg, SYSFLD_SPSCAN, "%s%d", applName, entry);
			msg_putfld(lh_msg, SYSFLD_NETWORK, "%s", netList[j].netName);
			spool_msg(lh_msg);
			msg_delete(lh_msg);
#endif				/* NOSPOOLER */
		}
		appl->nextMessID++;
	}
	if (enable_garbage) {
		garbage();
	}
}

/********************************
 *  Reception related procedures*
 ********************************/
int
wantedMessage(appl, waited, waiting)
	register appliEntry *appl;
	register messDesc *waited;
	register localMessDesc *waiting;
{
	register LIEN *dl;
	register waitDesc *wd;

	if (appl->waitTab[waited->senderID] < waited->messID) {
		if (db_spool)
			print("LH -->: Message <%d, %d> must wait for the message <%d, %d> \n",
			      waiting->senderID, waiting->messID, waited->senderID, waited->messID);

		wd = (waitDesc *) calloc(1, sizeof(waitDesc));
		if (wd == (waitDesc *) 0)
			panic("calloc failed \n");
		wd->waitedID = waited->messID;
		wd->lmd = waiting;
		dl = newLien(wd);
		lh_append(appl->PCWaitingMess[waited->senderID], dl);
		return (1);
	}
	return (0);
}

void
garbage()
{
	register LIEN *l, *l1, *applien;
	register PDESCBUF_item *pbuf;
	register appliEntry *appl;

	for (applien = applList->head; applien != (LIEN *) 0; applien = applien->next) {
		appl = (appliEntry *) applien->item;
		if (appl->enableGarbage) {
			l = (LIEN *) appl->PDESCBUF->head;
			while (l != (LIEN *) 0) {
				l1 = l->next;
				pbuf = (PDESCBUF_item *) l->item;
				if (!pbuf->count && !bitv(&pbuf->desc->visitState, &all_ones)) {
					/* This descriptor is alresdy seen by all existing partners 
					 */
#ifdef TRACE_GARBAGE
					print
					    ("LH -->: Freeing a message descriptor from PDESCBUF \n");
#endif				/* TRACE_GARBAGE */
					free((char *) pbuf->desc);
					free((char *) pbuf);
					lh_remove(appl->PDESCBUF, l);
				}
				l = l1;
			}
			appl->enableGarbage = 0;
		}
	}
	enable_garbage = 0;
}

void
lh_cb_deliver(ld, appl)
	register localMessDesc *ld;
	register appliEntry *appl;
{
	register LIEN *l, *l1;
	register waitDesc *wd;
	message *wan_msg;
	int cc;

	/* Notify all undeliverable messages, waiting for this message */
	l = appl->PCWaitingMess[ld->senderID]->head;
	while (l != (LIEN *) 0) {
		l1 = l->next;
		wd = (waitDesc *) l->item;
		if (ld->messID == wd->lmd->messID) {
			wd->lmd->nb_waitedMess--;
			lh_remove(appl->PCWaitingMess[ld->senderID], l);
			free((char *) wd);
			free((char *) l);
		}
		l = l1;
	}
	wan_msg = (message *) 0;
	if ((cc = msg_get(ld->mptr, "%m", &wan_msg)) != 1) {
		panic("Can't get the WAN message <cc = %d> \n", cc);
	}
#ifdef NOSPOOLER
	user_rcvMess(wan_msg);
#else
	respool(wan_msg);
#endif				/* NOSPOOLER */
	msg_delete(wan_msg);
	msg_delete(ld->mptr);
}

void
deliver_any(appl)
	register appliEntry *appl;
{
	register LIEN *l, *l1;
	register localMessDesc *ld1;

	/* Identifying the group of messages to be delivered */
      restart:
	l = appl->waitMessQPtr->head;
	while (l != (LIEN *) 0) {
		l1 = l->next;
		ld1 = (localMessDesc *) l->item;
		if (!ld1->nb_waitedMess) {
			lh_remove(appl->waitMessQPtr, l);
			lh_append(appl->deliverableQPtr, l);
		}
		l = l1;
	}
	/* Call to lh_cb_deliver for all in the deliverable queue */
	if (appl->deliverableQPtr->head != (LIEN *) 0) {
		while (appl->deliverableQPtr->head != (LIEN *) 0) {
			ld1 = (localMessDesc *) appl->deliverableQPtr->head->item;
			lh_cb_deliver(ld1, appl);
			freeLIEN(appl->deliverableQPtr, appl->deliverableQPtr->head);
			free((char *) ld1);
		}
		goto restart;
	}
}

void
lh_rcv_cbcast(lh_cb_msg)
	message *lh_cb_msg;
{
	register localMessDesc *ldesc;
	register appliEntry *app;
	register PDESCBUF_item *pbuf;
	register LIEN *dl, *dlien, *dlien1;
	register i, j;
	messDesc *d;
	int nbPred, pos, found, messID, ack, wanted;
	char *applName, *sender;

	pos = 0;
	ldesc = newLocalDesc();
	msg_get(lh_cb_msg, "%m%d%d%-s", &ldesc->mptr, &messID, &ack, &sender);
	msg_getfld(ldesc->mptr, LH_CB_HDR, &pos, "%d", &ldesc->messID);
	msg_getfld(ldesc->mptr, LH_CB_HDR, &pos, "%d", &ldesc->senderID);
	msg_getfld(ldesc->mptr, LH_CB_HDR, &pos, "%-s", &applName);
	app = 0;
	for (dl = applList->head; (dl != (LIEN *) 0); dl = dl->next) {
		app = (appliEntry *) dl->item;
		if (!strcmp(applName, app->name))
			break;
	}
	if (dl == (LIEN *) 0) {
		print("LH -->: BUG0; can't find <%s> WAN application on this cluster \n", applName);
		free((char *) ldesc);
		return;
	}
	if (!app->flag) {
		free((char *) ldesc);
		return;
	}
	if (app->waitTab[ldesc->senderID] > ldesc->messID) {
		if (db_spool)
			print("LH -->: <%d, %d> cbcast has been previously received \n",
			      ldesc->senderID, ldesc->messID);
		return;
	}
	d = (messDesc *) 0;
	nbPred = 0;
	msg_getfld(ldesc->mptr, LH_CB_HDR, &pos, "%-Z", &d, &nbPred);
#ifdef TRACE_ALL_GARBAGE
	print("LH -->: number of descriptors within the message is <%d> \n", nbPred);
	for (i = 0; i < nbPred; i++)
		printDesc(&d[i]);
#endif				/* TRACE_ALL_GARBAGE */
	for (i = 0; i < nbPred; i++) {
		found = 1;
		while ((app->rcvDesc[d[i].senderID]->head != (LIEN *) 0) && found) {
			dl = (LIEN *) app->rcvDesc[d[i].senderID]->head->item;
			pbuf = (PDESCBUF_item *) dl->item;
			if (!bitv(&pbuf->desc->visitState, &all_ones)) {
				freeLIEN(app->rcvDesc[d[i].senderID],
					 app->rcvDesc[d[i].senderID]->head);
				pbuf->count--;
				if (!pbuf->count) {
					app->enableGarbage++;
					enable_garbage++;
				}
			} else
				found = 0;
		}
		found = 0;
		for (dlien = app->rcvDesc[d[i].senderID]->head;
		     ((dlien != (LIEN *) 0) && !found); dlien = dlien1) {
			dlien1 = dlien->next;
			dl = (LIEN *) dlien->item;
			pbuf = (PDESCBUF_item *) dl->item;
			if (pbuf->desc->messID == d[i].messID) {
#ifdef TRACE_GARBAGE
				print("LH -->: descriptor <%d, %d> found in the PDESCBUF\n",
				      d[i].senderID, d[i].messID);
#endif				/* TRACE_GARBAGE */
				bandv(&pbuf->desc->visitState, &d[i].visitState);
				found = 1;
				if (!bitv(&pbuf->desc->visitState, &all_ones)
				    && (dlien == app->rcvDesc[d[i].senderID]->head)) {
					freeLIEN(app->rcvDesc[d[i].senderID], dlien);
					pbuf->count--;
					if (!pbuf->count) {
						app->enableGarbage++;
						enable_garbage++;
					}
				}
			}
		}
		if (!found && bitv(&d[i].visitState, &all_ones)) {
			if ((d[i].destID == ALLCLUSTERS) || (d[i].destID == thisNetIndex)) {
				wanted = 0;
				if (d[i].messID != ldesc->messID)
					wanted = wantedMessage(app, &d[i], ldesc);
				if (!wanted && (d[i].senderID != thisNetIndex))
					bic(&d[i].visitState, thisNetIndex);
				else if (wanted)
					ldesc->nb_waitedMess++;
			}
			if (d[i].senderID != thisNetIndex) {
#ifdef TRACE_ALL_GARBAGE
				print("LH -->: descriptor <%d, %d> added to PDESCBUF\n",
				      d[i].senderID, d[i].messID);
#endif				/* TRACE_ALL_GARBAGE */
				pbuf = newPDESCBUF();
				pbuf->desc = newDesc();
				bcopy((char *) &d[i], (char *) pbuf->desc, sizeof(messDesc));
				dl = newLien(pbuf);
				lh_append(app->PDESCBUF, dl);
				dl = newLien(app->PDESCBUF->last);
				lh_append(app->rcvDesc[d[i].senderID], dl);
				pbuf->count++;
				for (j = 0; j < MAX_CLUSTERS; j++)
					if ((j != thisNetIndex) && netList[j].hostTab[0].portNumber) {
						dl = newLien(app->PDESCBUF->last);
						lh_append(app->PWDESC[j], dl);
						pbuf->count++;
					}
			}
		}
	}
	dl = newLien(ldesc);
	app->waitTab[ldesc->senderID]++;
	if (!ldesc->nb_waitedMess) {
		if (db_spool)
			print("LH -->: The arrival message is deliverable \n");
		lh_append(app->deliverableQPtr, dl);
		deliver_any(app);
	} else {
		lh_append(app->waitMessQPtr, dl);
	}
	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(sender, netList[i].netName)) {
			comStateTab[i].oldestInMess = messID;
			comStateTab[i].lowerBound = ack;
			break;
		}
	if (enable_garbage)
		garbage();
}

/************************************************************************
 * Long haul atomic broadcast related procedures
 ************************************************************************/
/**********************
 * Sending procedure
 **********************/
/* Associated with the entry point LH_ABCAST */
void
lh_abcast(msg)
	register message *msg;
{
	char *applName, *dest;
	register LIEN *l;
	register i;
	register appliEntry *appl;
	int position, cc;
	bitvec convID;

	position = 0;
	msg_getfld(msg, SYSFLD_NETWORK, &position, "%-s", &dest);
	position = 0;
	msg_getfld(msg, SYSFLD_SPSCAN, &position, "%-s", &applName);
	if ((l = isAppli(applName)) == (LIEN *) 0)
		reply(msg, "%d", UNKNOWN_APPL);
	else if (!strcmp(dest, "all")) {
		appl = (appliEntry *) l->item;
		position = 1;
		bclr(&convID);
		if ((cc = msg_getfld(msg, SYSFLD_NETWORK, &position, "%B", &convID)) != 1) {
			bisv(&convID, &global_convID);
			bis(&convID, thisNetIndex);
			msg_putfld(msg, SYSFLD_NETWORK, "%B", convID);
		}
		for (i = 0; (i < MAX_CLUSTERS) && !bit(&convID, i); i++) ;
		msg_putfld(msg, SYSFLD_LHOPCODE, "%d", LH_ABCAST_OP);
		msg_putfld(msg, LH_CB_HDR, "%d%d%s", appl->nextMessID, thisNetIndex, applName);
		msg_replacefield(msg, SYSFLD_NETWORK, netList[i].netName, FTYPE_CHAR,
				 strlen(netList[i].netName) + 1);
#ifdef NOSPOOLER
		lh_send(netList[i].netName, msg);
#else
		spool_msg(msg);
#endif				/* NOSPOOLER */
		appl->nextMessID++;
	} else
		lh_cbcast(msg);
}

/***********************
 * receiving procedure
 ***********************/
void
lh_rcv_abcast(lh_ab_msg)
	register message *lh_ab_msg;
{
	message *msg;
	int messID, ack, i;
	char *sender;

	msg_get(lh_ab_msg, "%m%d%d%-s", &msg, &messID, &ack, &sender);
	msg_replacefield(msg, SYSFLD_NETWORK, "all", FTYPE_CHAR, strlen("all") + 1);
	lh_cbcast(msg);
	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(sender, netList[i].netName)) {
			comStateTab[i].oldestInMess = messID;
			comStateTab[i].lowerBound = ack;
			break;
		}
	msg_delete(msg);
}

void
comstate_update(m)
	register message *m;
{
	message *msg;
	int messID, ack, i;
	char *sender;

	msg_get(m, "%d%d%-s", &messID, &ack, &sender);
	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(sender, netList[i].netName)) {
			comStateTab[i].oldestInMess = messID;
			comStateTab[i].lowerBound = ack;
			if (db_spool)
				print("comstate_update: set input messageID to %d ack %d\n", messID,
				      ack);
			break;
		}
	msg_delete(m);
}

/************************************************************************
 * Long haul File transfer protocol
 ***********************************************************************/

void
lh_setsrcerr(msg)
	register message *msg;
{
	char *gname;
	int entry;

	if (msg_get(msg, "%-s%d", &gname, &entry) != 2) {
		return;
	}
	strcpy(srcerrgroup, gname);
	srcerrentry = entry;
}

void
lh_setdsterr(msg)
	register message *msg;
{
	char *gname;
	int entry;

	if (msg_get(msg, "%-s%d", &gname, &entry) != 2) {
		return;
	}
	strcpy(dsterrgroup, gname);
	dsterrentry = entry;
}

/******************************
 * Sending related procedures
 *****************************/
void
do_lh_spool(msg, net, opcode)
	register message *msg;
	register char *net;
	register int opcode;
{
	register j;

	msg_putfld(msg, SYSFLD_LHOPCODE, "%d", opcode);
	if (!strcmp(net, "all")) {
		for (j = 0; j < MAX_CLUSTERS; j++)
			if ((j != thisNetIndex) && (netList[j].hostTab[0].portNumber)) {
				message *mc = msg_copy(msg);

				msg_replacefield(mc, SYSFLD_NETWORK, netList[j].netName,
						 FTYPE_CHAR, strlen(netList[j].netName) + 1);
				spool_msg(mc);
				msg_delete(mc);
			}
		msg_replacefield(msg, SYSFLD_NETWORK, netList[thisNetIndex].netName,
				 FTYPE_CHAR, strlen(netList[thisNetIndex].netName) + 1);
		spool_msg(msg);
	} else {
		if (!strcmp(net, "local"))
			msg_replacefield(msg, SYSFLD_NETWORK, netList[thisNetIndex].netName,
					 FTYPE_CHAR, strlen(netList[thisNetIndex].netName) + 1);
		spool_msg(msg);
	}
}

void
lh_remote_file_xfer(mp)
	register message *mp;
{
	int pos = 0;
	char *net;

	msg_getfld(mp, SYSFLD_NETWORK, &pos, "%-s", &net);
	do_lh_spool(mp, net, LH_XFER_OP);

}

void
do_send_file(netName, lh_msg, path, dest)
	register char *netName;
	register message *lh_msg;
	register char *path, *dest;
{
	FILE *fdesc;
	int sz, currentOfset, continuer, ch_size;
	char *buffer;
	message *frag_msg;

	ch_size = MAX_MESSAGE_SIZE;
	buffer = (char *) calloc(1, ch_size);
	if ((fdesc = fopen(path, "r")) == NULL) {
		print("LH: --> Can't open the file <%s>", path);
		perror("");
		return;
	}
	if (db_spool)
		print("LH: --> do_send_file invoked for <%s> \n", path);
	do_lh_send(netName, lh_msg, 0);
	if (strcmp(netName, netList[thisNetIndex].netName)) {
		continuer = 1;
		currentOfset = 0;
		while (continuer) {
			frag_msg = msg_newmsg();
			sz = fread(buffer, 1, ch_size, fdesc);
			if (db_spool)
				print("LH: --> fread offset %d length %d\n", currentOfset, sz);
			msg_putfld(frag_msg, SYSFLD_LHOPCODE, "%d", RCV_FRAG_OP);
			msg_putfld(frag_msg, SYSFLD_FILE, "%s%s", path, dest);
			msg_putfld(frag_msg, FRAG_DATA_FLD, "%d%C", currentOfset, buffer, sz);
			do_lh_send(netName, frag_msg, 1);
			if (sz < ch_size)
				continuer = 0;
			else
				currentOfset += sz;
			msg_delete(frag_msg);
		}
	}
	free(buffer);
	fclose(fdesc);
}

/*******************************
 * Receiving related procedures
 *******************************/

void
destfile(s, d)
	char *s, *d;

/* create dest file name from source file name */
/* if source is absolute, leave it alone */
/* otherwise, use /tmp/LASTCOMOPNENT */

{
	if ((s == 0) || (*s == 0))
		panic("LH: bad dest file name");
	if (s[0] == '/') {
		strcpy(d, s);
		return;
	} else {
		char *p;

		strcpy(d, "/tmp/");
		p = (char *) strrchr(s, '/');
		if (p == 0)
			p = s - 1;
		strcat(d, p + 1);
	}
}

void
rcvFileHdr(msg, sender)
	register message *msg;
	register char *sender;
{
	int position = 0;
	int opcode, messID, ack, op;
	LIEN *l;
	rcvFileDesc *fdesc;
	char hostName[40], *rabsname, *rname, *source;

	msg_getfld(msg, SYSFLD_FILE, &position, "%-s%-s%d", &source, &rabsname, &op);
	if (strcmp(sender, netList[thisNetIndex].netName)) {
		fdesc = (rcvFileDesc *) calloc(1, sizeof(rcvFileDesc));
		if (fdesc == (rcvFileDesc *) 0)
			panic("LH -->: Can't allocate a rcvFileDesc structure \n");
		fdesc->ch_size = MAX_MESSAGE_SIZE;
		fdesc->errno = 0;
		fdesc->sender = sender;
		fdesc->op = op;
		destfile(rabsname, fdesc->name);
		unlink(fdesc->name);
		if ((fdesc->fdesc = fopen(fdesc->name, "w")) == NULL)
			fdesc->errno = errno;
		else if (db_spool)
			print("LH: created file %s\n", fdesc->name);
		msg_increfcount(msg);
		fdesc->lh_msg = msg;
		l = newLien(fdesc);
		lh_append(filesList, l);
	} else {
		gethostname(hostName, 40);
		msg_putfld(msg, SYSFLD_LH_FILEREP, "%s%s", source, hostName);
		position = 0;
		msg_getfld(msg, SYSFLD_LHOPCODE, &position, "%d", &opcode);
		position = 0;
		msg_getfld(msg, LH_HDR, &position, "%d%d", &messID, &ack);
		cbcast(interLanGaddr, LH_XFER_REQUEST, "%d%d%s%m", messID, ack, sender, msg, 0);
	}
}

void
rcvFragment(msg, sender)
	register message *msg;
	register char *sender;
{
	register LIEN *l;
	rcvFileDesc *fdesc = 0;
	char *fname, *frag_data, *cptr1, *cptr2, *dest, *rname;
	int ofset, frag_size, position = 0;
	message *lh_msg, *ack_msg;
	int opcode, messID, ack;
	char hostName[40], buf[500];

	msg_getfld(msg, SYSFLD_FILE, &position, "%-s%-s", &fname, &dest);
	destfile(dest, buf);
	l = filesList->head;
	while (l != (LIEN *) 0) {
		fdesc = ((rcvFileDesc *) l->item);
		if (!strcmp(fdesc->name, buf))
			break;
		else
			l = l->next;
	}
	if (l == (LIEN *) 0)
		panic("LH -->: Can't find <%s> file descriptor \n", fname);
	position = 0;
	msg_getfld(msg, FRAG_DATA_FLD, &position, "%d%-C", &ofset, &frag_data, &frag_size);
	if (fdesc->fdesc != NULL) {
		if (db_spool)
			printf("LH -->: fwrite called for frag <ofset: %d, size: %d> \n", ofset,
			       frag_size);
		if (fwrite(frag_data, 1, frag_size, fdesc->fdesc) != frag_size) {
			fclose(fdesc->fdesc);
			fdesc->fdesc = NULL;
			fdesc->errno = errno;
			if (db_spool)
				print("WARNING: fwrite failed (errno %d)\n", errno);
		}
	}
	if (frag_size < fdesc->ch_size) {
		/* last fragment; send local notification if successful, and send the ack back */
		lh_msg = fdesc->lh_msg;
		gethostname(hostName, 40);
		msg_putfld(lh_msg, SYSFLD_LH_FILEREP, "%s%s", fdesc->name, hostName);
		position = 0;
		msg_getfld(lh_msg, LH_HDR, &position, "%d%d", &messID, &ack);
		if (fdesc->fdesc != NULL) {
			position = 0;
			msg_getfld(lh_msg, SYSFLD_LHOPCODE, &position, "%d", &opcode);
			cbcast(interLanGaddr, LH_XFER_REQUEST, "%d%d%s%m",
			       messID, ack, sender, lh_msg, 0);
		} else {
			/* send an error notification to the user error handler, if any */
			cbcast(interLanGaddr, LH_XFER_RCV_FAILURE, "%s%s%s%d",
			       fname, fdesc->name, sender, fdesc->errno, 0);
		}
		ack_msg = msg_newmsg();
		msg_putfld(ack_msg, SYSFLD_NETWORK, "%s", fdesc->sender);
		msg_putfld(ack_msg, SYSFLD_SPSCAN, "%s%d", "", WAN_DUMMY_ENTRY);
		msg_putfld(ack_msg, SYSFLD_LHOPCODE, "%d", LH_XFER_ACK);
		msg_put(ack_msg, "%s%s%d%d%d", fname, dest, fdesc->errno, messID, fdesc->op, 0);
		if (db_spool)
			print
			    ("LH -->: spooling message to %s (%s): errno %d messID %d od %d (sender %s)\n",
			     fname, dest, fdesc->errno, messID, fdesc->op, fdesc->sender);
		spool_msg(ack_msg);
		msg_delete(ack_msg);
		if (fdesc->fdesc != NULL) {
			fclose(fdesc->fdesc);
		}
		msg_delete(lh_msg);
		freeLIEN(filesList, l);
	}
}

void
lh_rcv_remote_file(lh_xfer_msg)
	register message *lh_xfer_msg;
{
	message *lh_msg;
	char *sender;
	int messID, ack, i;

	msg_get(lh_xfer_msg, "%d%d%-s%m", &messID, &ack, &sender, &lh_msg);
	respool(lh_msg);
	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(sender, netList[i].netName)) {
			comStateTab[i].oldestInMess = messID;
			comStateTab[i].lowerBound = ack;
			break;
		}
	msg_delete(lh_msg);
}

void
lh_rcv_failure(m)
	register message *m;
{
	if (dsterrentry != -1) {
		msg_putfld(m, SYSFLD_SPSCAN, "%s%d", dsterrgroup, dsterrentry);
		respool(m);
	}
	msg_delete(m);
}

void
lh_snd_failure(m)
	register message *m;
{
	if (srcerrentry != -1) {
		msg_putfld(m, SYSFLD_SPSCAN, "%s%d", srcerrgroup, srcerrentry);
		respool(m);
	}
	msg_delete(m);
}

/***********************************************************
 * Long haul spool entry point (instead of spool in spooler)
 ***********************************************************/
void
lh_spool(mp)
	register message *mp;
{
	int cc, pos = 0;
	char *net;

	if ((cc = msg_getfld(mp, SYSFLD_NETWORK, &pos, "%-s", &net)) == 1) {
		do_lh_spool(mp, net, RESPOOL_OP);
	} else {
		spool_msg(mp);
	}
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 *                 INTERFACE WITH THE SPOOLER
 *::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
void
startReplay(netIndex)
	register netIndex;
{
	int cc = 0;

	localreplay(netList[netIndex].netName, lh_send);
	if (cc = allConReady()) {
		t_sig(&connectionsAllReady, (VOID *) 1);
	}
}

void
stopReplay(partner)
	register char *partner;
{
	message *mp;
	register i;

	for (i = 0; i < MAX_CLUSTERS; i++)
		if (!strcmp(partner, comStateTab[i].partnerNet))
			break;
	if (i < MAX_CLUSTERS) {
		register s;

		mp = msg_gen("%s%d", partner, comStateTab[i].lowerBound);
		s = DEC(seqentry);
		outOFband = oobseq[s];
		h_spool_set_replay_pointer(mp);
		msg_rewind(mp);
		spool_set_chkpt_pointer(mp);
	} else
		panic("LH -->: Can't find the partner to stop in the comState table \n");

	localreplay(partner, NULLROUTINE);
}

#ifdef NOSPOOLER
/* 
 * The long haul package will not use the spooling mechanisms if compiled with -DNOSPOOLER.
 * In this case, pm_rcvMess() is a server procedure; it is called whenever a message is to be delivered.
 * currentMessageID with be used as counter of inter-clusters messages.
 */
int
localreplay(gp, rcvProc)
	char *gp;
	vfunc *rcvProc;
{
}

int
spool(msg)
	message *msg;
{
}

int
h_spool_set_replay_pointer(msg)
	message *msg;
{
}

int
spool_set_chkpt_pointer(msg)
	message *msg;
{
}

int
respool(msg)
	message *msg;
{
}
#endif				/* NOSPOOLER */

int
Connect(s, name, namelen)
	int s;
	struct sockaddr *name;
	int namelen;
{
	int i;

      again:
	i = connect(s, name, namelen);
	if (i == -1) {
		if (db_spool) {
			perror("long_haul connect");
		}
		if (errno == EINTR) {
			goto again;
		}
	}
	return i;
}

int
Listen(s, backlog)
	int s, backlog;
{
	int i;

      again:
	i = listen(s, backlog);
	if (i == -1) {
		if (db_spool) {
			perror("long_haul listen");
		}
		if (errno == EINTR) {
			goto again;
		}
	}
	return i;
}

int
Socket(domain, type, protocol)
	int domain, type, protocol;
{
	int i;

      again:
	i = socket(domain, type, protocol);
	if (i == -1) {
		if (db_spool) {
			perror("long_haul scoket");
		}
		if (errno == EINTR) {
			goto again;
		}
	}
	return i;
}

int
Accept(s, addr, addrlen)
	int s;
	struct sockaddr *addr;
	int *addrlen;
{
	int i, j = *addrlen;

      again:
	*addrlen = j;
	i = accept(s, addr, addrlen);
	if (i == -1) {
		if (db_spool) {
			perror("long_haul accept");
		}
		if (errno == EINTR) {
			goto again;
		}
	}
	return i;
}

int
Bind(s, name, namelen)
	int s;
	struct sockaddr *name;
	int namelen;
{
	int i;

      again:
	i = bind(s, name, namelen);
	if (i == -1) {
		if (db_spool) {
			perror("long_haul bind");
		}
		if (errno == EINTR) {
			goto again;
		}
	}
	return i;
}
