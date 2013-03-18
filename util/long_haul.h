/*  $RCSfile: long_haul.h,v $ $Revision: 2.33 $ $Date: 90/09/11 14:42:45 $  */
#ifndef _LHHDR_
#define _LHHDR_ 1
/*
 *      New ISIS Long-Haul server header file
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
 */

/************************************************************************************
 *  LONG HAUL CONSTANTES DEFINITION
 ************************************************************************************/
#define INTERLANGNAME              "@*:interLanGroup"
#define MAX_CLUSTERS               5
#define MAX_ACCESS_POINTS          5
#define MAX_WAN_APPL               10
#define MAXNAMELENGTH              PG_GLEN
#define STARTUPDELAY               10000
#define retryDelay                 10000
#define ALLCLUSTERS                -1     /* Use to request a cbcast to all clusters */
#define localNet                   "local"

/* Long haul message specific fields values */
#define LH_HDR                     1  /* Contains the hdr of any cross network message */
#define LH_CB_HDR                  2  /* Contains LH_CBCAST specific info */
#define FRAG_DATA_FLD              3  /* Contains a fragment's size and data */

/* Long haul opcodes */
#define INITOP                     1  /* Initializes a long-haul connection */
#define RESPOOL_OP                 2  /* Request a respool of the long-haul message */
#define CREATEWSPOOL_OP            3  /* Request the creation of a wide area spool */
#define LH_CBCAST_OP               4  /* Request a long haul cbcast treatment */
#define RCV_FRAG_OP                5  /* Receive a fragment from a partner */
#define LH_XFER_OP                 6  /* Request the reception of a remote file */
#define LH_ABCAST_OP               7  /* Request a long haul abcast treatment */
#define LH_XFER_ACK                8  /* acknowledges a file transfer */

/* Long haul errors code */
#define UNKNOWN_APPL               -11 /* Application not found in the application list */
#define UNKNOWN_DEST               -12 /* Unknown cluster name provided as a destination */
#define INV_XFER_REQ               -13 /* The file transfert request is invalid */

/* Long haul protocols constantes */
#define SP_MESSID                   5  /* To be used while respooling a cbcast at destination */
#define SP_SENDERID                 6  /* To be used while respooling a cbcast at destination */
#define messDesc_typeno             1  /* Type number of the messDesc (used in pc_cbcast message */
#define messDesc_format             'z' /* Character format for the messDesc type (used in pc_bcast) */
#define WAN_DUMMY_ENTRY            10  /* used while spooling for the wanManager */

#define MAX_MESSAGE_SIZE           8192 /* Used by the file transfert protocol */
#define MAX_FILE_NAME              128  /* Used by the file transfert protocol (right value to be defined) */

/***************************************************************************************
 * LONG HAUL TYPES SPECIFICATION
 ***************************************************************************************/
/* This structure specifies a run time description of an internet host. 
 * hostAddr specifies the internet  address of the concerned host.
 * portNumber is a tcp port where connection requests are awaiting.
 */
struct hostDesc
{
  struct in_addr hostAddr;
  int portNumber;
};

/* This structure describes the run-time  view of an isis logical net (or cluster).
 * netName is the isis  cluster name.
 * hostTab is the list of at most MAX_ACCESS_POINTS internet hosts' descriptors.
 */
struct r_netView 
{
  char netName [MAXNAMELENGTH];
  struct hostDesc hostTab[MAX_ACCESS_POINTS];
};

/* This structure describes an incoming / outgoing connection.
 * sock is the socket identifier allocated to this connection.
 * wid is the wait identifier associated with this socket. ... v2 ...
 * hostIndex is the index within hostTab of the remote net's hosts list.
 * partner is the remote network name.
 */
struct conDesc
{
  int sock;
  int wid;
  int hostIndex;
  char partner[MAXNAMELENGTH];
};

/* The backupCon structure describes an incoming connection with can be used 
 * as a backup of an outgoing connection.
 * sock is the socket allocated for this backup connection.
 * hostIndex is  the initializer's host index within its net.
 * procAddr is the address of the member in charge of this incoming connection.
 */
struct backupCon
{
  int hostIndex;
  address procAddr;
  struct conDesc *con;
};

/* Precises the number of nets affected to each runing member.
 * procAddr is the member's address.
 * weight is the number of nets with which this member communicates.
 */
struct memberLoad
{
  address procAddr;
  int weight;
};

/*Describes the state of communication between the local net and a remote one.
 * partnerNet is this partner's net name.
 * oldestInMess is the greatest message ID received from this remote net.
 * lowerBound is the greatest message ID already acknowledged by this partner.
 */
struct comState
{
  char partnerNet[MAXNAMELENGTH];
  int oldestInMess;
  int lowerBound;
};

/************************************************************************************
 *  MAIN STRUCTURES USED FOR THE LONG HAUL PROTOCOLS
 ************************************************************************************/
struct lien
{
  VOID *item;                /* Refers message descriptor */
  struct lien *next;         /* Refers the next item of the list */
  struct lien *previous;     /* Refers the previous item of the list */
};
typedef struct lien LIEN;
struct queueDesc
{
  LIEN *head;                /* Refers the first item of the list */
  LIEN *last;                /* Refers the last item of the list */
};
typedef struct queueDesc LISTE;
struct messDesc
{
  long messID;               /* Unique message idenfier */
  long senderID;            /* The sender cluster ID */
  long destID;              /* If non-zero, specifies the destinator's cluster ID */
  bitvec visitState;         /* Specifies yet unvisited clusters by this descriptor*/
};
typedef struct messDesc messDesc;
struct PDESCBUF_item
{
  int count;               /* reference counter */
  messDesc *desc;            /* message descriptor associated with this item */
};
typedef struct  PDESCBUF_item PDESCBUF_item;
struct localMessDesc
{
  long nb_waitedMess;        /* Number of messages preceding this delivery */
  long messID;               /* The ID of the awaiting message */
  long senderID;             /* The sender of the awaiting message */
  message *mptr;             /* Message containing the long haul message */
};
typedef struct localMessDesc localMessDesc;
struct waitDesc
{
  long waitedID;             /* ID of the waited message from the partener */
 localMessDesc *lmd;          /* Local descriptor of the awaiting  message */
};
typedef struct waitDesc waitDesc;
struct appliEntry
{
  char name[PG_GLEN];                 /* The application's name */
  int enableGarbage;                  /* When set, means some descriptors should be collected */
  long nextMessID;                    /* The message identifier for the next message to be sent */
  int waitTab[MAX_CLUSTERS];          /* Table of expected messID from partners */
  LISTE *waitMessQPtr;                /* Refers the queue of undeliverable messages */
  LISTE *deliverableQPtr;             /* refers the deliverable list of descriptors */
  LISTE *PDESCBUF;                    /* Refers the PDESCBUF of all known descriptors */
  LISTE *PWDESC[MAX_CLUSTERS];        /* awaiting lists of descriptors to be sent to partners */
  LISTE *rcvDesc[MAX_CLUSTERS];       /* list of descriptors already accepted from partners */
  LISTE *PCWaitingMess[MAX_CLUSTERS]; /* Per Cluster  Waiting messages list */
  int appState;                       /* Current state of this application */
  int flag;                           /* Set if this descriptor's holder has a valid copy */
  address has_copy[MAX_SITES];        /* Table of members holding  this application's info */
};
typedef struct appliEntry appliEntry;
struct rcvFileDesc
{
  char name[MAX_FILE_NAME];    /* Local name of the file being received */
  FILE  *fdesc;                /* Stream associated to the received file */
  message *lh_msg;             /* Holds the initial message until all the file has been received */
  char *sender;                /* net the file is coming from */
  int messID;                  /* User message ID of the header message */
  int ch_size;                 /* Chunck used by the long haul for xfer */
  int errno;                   /* records open/write errors, etc */
  int op;                      /* move or copy??? */
};
typedef struct rcvFileDesc rcvFileDesc;

/*************************************************************************************
 * EXTERNAL  VARIABLES AND FUNCTIONS
 *************************************************************************************/
extern bitvec all_ones;
extern int db_spool;
#ifdef __STDC__
extern void replay(message*);
extern int spool_msg(message*);
extern int respool(message*);
extern int localreplay(char*, vfunc*);
extern int h_spool_set_replay_pointer(message*);
extern int spool_set_chkpt_pointer(message*);
extern int allConReady(), isClusterName(char*);
extern void long_haul_init(FILE*), initializer();
extern void sendBackupList(int, address*),  sendWeightTab(int, address*);
extern void sendAffectation(int, address*), sendComState(int, address*);
extern void sendApplList(int, address*);
extern void rcvAffectation(int, message*), rcvComState(int, message*);
extern void rcvBackupList(int, message*), rcvWeightTab(int, message*);
extern void rcvApplList(int, message*);
extern void sendStartup();
extern void registerCon(message*), updateComState(message*);
extern void updateLoad(message*), updateGView(groupview*, int);
extern void updateOldestInMess(message*), updateLowerBound(message*);
extern void receiveStartup(message*), treatPartnerFailure(message*);
extern void dispatcherProc(), establishier(int), acceptProc();
extern void lh_send(char*, message*);
extern void do_lh_send(char*, message*, int);
extern void lh_receive(struct conDesc*);
extern void lh_rcv_join(message*);
extern void signalConFailure(int, int, int), startReplay(int), stopReplay(char*);
extern int lhmsg_isValid(int, char*),  sendInitialMess(int, int);
extern void lh_rcv_cbcast(message*);
extern void set_join(message*);
extern void lh_cbcast(message*);
extern void lh_abcast(message*);
extern void lh_rcv_abcast(message*);
extern void lh_spool(message*);
extern void do_send_file(char*, message*, char*, char*);
extern void localjoin(char*);
extern void treatApplMgrFailure();
extern void garbage();
extern void PDESCBUF_converter(messDesc*);
extern LIEN *newLien(VOID*);
extern LISTE *newListe();
extern messDesc *newDesc();
extern localMessDesc *newLocalDesc();
extern PDESCBUF_item *newPDESCBUF();
extern void lh_append();
extern void lh_remove(LISTE*, LIEN*);
extern void freeLIEN(LISTE*,LIEN*);
extern messDesc *allocDescTab(LISTE*,int*);
extern void printDesc(messDesc*);
extern appliEntry *newEntry(char*);
extern void freeEntry(appliEntry*);
extern void rcvFileHdr(message*, char*);
extern void rcvFragment(message*, char*);
#else
extern int replay();
extern int respool(); 
extern int localreplay();
extern int h_spool_set_replay_pointer();
extern int spool_set_chkpt_pointer();
extern int allConReady(), isClusterName();
extern void long_haul_init(), initializer(), sendBackupList(),  sendWeightTab();
extern void sendAffectation(), sendStartup(), rcvBackupList(), rcvWeightTab(), sendApplList();
extern void rcvAffectation(), rcvComState(), rcvBackList(), rcvWeightTab(), rcvApplList();
extern void updateLoad(), updateGView(), updateOldestInMess(), updateLowerBound();
extern void receiveStartup(), treatPartnerFailure(), sendComState();
extern void establishier(), acceptProc(), lh_send(), do_lh_send(), lh_receive(), lh_rcv_join();
extern void signalConFailure(), startReplay(), dispatcherProc(), stopReplay();
extern int lhmsg_isValid(),  sendInitialMess();
extern void treatApplMgrFailure();
extern void PDESCBUF_converter();
extern void lh_rcv_cbcast();
extern void set_join();
extern void lh_cbcast();
extern void lh_abcast();
extern void lh_rcv_abcast();
extern void lh_spool();
extern void do_send_file();
extern void localjoin();
extern void garbage();
extern void PDESCBUF_converter();
extern LIEN *newLien();
extern LISTE *newListe();
extern messDesc *newDesc();
extern localMessDesc *newLocalDesc();
extern PDESCBUF_item *newPDESCBUF();
extern void lh_append();
extern void lh_remove();
extern void freeLIEN();
extern messDesc *allocDescTab();
extern void printDesc();
extern appliEntry *newEntry();
extern void freeEntry();
extern void rcvFileHdr();
extern void rcvFragment();
#endif __STDC__
#endif _LHHDR_

