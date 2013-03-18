/*  $RCSfile: pr_msgfields.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:56 $  */
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
/* Names for message fields used by protos code */      
        
#ifndef PR_MSGFIELDS    
#define PR_MSGFIELDS    
        
/* Used for client->isis communication */
#define CL_ALEN                 1
#define CL_ALIST                2
#define CL_ENTRY                3
#define CL_GID                  4
#define CL_MSG                  5
#define CL_NEWPNAME             6
#define CL_NWANT                7
#define CL_OLDPNAME             8
#define CL_PID                  9
#define CL_PNAME                10
#define CL_SIGNO                11
#define CL_SITES                12
#define CL_VIEW                 13
#define CL_GNAME                14
#define CL_EVENT                15
#define CL_NEWVIEW              16
#define CL_RESTARTMODE          17
#define CL_COORD                18
#define CL_VIEWID               19
#define CL_INCARN               20
#define CL_FILE                 21
#define CL_RLIST                22
#define CL_NEW_SNAMES           23
#define CL_HOW                  24
#define SYSFLD                  25

/* Used for isis->isis communication */
#define ADDR_GID                (SYSFLD+1)
#define ADDR_ADDRESS            (SYSFLD+2)
        
#define FLD_ADDCACHE            (SYSFLD+3)
#define FLD_MSGID               (SYSFLD+4)
#define FLD_MSG                 (SYSFLD+5)
#define FLD_PRIORITY            (SYSFLD+6)
#define FLD_PIGGYLIST           (SYSFLD+7)
#define FLD_REMDESTS            (SYSFLD+8)
#define FLD_RECEIVED            (SYSFLD+9)
#define FLD_ISNULLREP           (SYSFLD+10)
#define FLD_BEFORE              (SYSFLD+11)
#define FLD_ANSW                (SYSFLD+12)
#define FLD_NDESTS              (SYSFLD+13)
#define FLD_NSENT               (SYSFLD+14)
#define FLD_VIEWID              (SYSFLD+15)
#define FLD_REJECT              (SYSFLD+16)
#define FLD_GID                 (SYSFLD+17)
#define FLD_FAILED              (SYSFLD+18)
#define FLD_TRUESENDER          (SYSFLD+19)
#define FLD_SLIST               (SYSFLD+20)
#define FLD_FDTYPE              (SYSFLD+21)
        
#define GMGR_PNAME              (SYSFLD+22)
#define GMGR_GNAME              (SYSFLD+23)
#define GMGR_MODE               (SYSFLD+24)

#define AS_IGNORES              (SYSFLD+25)
        
#define INTERSITE_HDR           (SYSFLD+28)
#define INTERSITE_MSG           (SYSFLD+29)
#define INTERSITE_FHDR          (SYSFLD+30)
#define INTERSITE_FRAG          (SYSFLD+31)
#define INTERSITE_DEBUG         (SYSFLD+32)
        
#define FLD_OLDVIEWID           (SYSFLD+33)
#define FLD_OLDSLIST            (SYSFLD+34)
#define FLD_DEADLIST            (SYSFLD+35)

#define FLD_ISABORTREP          (SYSFLD+36)

#define FLD_PIGGYFLAG           (SYSFLD+37)

#define INTERCLIENT_HDR         (SYSFLD+38)
#define INTERCLIENT_MSG         (SYSFLD+39)
#define INTERCLIENT_FHDR        (SYSFLD+40)
#define INTERCLIENT_FRAG        (SYSFLD+41)
        
#endif  
