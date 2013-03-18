/*  $RCSfile: pr_errors.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:14 $  */
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
/* Error codes */

#ifdef  ISIS_ERRORS
static  char *IE_errors[] ={
        "<< no error >>",                                       /* 0 */
        "group unknown",                                        /* IE_UNKNOWN */
        "restricted group address",                             /* IE_RESTRICTED */
        "dest list too long",                                   /* IE_TOOLONG */
        "not known locally",                                    /* IE_NOTLOCAL */
        "bad argument",                                         /* IE_BADARG */
        "total failure",                                        /* IE_TOTFAIL */
        "not implemented",                                      /* IE_NOTIMP */
        "try again?",                                           /* IE_AGAIN */
        "not allowed",                                          /* IE_NOTALLOWED */
        "connect failed",                                       /* IE_CONNECT */
        "broken connection",                                    /* IE_BROKEN */
        "bad format item",                                      /* IE_BADFITEM */
        "missmatch",                                            /* IE_MISSMATCH */
        "not member",                                           /* IE_NOTMEMB */
        "illegal use of bcast guard",                           /* IE_GUARD */
        "guard parsing problem",                                /* IE_GPARSE */
        "must join but group hasn't been created yet",          /* IE_MUSTJOIN */
        "the specified group is not logged",                    /* IE_NOTLOGGED */
        "the specified group is already logged",                /* IE_RELOG */
        "no log checkpoint exists for that group",              /* IE_NOCKPT */
        "I/O error on log, see errno",                          /* IE_LOGIO */
        "abortreply received",                                  /* IE_ABORT */
        "nested transactions not allowed",                      /* IE_NESTEDTRANS*/
        "no such transaction",                                  /* IE_NOTRANS */
        "already a participant of this transaction",            /* IE_PARTICIPANT*/
        "cannot contact transaction recover manager",           /* IE_NOTRANSRECOV */
        "length mismatch",                                      /* IE_WRONGLEN */
	"coordinator refused",					/* IE_REFUSED */
	"empty plist",						/* IE_EMPTY */
	"old format log file",					/* IE_OLDLOG */
};
#endif


#define IE_UNKNOWN               -1
#define IE_RESTRICTED            -2
#define IE_TOOLONG               -3
#define IE_NOTLOCAL              -4
#define IE_BADARG                -5
#define IE_TOTFAIL               -6
#define IE_NOTIMP                -7
#define IE_AGAIN                 -8
#define IE_NOTALLOWED            -9
#define IE_CONNECT               -10
#define IE_BROKEN                -11
#define IE_BADFITEM              -12
#define IE_MISSMATCH             -13
#define IE_NOTMEMB               -14
#define IE_GUARD                 -15
#define IE_GPARSE                -16
#define IE_MUSTJOIN              -17
#define IE_NOTLOGGED             -18
#define IE_RELOG                 -19
#define IE_NOCKPT                -20
#define IE_LOGIO                 -21
#define IE_ABORT                 -22
#define IE_NESTEDTRANS           -23
#define IE_NOTRANS               -24
#define IE_PARTICIPANT           -25
#define IE_NOTRANSRECOV          -26
#define IE_WRONGLEN              -27
#define IE_REFUSED               -28
#define IE_EMPTY                 -29
#define IE_OLDLOG		 -30
