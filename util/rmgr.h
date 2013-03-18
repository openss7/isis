/*  $RCSfile: rmgr.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:08 $  */
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
 */
/* rmgr.h:  recovery manager */

#define MAXNAMELEN      128

char    sbuf1[MAXNAMELEN], sbuf2[MAXNAMELEN], sbuf3[MAXNAMELEN], sbuf4[MAXNAMELEN];

char    *rmgrbuf_init();

#define RMGRDIR  rmgrbuf_init(sbuf1, "%s/rmgr_dir", isis_dir)
#define RMGRRC   rmgrbuf_init(sbuf2, "%s/rmgr.rc", isis_dir)
#define RMGRRC2  rmgrbuf_init(sbuf3, "%s/rmgr.rc2", isis_dir)
#define RMGRLOCK rmgrbuf_init(sbuf4, "%s/rmgr.lock", isis_dir)

#define MSG_LASTVIEW    1
#define MSG_REGISTER    2
#define MSG_UNREGISTER  3
