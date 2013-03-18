/*  $RCSfile: cl_cmd.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:20:42 $  */
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
#define MSG_COMMAND  0

#define FLD_ARGV    1

#define getargs(mp,argc,argv,argmax)  \
        { argc = msg_getfields(mp, FLD_ARGV, (char **) argv, NULL, argmax-1); \
          argv[argc] = NULL;  \
        }
