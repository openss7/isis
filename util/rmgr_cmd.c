/*
 *	By Frank Schmuck
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
/*********************************************************************
*
*  rmupdate -- update an entry in rmgr.rc file
*
*********************************************************************/

char  rmgr_cmd_rcsid[] = "$Revision: 2.0 $$Date: 90/05/04 15:24:10 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/rmgr_cmd.c,v $";
#include <stdio.h>
#include "isis.h"

void  usage(s)
    char  *s;
{
    fprintf(stderr, "usage: %s site [-E] key [program arg0 arg1 ...]\n", s);
    exit (-1);
}

main(argc, argv, envp)
    int  argc;
    char *argv[], *envp[];
{
    char  *key, *program, **args;
    int   rc;

    if (argc < 3) usage(argv[0]);
    my_site_no = atoi(argv[1]);
    if (my_site_no == 0) usage(argv[0]);
    if (strcmp(argv[2], "-E") == 0) {
        if (argc < 5) usage(argv[0]);
        key = argv[3];
        program = argv[4];
        args = &argv[5];
    } else {
        key = argv[2];
        program = argv[3];
        args = (program != NULL)? &argv[4]: NULL;
        envp = NULL;
    }

    if (strlen(key) > RMLEN) {
        fprintf(stderr, "%s: key too long\n", key);
        exit (-1);
    }
    isis_init(0);
    switch (rc = rmgr_update(key, program, args, envp)) {
    case RM_ELOCKED:
        fprintf(stderr, "%s: rmgr.rc file locked\n", argv[0]);
        break;
    case RM_ENOTFOUND:
        fprintf(stderr, "%s: key not found in rmgr.rc file\n", argv[0]);
        break;
    }
    exit(rc);
}
