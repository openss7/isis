/*  $RCSfile: cl_print.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:19 $  */
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
#include <stdio.h>

void
isis_print(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
  char * fmt;
  int a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
  {
        extern FILE *isis_outfile;
        if(isis_outfile)
        {
            fprintf(isis_outfile, fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
            fflush(isis_outfile);
        }
        else
        {
            fprintf(stdout, fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
            fflush(stdout);
        }
  }
