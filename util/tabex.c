/*  $RCSfile: tabex.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:16 $  */
/*
 *	By Ken Birman
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
/* Expand tabs into spaces, because of lousy emacs conventions */
main()
  {
        register c, n = 0;

        while((c = getchar()) > 0)
        {
            switch(c)
            {
                case '\n':
                    n = 0;
                    break;
                default:
                    ++n;
                    break;
                case '\t':
                    do
                        putchar(' ');
                    while(++n%8);
                    continue;
            }
            putchar(c);
        }
   }
