/*  $RCSfile: grid.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:12 $  */
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
#define AB_MODE     0
#define CB_MODE     1
#define FB_MODE     2
#define MB_MODE     3
#define GB_MODE     4
#define NO_MODE     5

int     value[8][8];
char    color[8][8];
int     mode;
int     n_memb;
char    go;
int     nstopped;
void    update();

#if (HPUX)
#define RANDOM rand
#define SRANDOM srand
int srand(), rand();
#else
#define RANDOM random
#define SRANDOM srandom
int srandom(), random();
#endif

