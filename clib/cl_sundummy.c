/*  $RCSfile: cl_sundummy.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:28 $  */
/*
 *      These dummy versions of suntools routines are used
 *      so that ISIS will link correctly on a non-SUNTOOLS UNIX
 *      application
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
#include "isis.h"

notify_set_itimer_func(client, infunc, which, value, ovalue)
  ifunc *client, *infunc;
  int which;
  struct itimerval *value, *ovalue;
  {
  }
