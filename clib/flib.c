/*  $RCSfile: flib.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:49 $  */
/*
 * ISIS interface from Fortran 
 * (some of these routines are called from lisp too)
 * flib1.c contains routines that have no underscores in the first place.
 *
 *	Originally coded by Ken Birman
 *
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
 
#include "isis.h"
#include "spooler.h"

int
abcast_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_ABCAST, "", &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
bcast_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_ABCAST, "", &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

void
abortreply_(mp)
  message **mp;
  {
	abortreply(*mp);
  }

int
cbcast_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_CBCAST, "", &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

#ifndef MACH
static char *imsg = "**** subroutine/function parameter was not declared external";
#define is_ifunc(routine) { if(((char**)routine) && *((char**)routine) == (char*)0) panic(imsg); }
#else
#define is_ifunc(routine)
#endif

void
forward_(fmsg, to, ent, cmsg)
  message **fmsg, **cmsg;
  address **to;
  int *ent;
  {
        forward(*fmsg, *to, *ent, *cmsg);
  }

int
fbcast_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_FBCAST, "", &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
gbcast_(va_alist)
  va_dcl
  {
        int rval;
        va_list ap;
        va_start(ap);
	BEGINFROMFORTRAN
            rval = do_bcast(CL_GBCAST, "", &ap);
	ENDFROMFORTRAN
        va_end(ap);
        return rval;
  }

int
nullreply_(mp)
  message **mp;
  {
        return nullreply(*mp);
  }

void
paddr_(addr)
  address **addr;
  {
        paddr(*addr);
  }

void
paddrs_(addr)
  address **addr;
  {
        paddrs(*addr);
  }

void
peid_(eid)
  event_id *eid;
  {
        peid(*eid);
  }

void
pmsg_(mp)
  message **mp;
  {
        pmsg(*mp);
  }

void
reply_(va_alist)
  va_dcl
  {
        va_list ap;
        va_start(ap);
        BEGINFROMFORTRAN
            do_reply(&ap);
        ENDFROMFORTRAN
        va_end(ap);
  }

int
bit_(bv, i)
  bitvec **bv;
  int *i;
  {
        return bit(*bv, *i);
  }

int
bis_(bv, i)
  bitvec **bv;
  int *i;
  {
        return bis(*bv, *i);
  }

void
bclr_(bv)
  bitvec **bv;
  {
        bclr(*bv);
  }

int
btst_(bv)
  bitvec **bv;
  {
        return btst(*bv);
  }

int
address_(site, inc, pro, ent)
  int *site, *inc, *pro, *ent;
  {
	register qnode *qp;
	extern qnode *ADDRS;
	address addr;
        addr = ADDRESS(*site, *inc, *pro, *ent);
	if((qp = pg_find(ADDRS, &addr)) == 0)
	    qp = pg_add(ADDRS, &addr, NULLARG, NULLROUTINE);
	return (int)&qp->qu_pname;
  }

int
nulladdress_()
  {
        return (int)&NULLADDRESS;
  }

void
pmembers_(gv)
  register groupview **gv;
  {
	paddrs((*gv)->gv_members);
  }

void
pclients_(gv)
  register groupview **gv;
  {
	paddrs((*gv)->gv_clients);
  }


void
spool_(va_alist)
  va_dcl
  {
	va_list ap;
        va_start(ap);
        BEGINFROMFORTRAN;
            do_spool(&ap, 0);
        ENDFROMFORTRAN;
	va_end(ap);
  }

void
spoolm_(va_alist) 
  va_dcl
  {
	va_list ap;
        va_start(ap);
        BEGINFROMFORTRAN;
            do_spool(&ap, 1);
        ENDFROMFORTRAN;
        va_end(ap); 
  } 

void newspost_(slist, subj, mp, back)
  site_id *slist;
  char *subj;
  message **mp;
  int *back;
  {
        news_post(slist, subj, *mp, *back);
  }

