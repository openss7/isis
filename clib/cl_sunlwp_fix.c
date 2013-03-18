/*  $RCSfile: cl_sunlwp_fix.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:29 $  */
/*
 *	Originally coded by Robert Cooper
 *      cl_sunlwp_wrapper.c 
 *      Wrapper functions needed because on gcc and Sun C have incompatible
 *      conventions for passing 8-byte structs by value.
 *      MUST BE COMPILED USING SUN C!!!
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
int ___lwpdummy___;			/* Just to prevent there being a null object file. */

#if SUNLWP && SUN4
#if __GNUC__
#error Must be compiled using Sun C!!!
#endif

#include <lwp/lwp.h>

int
_mon_enter(mid)
	struct mon_t *mid;
{
	return (mon_enter(*mid));
}

int
_mon_exit(mid)
	struct mon_t *mid;
{
	return (mon_exit(*mid));
}

int
_cv_wait(cv)
	struct cv_t *cv;
{
	return (cv_wait(*cv));
}

int
_cv_notify(cv)
	struct cv_t *cv;
{
	return (cv_notify(*cv));
}

int
_cv_destroy(cv)
	struct cv_t *cv;
{
	return (cv_destroy(*cv));
}
#endif
