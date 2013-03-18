/*  $RCSfile: cl_plist.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:18 $  */
/*
 *	Originally coded by Ken Birman
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
*  Process lists 
*
*********************************************************************/

#define  pl_getview(list_p)     pg_getview((list_p))
#define  pl_rank(paddr, list_p) pg_rank((paddr), (list_p))

#if FUN_TYPES
address *pl_create(address * gaddr, address * paddrs);
address *pl_makegroup(address * list_p, char *gname);
void pl_add(address * list_p, address * addr_p);
void pl_remove(address * list_p, address * addr_p);
void pl_delete(address * list_p);
#else
address *pl_create();
address *pl_makegroup();
void pl_add();
void pl_remove();
void pl_delete();
#endif
