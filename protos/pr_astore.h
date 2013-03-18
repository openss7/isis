/*  $RCSfile: pr_astore.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:55 $  */
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
 *
 *
 *
 */
/* Header for associative store routines and users */

#ifndef PR_ASTORE
#define PR_ASTORE

struct st_idlist
{
        int        idl_id;
        bitvec  idl_ignore;
        bitvec  idl_scope;
};

#define             FTYPE_IDL       FTYPE_LONG

/* Bits in qu_flags used by associative store */
#define         AS_LOCAL        0x1
#define         AS_DELETE       0x2
#define         AS_ADELETE      0x4
#define         AS_ABFSENT      0x10
#define         AS_IDLISTED     0x20

#define         AS_THRESHOLD    15             /* Trigger collection if local */
#define         AS_MAXDELETE    50             /* deletes reach AS_THRESHOLD  */
                                               /* or all deletes AS_MAXDELETE */
#define         AS_MCOLLECT     200            /* Limit on collection size */

#define         ABFREED         0x80000000     /* Indicates that this id was ab_freed */

#define st_add_int(id,fname,value)    st_add(id,fname,(char*)value,nullroutine)
#define st_find_int(id,fname)         ((int)st_find(id,fname))
#define st_add_bitvec(id,fname,bvec)  (st_add(id,fname,(char*)0, nullroutine)->qu_bitvec = bvec)

qnode       *st_add(), *st_delete(), *st_find_node();
char        *st_find();
int         st_deleteable();
/*and others...*/

#endif
