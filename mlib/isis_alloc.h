/*  $RCSfile: isis_alloc.h,v $ $Revision: 2.6 $ $Date: 90/06/24 14:07:04 $  */
/*
 *      ISIS release V2.0, May 1990
 *      Export restrictions apply
 *
 *	The contents of this file are subject to a joint, non-exclusive
 *	copyright by members of the ISIS Project.  Permission is granted for
 *	use of this material in unmodified form in commercial or research
 *      settings.  Creation of derivative forms of this software may be
 *	subject to restriction; obtain written permission from the ISIS Project
 *	in the event of questions or for special situations.
 *	-- Copyright (c) 1990, The ISIS PROJECT
 */

#ifndef PR_ALLOC
#define PR_ALLOC
/*  general purpose memory allocation/deallocation mechanism */

#if ! ( __cplusplus || c_plusplus )
typedef struct adesc adesc;
#endif

#if ! ( __cplusplus || c_plusplus )
typedef struct chunk chunk;
#endif

struct chunk {
	chunk *c_next;			/* Next chunk */
	char *c_first;			/* First byte of this chunk */
	char *c_last;			/* Last byte of this chunk */
	long c_inuse;			/* Bitmap of items in use */
};

struct adesc {
	long a_isize;			/* Item size */
	long a_izero;			/* Amount to zero */
	short a_nalloc;			/* Number to allocate at a time (32 max) */
	long a_flags;			/* Flags */
	chunk *a_chunks;		/* To list of chunks */
	chunk *a_notfull;		/* To a chunk that isn't full */
	int a_nmalloc;			/* Number of mallocs done */
	int a_nmfree;			/* Number of mallocs done */
};

#define A_INIT  0x01		/* Set after first time */

extern int nalloc, nfree, memused, memfree, memalloc;

/* Memory allocator routines. */

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	void malloc_dump();
	void *mallocate(adesc * ap);
	void mdeallocate(void *pt, adesc * ap);
#ifdef __cplusplus
}
#endif
#else

void *mallocate();

#endif

#endif				/* PR_ALLOC */
