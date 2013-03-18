/*  $RCSfile: msg_alloc.c,v $ $Revision: 2.23 $ $Date: 90/09/12 13:27:25 $  */
/*
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
 *	Originally coded by Ken Birman
 *	Optimizations by Pat Stephenson
 *
 *
 ******************************************************
 *        General purpose memory allocator/deallocator
 ******************************************************
 */

# include "isis.h"

static adesc *alist[20], **alp = alist;

static chunks_initialized = 0;

typedef struct chunker {
	int chunksize;			/* size of each chunk, bytes */
	int blocksize;			/* # of chunks in this block */
	int used;			/* no actually used at the moment */
	char *freelist;			/* list of free chunks in this block; */
	struct chunker *next;		/* pointer to next block */
	struct chunker *prev;		/* pointer to previous block */
} chunker;

#if FUN_TYPES
void chunk_init(chunker *, int, int);
chunker *chunk_more(chunker *);
#else
void chunk_init();
chunker *chunk_more();
#endif

static chunker chunk16;
static chunker chunk32;
static chunker chunk64;
static chunker chunk128;

#define SMALL 127

static chunker *smallchunks[SMALL + 1] = {
	0, 0, 0, 0, &chunk16, &chunk16, &chunk16, &chunk16,
	&chunk16, &chunk16, &chunk16, &chunk16, &chunk16, &chunk16, &chunk16, &chunk16,
	&chunk32, &chunk32, &chunk32, &chunk32, &chunk32, &chunk32, &chunk32, &chunk32,
	&chunk32, &chunk32, &chunk32, &chunk32, &chunk32, &chunk32, &chunk32, &chunk32,
	&chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64,
	&chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64,
	&chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64,
	&chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64, &chunk64,
	&chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128,
	&chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128,
	&chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128,
	&chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128,
	&chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128,
	&chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128,
	&chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128,
	&chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128, &chunk128
};

void
chunks_init()
{
	chunk_init(&chunk16, 16, 512);
	chunk_init(&chunk32, 32, 256);
	chunk_init(&chunk64, 64, 256);
	chunk_init(&chunk128, 128, 256);
}

void
chunk_init(c, s, h)
	struct chunker *c;
	int s;
	int h;
{
	c->chunksize = s + sizeof(chunker *);
	c->blocksize = h;
	c->used = 0;
	c->freelist = 0;
	c->next = 0;
	c->prev = 0;
}

chunker *
chunk_more(c)
	struct chunker *c;
{
	char *p;
	struct chunker *newc =
	    (struct chunker *) malloc(sizeof(chunker) + c->blocksize * c->chunksize);
#ifdef LOQUACIOUS
	printf("Alloc block %X size %D with %D entries\n", newc, c->chunksize, c->blocksize);
#endif
	if (newc == 0)
		return 0;
	newc->chunksize = c->chunksize;
	newc->blocksize = c->blocksize;
	newc->prev = c->prev;
	newc->next = c;
	if (newc->prev)
		newc->prev->next = newc;
	c->prev = newc;

	/* create the freelist for this chunk */
	for (p = (char *) (newc + 1);
	     p < (((char *) (newc + 1)) + (c->blocksize - 1) * (c->chunksize)); p += c->chunksize) {
		/* point to the next element */
		*(char **) p = p + c->chunksize;
	}
	*(char **) p = 0;
	newc->freelist = (char *) (newc + 1);
	newc->used = 0;

	/* return the new chunk */
	return newc;
}

void
malloc_dump()
{
	register adesc **app;

	print("Memory management statistics:\n");
	for (app = alist; app != alp; app++) {
		register adesc *ap = *app;

		print("  [%.5x] %d/%d nallocs %d nfrees %d\n", ap, ap->a_isize, ap->a_izero,
		      ap->a_nmalloc, ap->a_nmfree);
	}
}

void *
mallocate(ap)
	register adesc *ap;
{
	register chunker *cp;
	register char **pt;

	if ((ap->a_flags & A_INIT) == 0) {
		if ((chunks_initialized & A_INIT) == 0) {
			chunks_init();
			chunks_initialized |= A_INIT;
		}
		/* round up to 4 */
		ap->a_isize = ((ap->a_isize + sizeof(char *)) & ~(sizeof(char *) - 1));
		if (ap->a_isize <= SMALL)
			ap->a_chunks = (chunk *) smallchunks[ap->a_isize];
		else {
			/* use the size hint passed in */
			cp = (chunker *) malloc(sizeof(chunker));
			chunk_init(cp, ap->a_isize, ap->a_nalloc);
			ap->a_chunks = (chunk *) cp;
		}
		ap->a_flags |= A_INIT;
		if (ap->a_nalloc <= 0)
			panic("ap %x nalloc %d outside bounds of 0..\n", ap, ap->a_nalloc);
		/* put it on the list of descriptors */
		*alp++ = ap;
	} else
		++ap->a_nmalloc;
	if (ap->a_chunks) {
		chunker *c1, **ptr;

		c1 = (chunker *) (ap->a_chunks);
		while (c1 && !c1->freelist)
			c1 = c1->prev;
		if (!c1 && !(c1 = chunk_more((chunker *) ap->a_chunks)))
			panic("Out of memory!");
		ptr = (chunker **) c1->freelist;
		c1->freelist = *(char **) (c1->freelist);
		c1->used++;
		*ptr = c1;
		pt = (char **) (((char *) ptr) + sizeof(char *));
#ifdef LOQUACIOUS
		printf("Alloc chunk %X from block %X size %D with %D entries\n", pt, c1,
		       c1->chunksize, c1->blocksize);
#endif
	} else {
		pt = (char **) malloc(ap->a_isize + sizeof(chunk *));
		if (pt == 0)
			panic("Out of memory!");
		*pt++ = 0;
	}
	if (ap->a_izero)
		bzero(pt, ap->a_izero);
	++nalloc;
	memalloc += ap->a_isize;
	return ((void *) pt);
}

/* Deallocate an instance of the designated item */
void
mdeallocate(pt, ap)
	register adesc *ap;
	register VOID *pt;
{
	struct chunker *c;

#define ptc ((char*)pt)

	++nfree;
	++ap->a_nmfree;
	memfree += ap->a_isize;
#if     !(HPUX|AIXRS|SGI|CONVEX)
	ptc -= sizeof(char *);
#else
	pt = (VOID *) (ptc - sizeof(char *));
#endif
	c = *(chunker **) (ptc);
	if (!c) {
		free(ptc);
		return;
	}
#ifdef LOQUACIOUS
	printf("Free chunk %X in block %X size %D with %D entries\n", ptc + sizeof(char *), c,
	       c->chunksize, c->blocksize);
#endif
#ifndef HPUX
	*(char **) ptc = c->freelist;
#else
	begin {
		register char **cpt;

		cpt = (char **) ptc;
		*cpt = c->freelist;
	}
#endif
	c->freelist = ptc;
	if (--c->used <= 0) {
		/* remove this, IF not the only one left (apart from the dummy at tail) */
		/* note that c->next should always exist for real blocks */
		if (c->prev || c->next->next) {
			if (c->prev)
				c->prev->next = c->next;
			c->next->prev = c->prev;
#ifdef LOQUACIOUS
			printf("Free block %X size %D with %D entries\n", c, c->chunksize,
			       c->blocksize);
#endif
			free(c);
		}
	}
}
