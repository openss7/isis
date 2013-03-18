/*  $RCSfile: cl_hashtab.c,v $ $Revision: 2.102 $ $Date: 90/09/11 11:17:33 $  */
/******************************************************************************
* Generic hash tables.
*
******************************************************************************/

#include "isis.h"
#include <stdio.h>

/* Linked list 
   A null list is represented by an ht_link containing three NULLs. 
   A non-null list is represented by one or more ht_links linked by
   their "next" fields, with the last "next" field NULL, and none of
   the "value" fields null. 
*/

typedef struct ht_link ht_link;
struct ht_link {
	void *key;			/* NULL if no entry at this slot. */
	/* Only the first ht_link in a list may have a null pointer in this field. */
	void *value;			/* NULL if no entry at this slot. */
	ht_link *next;			/* NULL if no successor. */
};

#define NULL_LINK (ht_link *) 0
static adesc link_ad = { sizeof(ht_link), sizeof(ht_link), 10 };

struct hash_tab {
	int hash_max;
	int (*hash_func) ( /* void *key, int hash_max */ );
	bool (*equality_func) ( /* void *key1, void *key2 */ );
	void (*delete_func) ( /* void *key, void *value; */ );
	int n_entries;
	ht_link slot[1];
};

hash_tab *
make_hash_tab(hash_max, hash_func, equality_func, delete_func)
	int hash_max;
	int (*hash_func) ( /* void *, int */ );
	bool (*equality_func) ( /* void *, void * */ );
	void (*delete_func) ( /* void *, void * */ );

  /* Make a hash table with hash_max entries. hash_max should be prime. */
{
	hash_tab *tab = (hash_tab *) malloc(sizeof(hash_tab) + sizeof(ht_link) * (hash_max - 1));

	tab->hash_max = hash_max;
	tab->hash_func = hash_func;
	tab->equality_func = equality_func;
	tab->delete_func = delete_func;
	tab->n_entries = 0;
	bzero(tab->slot, sizeof(ht_link) * (hash_max));
	return (tab);
}

void
hash_tab_entries(tab, func, arg)
	hash_tab *tab;
	void (*func) ( /* void *key; void *value; void *arg */ );
	void *arg;

  /* Find first entries which satisfies test. */
{
	int i;
	ht_link *slot = tab->slot;

	for (i = 0; i < tab->hash_max; i++) {
		void *key = slot->key;

		if (key) {	/* Test because first entry may be NULL */
			ht_link *link = slot;

			(*func) (key, slot->value, arg);

			while ((link = link->next) != NULL_LINK) {
				/* Later entries are always non-NULL */
				(*func) (link->key, link->value, arg);
			}
		}
		slot++;
	}
}

void
hash_tab_destroy(tab)
	hash_tab *tab;
{
	int i;
	ht_link *slot = tab->slot;
	void (*func) () = tab->delete_func;

	for (i = 0; i < tab->hash_max; i++) {
		void *key = slot->key;

		if (key) {
			ht_link *link = slot->next;

			(*func) (key, slot->value);

			while (link != NULL_LINK) {
				ht_link *prev = link;

				link = link->next;
				(*func) (prev->key, prev->value);
				mdeallocate(prev, &link_ad);
			}
		}
		slot++;
	}
	free(tab);
}

static bool
hash_tab_update(tab, key, value, replace)
	register hash_tab *tab;
	void *key;
	void *value;
	bool replace;
{
	register ht_link *link = &(tab->slot[tab->hash_func(key, tab->hash_max)]);
	void *key1 = link->key;

	if (key1) {
		register ht_link *nextlink;

		forever {
			if (tab->equality_func(key, key1)) {
				/* Value already exists for this key. */
				if (replace) {
					tab->delete_func(key1, link->value);
					link->key = key;
					link->value = value;
				}
				return (TRUE);
			}

			nextlink = link->next;
			if (nextlink) {
				link = nextlink;
				key1 = link->key;
			} else {
				break;
			}
		}

		/* Hash synonyms exist: append new link to hash chain. */
		nextlink = (ht_link *) mallocate(&link_ad);
		nextlink->key = key;
		nextlink->value = value;
		nextlink->next = (ht_link *) 0;
		link->next = nextlink;
	} else {
		/* No existing entry or hash synonyms: overwite tab->slot[i]. Hopefully this is the 
		   common case. */
		link->key = key;
		link->value = value;
	}
	return (FALSE);

}

bool
hash_tab_add(tab, key, value)
	hash_tab *tab;
	void *key;
	void *value;
{
	return (hash_tab_update(tab, key, value, FALSE));
}

bool
hash_tab_replace(tab, key, value)
	hash_tab *tab;
	void *key;
	void *value;
{
	return (hash_tab_update(tab, key, value, TRUE));
}

bool
hash_tab_delete(tab, key)
	hash_tab *tab;
	void *key;
{
	ht_link *link = &(tab->slot[tab->hash_func(key, tab->hash_max)]);
	ht_link *prevlink = (ht_link *) NULL;
	void *key1 = link->key;

	if (key1) {
		forever {
			if (tab->equality_func(key, key1)) {
				break;
			}
			prevlink = link;
			link = link->next;
			if (link) {
				key1 = link->key;
			} else {
				return (FALSE);
			}
		}
	} else {
		return (FALSE);
	}

	tab->delete_func(link->key, link->value);
	if (prevlink) {
		prevlink->next = link->next;
		mdeallocate((char *) link, &link_ad);
	} else {
		/* No prevlink: entry being deleted is an element of tab-> slot, so don't
		   deallocate it! */
		ht_link *lnext = link->next;

		if (lnext) {	/* Is there another element ? */
			(*link) = (*lnext);
			mdeallocate((char *) lnext, &link_ad);
		} else {	/* It's the only element. */
			link->next = (ht_link *) NULL;
			link->key = (void *) NULL;
			link->value = (void *) NULL;
		}
	}
	return (TRUE);
}

void *
hash_tab_lookup(tab, key)
	hash_tab *tab;
	void *key;
{
	ht_link *link = &(tab->slot[tab->hash_func(key, tab->hash_max)]);
	void *key1 = link->key;

	if (key1) {
		forever {
			if (tab->equality_func(key, key1)) {
				return (link->value);
			}
			link = link->next;
			if (link) {
				key1 = link->key;
			} else {
				break;
			}
		}
	}
	return ((void *) NULL);
}

void *
hash_tab_search(tab, test, arg)
	hash_tab *tab;
	bool (*test) ( /* void *, void *, void * */ );
	void *arg;
{
	int i;
	ht_link *slot = tab->slot;

	for (i = 0; i < tab->hash_max; i++) {
		void *key = slot->key;

		if (key) {	/* Test because first entry may be NULL */
			ht_link *link = slot;

			if ((*test) (key, slot->value, arg)) {
				return (key);
			}

			while ((link = link->next) != NULL_LINK) {
				/* Later entries are always non-NULL */
				if ((*test) (link->key, link->value, arg)) {
					return (key);
				}
			}
		}
		slot++;
	}
	return (NULL);
}

void
hash_tab_stats(tab, stream)
	hash_tab *tab;
	FILE *stream;
{
	static int hist[11];

	/* hist[i] contains the number of slots with i entries. hist[10] contains the number of
	   slot with 10 or more entries. static variable is OK since we don't block. */
	int i;
	int tot = 0;
	ht_link *slot = tab->slot;

	for (i = 0; i < 11; i++) {
		hist[i] = 0;
	}

	for (i = 0; i < tab->hash_max; i++) {
		void *key = slot->key;
		int freq = 0;

		if (key) {	/* Test because first entry may be NULL */
			ht_link *link = slot;

			freq++;

			while ((link = link->next) != NULL_LINK) {
				/* Later entries are always non-NULL */
				freq++;
			}
		}
		tot += freq;
		if (freq >= 10) {
			freq = 10;
		}
		hist[freq]++;
		slot++;
	}

	if (stream == (FILE *) 0) {
		stream = stdout;
	}
	fprintf(stream, "%d slots, %d entries\n", tab->hash_max, tot);
	if (tot > 0) {
		int n = 10;

		fprintf(stream, "hash chain length frequencies: ");
		while (hist[n] == 0) {
			n--;
		}
		for (i = 0; i <= n; i++) {
			if (i == 10) {
				fprintf(stream, "(10+: %d) ", hist[i]);
			} else {
				fprintf(stream, "(%d: %d) ", i, hist[i]);
			}
		}
	}
	fprintf(stream, "\n");
	fflush(stream);
}
