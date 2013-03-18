/*  $RCSfile: cl_hashtab.h,v $ $Revision: 2.29 $ $Date: 90/07/30 10:47:51 $  */
typedef struct hash_tab hash_tab;

#if FUN_TYPES

hash_tab *make_hash_tab(int hash_max,
			int (*hash_func) (void *key, int hash_max),
			bool (*equality_func) (void *key1, void *key2),
			void (*delete_func) (void *key, void *value));
void hash_tab_entries(hash_tab * tab, void (*func) (void *key, void *value, void *arg), void *arg);
void hash_tab_destroy(hash_tab * tab);
bool hash_tab_add(hash_tab * tab, void *key, void *value);
bool hash_tab_replace(hash_tab * tab, void *key, void *value);
bool hash_tab_delete(hash_tab * tab, void *key);
void *hash_tab_lookup(hash_tab * tab, void *key);
void *hash_tab_search(hash_tab * tab, bool (*test) (void *key, void *value, void *arg), void *arg);
void hash_tab_stats(hash_tab * tab, FILE *stream);

#else

hash_tab *make_hash_tab();
void hash_tab_entries();
void hash_tab_destroy();
bool hash_tab_add();
bool hash_tab_replace();
bool hash_tab_delete();
void *hash_tab_lookup();
void *hash_tab_search();
void hash_tab_stats();

#endif
