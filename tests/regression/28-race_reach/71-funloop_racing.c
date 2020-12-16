// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
#include<pthread.h>
#include<stdio.h>
#include "racemacros.h"

struct cache_entry {
  int refs;
  pthread_mutex_t refs_mutex;
} cache[10];

void cache_entry_addref(struct cache_entry *entry) {
  pthread_mutex_lock(&entry->refs_mutex);
  access_or_assert_racefree(entry->refs); // UNKNOWN
  pthread_mutex_unlock(&entry->refs_mutex);
}

void *t_fun(void *arg) {
  int i;
  for(i=0; i<10; i++)
    cache_entry_addref(&cache[i]);
  return NULL;
}

int main () {
  for (int i = 0; i < 10; i++)
    pthread_mutex_init(&cache[i].refs_mutex, NULL);

  int i;
  create_threads(t);

  for(i=0; i<10; i++)
    cache_entry_addref(&cache[i]);
  access_or_assert_racefree(cache[5].refs); // UNKNOWN

  join_threads(t);
  return 0;
}
