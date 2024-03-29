// PARAM: --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
#include<pthread.h>
#include<stdio.h>

struct cache_entry {
  int refs;
  pthread_mutex_t refs_mutex;
} cache[10];

void cache_entry_addref(struct cache_entry *entry) {
  pthread_mutex_lock(&entry->refs_mutex);
  entry->refs++; // RACE!
  pthread_mutex_unlock(&entry->refs_mutex);
}

void *t_fun(void *arg) {
  int i;
  for(i=0; i<10; i++) cache_entry_addref(&cache[i]);
  return NULL;
}

int main () {
  for (int i = 0; i < 10; i++)
    pthread_mutex_init(&cache[i].refs_mutex, NULL);

  int i;
  pthread_t t1;
  pthread_create(&t1, NULL, t_fun, NULL);
  for(i=0; i<10; i++) cache_entry_addref(&cache[i]);

  pthread_mutex_lock(&cache[4].refs_mutex);
  cache[5].refs++; // RACE!
  pthread_mutex_unlock(&cache[4].refs_mutex);
  return 0;
}
