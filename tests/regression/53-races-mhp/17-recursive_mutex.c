// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset
#define _GNU_SOURCE
#include <pthread.h>

int global = 0;
#ifdef __APPLE__
pthread_mutex_t mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER;
#else
pthread_mutex_t mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#endif
pthread_t id1, id2;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  global++; // RACE (the unlock statement in main doesn't fully unlock the recursive mutex)
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void *t2(void *arg) {
  global++; // RACE (the unlock statement in main doesn't fully unlock the recursive mutex)
  return NULL;
}

int main(void) {
  pthread_create(&id1, NULL, t1, NULL);
  pthread_mutex_lock(&mutex);
  pthread_mutex_lock(&mutex);
  pthread_create(&id2, NULL, t2, NULL);
  pthread_mutex_unlock(&mutex);
  pthread_join(id2, NULL);
  return 0;
}
