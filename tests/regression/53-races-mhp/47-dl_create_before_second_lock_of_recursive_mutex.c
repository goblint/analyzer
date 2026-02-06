// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset
#define _GNU_SOURCE
#include <pthread.h>

int global = 0;
#ifdef __APPLE__
pthread_mutex_t mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER;
#else
pthread_mutex_t mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#endif
pthread_t id1;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  pthread_mutex_unlock(&mutex);
  global++;
  return NULL;
}

int main(void) {
  pthread_mutex_lock(&mutex);
  pthread_create(&id1, NULL, t1, NULL);
  global++; // NORACE
  pthread_mutex_lock(&mutex);
  global++; // NORACE
  pthread_mutex_unlock(&mutex);
  global++; // RACE (the unlock statement above doesn't fully unlock the recursive mutex)
  pthread_mutex_unlock(&mutex);
  global++; // RACE!
  return 0;
}
