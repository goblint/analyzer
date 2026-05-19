// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2;

// both accesses are protected by same mutex **locked in same thread**

void *tc(void *arg) {
  global++; // RACE!
  return NULL;
}

int main(void) {
  pthread_mutex_lock(&mutex);
  pthread_create(&id1, NULL, tc, NULL);
  pthread_create(&id2, NULL, tc, NULL);
  pthread_join(id1, NULL);
  pthread_join(id2, NULL);
  pthread_mutex_unlock(&mutex);
  return 0;
}
