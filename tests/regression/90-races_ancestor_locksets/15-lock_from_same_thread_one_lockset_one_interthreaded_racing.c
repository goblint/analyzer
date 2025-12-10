// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] transitiveDescendants --set ana.activated[+] creationLockset
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1;

// both accesses are protected by same mutex **locked in same thread**

void *t1(void *arg) {
  global++; // RACE!
  return NULL;
}

int main(void) {
  pthread_mutex_lock(&mutex);
  pthread_create(&id1, NULL, t1, NULL);
  global++; // RACE!
  pthread_join(id1, NULL);
  pthread_mutex_unlock(&mutex);
  return 0;
}
