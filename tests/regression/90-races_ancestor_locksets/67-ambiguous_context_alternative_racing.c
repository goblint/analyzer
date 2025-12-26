// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] creationLocksetAlternative --set ana.activated[+] taintedCreationLocksetAlternative
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2;

// both accesses are protected by same mutex **locked in same thread**

void *t1(void* arg) {
  pthread_mutex_lock(&mutex);
  global++; // RACE!
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void *t2(void *arg) {
  global++; // RACE!
  return NULL;
}

void evil(int x) {
    if (x) {
        pthread_mutex_lock(&mutex);
    }
    pthread_create(&id2, NULL, t2, NULL);
}

int main(void) {
  pthread_create(&id1, NULL, t1, NULL);
  int maybe;
  if (maybe) {
    evil(0);
  } else {
    evil(1);
  }
  return 0;
}
