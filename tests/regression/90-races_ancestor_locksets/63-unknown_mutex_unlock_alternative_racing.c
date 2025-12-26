// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] creationLocksetAlternative --set ana.activated[+] taintedCreationLocksetAlternative
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  global++; // RACE!
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void *t2(void *arg) { // t2 is not protected by mutex locked in main thread, since an unlock could happen before joining
  global++; // RACE!
  return NULL;
}

int main(void) {
  pthread_create(&id1, NULL, t1, NULL);
  pthread_mutex_lock(&mutex);
  pthread_create(&id2, NULL, t2, NULL);
  pthread_mutex_t *mutex_ref;
  pthread_mutex_unlock(mutex_ref); // unlock of unknown mutex
  pthread_join(id2, NULL);
  return 0;
}