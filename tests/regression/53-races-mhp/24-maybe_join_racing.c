// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset
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

void *t2(void *arg) { // t2 is not protected by mutex locked in main thread, joining may happen before unlocking
  global++; // RACE!
  return NULL;
}

int main(void) {
  pthread_create(&id1, NULL, t1, NULL);
  pthread_mutex_lock(&mutex);
  pthread_create(&id2, NULL, t2, NULL);
  int maybe;
  if (maybe) { 
    pthread_join(id2, NULL);
  }
  pthread_mutex_unlock(&mutex);
  return 0;
}