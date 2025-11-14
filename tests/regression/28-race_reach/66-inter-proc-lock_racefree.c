//PARAM: --set lib.activated[+] sv-comp --set ana.activated[+] creationLockset
#include <pthread.h>
#include "racemacros.h"

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t tmain_child, t1, t1_child;

void *t1_child(void* arg) { // t1child is protected by mutex locked in t1
  access(global);
  return NULL;
}

void *tmain_child(void *arg) { // tmainchild is protected by mutex locked in main thread
  assert_racefree(global); // NORACE
  return NULL;
}

void *t1_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  pthread_create(&t1_child, NULL, t1_child, NULL);
  pthread_join(t1_child, NULL);
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_create(&t1, NULL, t1, NULL);
  pthread_mutex_lock(&mutex);
  pthread_create(&tmain_child, NULL, tmain_child, NULL);
  pthread_join(tmain_child, NULL);
  pthread_mutex_unlock(&mutex);
  return 0;
}
