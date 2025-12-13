// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2, id3;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  global++; // NORACE
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void* t3(void* arg) {
  global++; // NORACE
  return NULL;
}

void *t2(void *arg) { // t2 is protected by mutex locked in main thread
  pthread_create(&id3, NULL, t3, NULL);
  pthread_join(id3, NULL);
  return NULL;
}

int main(void) {
  pthread_create(&id1, NULL, t1, NULL);
  pthread_mutex_lock(&mutex);
  pthread_create(&id2, NULL, t2, NULL);
  pthread_join(id2, NULL);
  pthread_mutex_unlock(&mutex);
  return 0;
}
