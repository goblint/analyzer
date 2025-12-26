// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset
#include <pthread.h>

// This program does not make any sense in practice (it produces a deadlock and if it did not, it wouldn't halt)
// We solely want to assert that the accesses to global race

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, idc1;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  global++; // RACE!
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void *tc1(void* arg);

void *tc2(void *arg) {
  pthread_t idc1;
  pthread_mutex_lock(&mutex);
  pthread_create(&idc1, NULL, tc1, NULL);
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void *tc1(void *arg) { // tc1 is protected by tc2, but not by the main thread.
  global++; // RACE!
  pthread_t idc2;
  pthread_create(&idc2, NULL, tc2, NULL);
  return NULL;
}

int main(void) {
  pthread_create(&id1, NULL, t1, NULL);
  pthread_create(&idc1, NULL, tc1, NULL);
  return 0;
}