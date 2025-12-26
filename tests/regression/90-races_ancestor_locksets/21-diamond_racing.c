// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2, id3, id4_1, id4_2;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  global++; // RACE!
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void *t4(void* arg) { // t4 is not protected by mutex, since it is created twice and the creation in t2 does not happen with mutex locked 
  global++; // RACE!
  return NULL;
}

void *t3(void* arg) {
  pthread_mutex_lock(&mutex);
  pthread_create(&id4_2, NULL, t4, NULL);
  pthread_join(id4_2, NULL);
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void *t2(void *arg) {
  pthread_create(&id4_1, NULL, t4, NULL);
  pthread_join(id4_1, NULL);
  return NULL;
}

int main(void) {
  pthread_create(&id1, NULL, t1, NULL);
  pthread_create(&id2, NULL, t2, NULL);
  pthread_create(&id3, NULL, t3, NULL);
  return 0;
}