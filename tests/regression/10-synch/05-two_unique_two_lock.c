// PARAM: --set ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *f1(void *arg) {
  pthread_mutex_lock(&A);
  myglobal=1; // RACE! (with just 4)
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&B);
  myglobal=2; // RACE! (with just 3)
  pthread_mutex_unlock(&B);
  return NULL;
}

void *f2(void *arg) {
  pthread_mutex_lock(&A);
  myglobal=3; // RACE! (with just 2)
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&B);
  myglobal=4; // RACE! (with just 1)
  pthread_mutex_unlock(&B);
  return NULL;
}

int main(void) {
  pthread_t t1, t2;
  pthread_create(&t1, NULL, f1, NULL);
  pthread_create(&t2, NULL, f2, NULL);
  return 0;
}
