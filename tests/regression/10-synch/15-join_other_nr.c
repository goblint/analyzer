// SKIP PARAM: --sets ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t2_fun(void *arg) {
  myglobal=42; //NOWARN
  return NULL;
}

void *t1_fun(void *arg) {
  pthread_t t2;
  pthread_create(&t2, NULL, t2_fun, NULL);
  pthread_join(t2, NULL);
  return NULL;
}

int main(void) {
  pthread_t t1, t2;
  myglobal = 1; //NOWARN
  pthread_create(&t1, NULL, t1_fun, NULL);
  pthread_join(t1, NULL);
  myglobal = 3; //NOWARN
  return 0;
}
