// PARAM: --set ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t2_fun(void *arg) {
  myglobal=42; // RACE!
  return NULL;
}

void *t1_fun(void *arg) {
  pthread_t t2;
  pthread_create(&t2, NULL, t2_fun, NULL);
  return NULL;
}

int main(void) {
  pthread_t t1, t2;
  myglobal = 1; // NORACE
  pthread_create(&t1, NULL, t1_fun, NULL);
  pthread_join(t1, NULL);
  myglobal = 3; // RACE!
  return 0;
}
