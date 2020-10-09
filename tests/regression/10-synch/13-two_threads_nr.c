// SKIP PARAM: --sets ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t_fun(void *arg) {
  myglobal=42; // NORACE
  return NULL;
}

int main(void) {
  pthread_t t1, t2;
  myglobal = 1; // NORACE
  pthread_create(&t1, NULL, t_fun, NULL);
  pthread_join (t1, NULL);
  myglobal = 2; // NORACE
  pthread_create(&t2, NULL, t_fun, NULL);
  pthread_join (t2, NULL);
  myglobal = 3; // NORACE
  return 0;
}
