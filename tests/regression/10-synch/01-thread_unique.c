// PARAM: --sets ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t_fun(void *arg) {
  myglobal=40; //NOWARN
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join (id, NULL);
  return 0;
}
