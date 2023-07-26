// PARAM: --disable ana.race.volatile
#include <pthread.h>
#include <stdio.h>

volatile int myglobal;

void *t_fun(void *arg) {
  myglobal= 8; //NORACE
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, (void*) &myglobal);
  myglobal = 42; //NORACE
  pthread_join (id, NULL);
  return 0;
}