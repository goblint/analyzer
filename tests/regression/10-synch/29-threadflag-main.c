// PARAM: --set ana.activated[-] threadid
// Deactivate threadid to rely on threadflag only.
#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t_fun(void *arg) {
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  myglobal=myglobal+1; // NORACE
  pthread_join (id, NULL);
  return 0;
}
