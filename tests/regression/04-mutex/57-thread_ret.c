//PARAM: --enable ana.int.interval

#include<pthread.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;


int f() {
  return 1;
}

void *t_fun(void *arg) {
  myglobal=f();
  return NULL;
}

int main(void) {
  pthread_t id;
  void *ptr;
  void **pptr = &ptr;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join (id, pptr);
  int v = *((int*) pptr);
  // If we don't have the threadreturn analysis running, all returns from all functions called by the t_fun thread, as well as of t_fun itself are joined together
  // But we still should get a value better than top!
  assert(v!=2);
  return 0;
}
