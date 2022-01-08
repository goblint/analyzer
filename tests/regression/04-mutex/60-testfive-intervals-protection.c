// PARAM: --enable ana.int.interval --set ana.base.privatization protection --set solvers.td3.side_widen sides-pp
// also needs sides-pp now that protected and unprotected use different global constraint variables
#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void lock() {
  pthread_mutex_lock(&mutex);
}

void unlock() {
  pthread_mutex_unlock(&mutex);
}


void *t_fun(void *arg) {
  lock();
  myglobal++; // NORACE
  unlock();
  return NULL;
}


int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  lock();
  myglobal++; // NORACE
  unlock();
  pthread_join (id, NULL);
  return 0;
}
