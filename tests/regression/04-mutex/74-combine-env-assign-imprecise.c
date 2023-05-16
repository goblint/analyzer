#include <pthread.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

int foo() {
  pthread_mutex_lock(&mutex1);
  return myglobal+1; // NORACE
}

void *t_fun(void *arg) {
  myglobal = foo(); // NORACE
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex1);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
