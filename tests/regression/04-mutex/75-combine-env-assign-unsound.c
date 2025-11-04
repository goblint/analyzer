#include <pthread.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

int foo() {
  int x = myglobal + 1; // NORACE
  pthread_mutex_unlock(&mutex1);
  return x;
}

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  myglobal=foo(); // RACE!
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex1);
  myglobal=myglobal+1; // RACE!
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
