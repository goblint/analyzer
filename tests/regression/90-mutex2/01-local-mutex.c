#include <pthread.h>

int myglobal;

void *t_fun(void *arg) {
  pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
  pthread_mutex_lock(&mutex1);
  myglobal=myglobal+1; // RACE!
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun, NULL);
  pthread_join (id, NULL);
  pthread_join (id2, NULL);
  return 0;
}
