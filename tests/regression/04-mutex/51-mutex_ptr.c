#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t * mp = &mutex1;

void *t_fun(void *arg) {
  pthread_mutex_t * mp1;
  int *p = &myglobal;
  mp1 = mp; // NORACE
  pthread_mutex_lock(mp);
  *p = *p + 1; // NORACE
  pthread_mutex_unlock(mp);
  return NULL;
}

int main(void) {
  pthread_mutex_t * mp1;
  pthread_t id;
  mp = &mutex1; // NORACE
  pthread_create(&id, NULL, t_fun, NULL);
  mp1 = mp; // NORACE
  pthread_mutex_lock(mp);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(mp);
  pthread_join (id, NULL);
  return 0;
}
