// CRAM
#include<pthread.h>
#include<stdio.h>

int myglobal;

void *t_fun(void *arg) {
  pthread_mutex_t *m;
  pthread_mutex_lock(m); // ghost_1 = 1
  myglobal++;
  pthread_mutex_unlock(m); // ghost_1 = 0
  return NULL;
}

int main () {
  pthread_t id;
  pthread_mutex_t *m;
  
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(m); // ghost_1 = 1
  myglobal++;
  pthread_mutex_unlock(m); // ghost_1 = 0
  return 0;
}