#include<pthread.h>
#include<stdio.h>

int myglobal;

void *t_fun(void *arg) {
  pthread_mutex_t *m;
  pthread_mutex_lock(m);
  myglobal++; // RACE!
  pthread_mutex_unlock(m);
  return NULL;
}

int main () {
  pthread_t id;
  pthread_mutex_t *m;
  
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(m);
  myglobal++; // RACE!
  pthread_mutex_unlock(m);
  return 0;
}
