#include <pthread.h>

int data;
pthread_mutex_t m[10];

void *t_fun(void *arg) {
  pthread_mutex_lock(&m[4]);
  data++; // RACE!
  pthread_mutex_unlock(&m[4]);
  return NULL;
}

int main() {
  int i;
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m[4]);
  pthread_mutex_unlock(&m[i]);
  data++; // RACE!
  pthread_mutex_unlock(&m[4]);
  return 0;
}

