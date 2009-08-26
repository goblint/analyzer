#include <pthread.h>

int data[10];
pthread_mutex_t m[10];

void *t_fun(void *arg) {
  pthread_mutex_lock(&m[4]);
  data[4]++; // RACE!
  pthread_mutex_unlock(&m[4]);
  return NULL;
}

int main() {
  int i;
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m[i]);
  data[4]++; // RACE!
  pthread_mutex_unlock(&m[i]);
  return 0;
}

