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
  for (int i = 0; i < 10; i++)
    pthread_mutex_init(&m[i], NULL);

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m[3]);
  data++; // RACE!
  pthread_mutex_unlock(&m[3]);
  return 0;
}

