#include <pthread.h>

int data;
int *p = &data, *q;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  *p = 8; // NORACE
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  q = p; // NORACE
  pthread_mutex_lock(&mutex);
  *q = 8; // NORACE
  pthread_mutex_unlock(&mutex);
  return 0;
}
