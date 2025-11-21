#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex); // must happen after unlock in main
  pthread_mutex_unlock(&mutex);
  global++; // NORACE
  return NULL;
}

int main(void) {
  pthread_mutex_lock(&mutex);
  pthread_create(&id1, NULL, t_fun, NULL);
  global++; // NORACE
  pthread_mutex_unlock(&mutex);
  return 0;
}
