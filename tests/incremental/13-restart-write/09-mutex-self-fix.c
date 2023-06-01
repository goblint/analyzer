#include <pthread.h>

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
int g;

void *t_fun(void *arg) {
  g++; // RACE!
  return NULL;
}

int main() {
  pthread_t id;
  while (1) {
    pthread_create(&id, NULL, t_fun, NULL);
  }
  return 0;
}
