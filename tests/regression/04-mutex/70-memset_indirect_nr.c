#include <pthread.h>
#include <stdio.h>
#include <string.h>

int g;

struct s {
  int *p;
} s = {&g};

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int *p;
  pthread_mutex_lock(&mutex);
  p = s.p; // NORACE
  pthread_mutex_unlock(&mutex);
  (*p)++; // NORACE
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex);
  memset(&s, 0, sizeof(s)); // NORACE
  pthread_mutex_unlock(&mutex);
  pthread_join (id, NULL);
  return 0;
}
