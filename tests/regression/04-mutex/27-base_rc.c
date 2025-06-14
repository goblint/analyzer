#include <pthread.h>
#include <stdio.h>

int global;
pthread_mutex_t gm = PTHREAD_MUTEX_INITIALIZER;

void bad() {
  global++; // RACE!
}
void good() {
  pthread_mutex_lock(&gm);
  global++; // NORACE (same unique thread with bad, same lock with main)
  pthread_mutex_unlock(&gm);
}

void (*f)(void) = good;
pthread_mutex_t fm = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  void (*g)(void);

  pthread_mutex_lock(&fm);
  g = f; // NORACE
  pthread_mutex_unlock(&fm);

  g();
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&fm);
  f = bad; // NORACE
  pthread_mutex_unlock(&fm);

  pthread_mutex_lock(&gm);
  printf("global: %d\n", global); // RACE!
  pthread_mutex_unlock(&gm);

  return 0;
}
