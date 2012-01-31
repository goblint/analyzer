#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

struct test {
  int x;
  int y;
} pq;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mtx);
  pq.y++;
  pq.y--;
  assert(pq.x == 0);
  pthread_mutex_unlock(&mtx);
  return NULL;
}

int main(void) {
  pthread_t id;
  pq.x = 0;
  pq.y = 0;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mtx);
  assert(pq.y == 0);
  pthread_mutex_unlock(&mtx);
  pthread_join (id, NULL);
  return 0;
}
