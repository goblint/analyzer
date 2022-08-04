// Copied & modified from 13/24.
#include <pthread.h>
#include <assert.h>

int g1,g2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t __global_lock = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  pthread_mutex_lock(&__global_lock);
  g1++;
  pthread_mutex_unlock(&__global_lock);
  pthread_mutex_lock(&__global_lock);
  g1--;
  pthread_mutex_unlock(&__global_lock);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

void *t2_fun(void *arg) {
  pthread_mutex_lock(&mutex2);
  pthread_mutex_lock(&__global_lock);
  g2++;
  pthread_mutex_unlock(&__global_lock); // Write Mine influence: [[g2, __global_lock], t2_fun, {mutex2}] -> 1
  pthread_mutex_lock(&__global_lock);
  g2--;
  pthread_mutex_unlock(&__global_lock); // Write Mine influence: [[g2, __global_lock], t2_fun, {mutex2}] -> 0
  pthread_mutex_unlock(&mutex2);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_t id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t2_fun, NULL);

  pthread_mutex_lock(&mutex1);
  pthread_mutex_lock(&__global_lock); // Read & join to g2 Mine influence: [[g2, __global_lock], t2_fun, {mutex2}] -> (0 join 1 = Unknown)
  __goblint_check(g1 == 0);
  pthread_mutex_unlock(&__global_lock);

  pthread_mutex_lock(&mutex2);
  pthread_mutex_lock(&__global_lock);
  __goblint_check(g1 == 0);
  pthread_mutex_unlock(&__global_lock);
  pthread_mutex_lock(&__global_lock);
  __goblint_check(g2 == 0);
  pthread_mutex_unlock(&__global_lock);
  pthread_mutex_unlock(&mutex2);

  pthread_mutex_lock(&__global_lock);
  __goblint_check(g1 == 0);
  pthread_mutex_unlock(&__global_lock);
  pthread_mutex_unlock(&mutex1);

  pthread_join(id, NULL);
  pthread_join(id2, NULL);
  return 0;
}