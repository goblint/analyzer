#include <pthread.h>
#include <assert.h>

/*
int g=0;
t1: lock(A); lock(B); g = 5; unlock(B); lock(B); g = 0; unlock(A);
t2: lock(B); lock(A); x = g; unlock(A); unlock(B); // Will read {5,0} with oplus, {0} with meet
t3: lock(A); lock(B); x = g; unlock(B); unlock(A); // Will always read only {0}
*/

int g = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  // t1
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = 5;
  pthread_mutex_unlock(&B);
  pthread_mutex_lock(&B);
  g = 0;
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  __goblint_check(g == 0);
  pthread_create(&id, NULL, t_fun, NULL);
  // t2
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&A);
  __goblint_check(g == 0);
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  pthread_join(id, NULL);
  return 0;
}
