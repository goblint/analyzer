// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <pthread.h>
#include <assert.h>

int g = 17; // matches write in t_fun
int h = 14; // matches write in t_fun
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t;
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = 17;
  t = g;
  h = t - 3;
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t2_fun(void *arg) {
  int t;
  pthread_mutex_lock(&B);
  t = h;
  if (t > -1000) // avoid underflow
    t--;
  h = t;
  pthread_mutex_unlock(&B);
  return NULL;
}

int main(void) {
  int t;

  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t2_fun, NULL);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  assert(g >= h);
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);

  t = g;
  pthread_mutex_lock(&A);
  assert(t >= g);
  pthread_mutex_unlock(&A);
  return 0;
}
