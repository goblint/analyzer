// SKIP PARAM: --sets ana.activated[+] octApron
#include <pthread.h>
#include <assert.h>

int g = 17;
int h = 14;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t, r; // rand
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = r;
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
  if (t > -1000)
    t--;
  h = t;
  pthread_mutex_unlock(&B);
  return NULL;
}

void *t3_fun(void *arg) {
  int t;
  pthread_mutex_lock(&A);
  t = g;
  if (t < 1000)
    t++;
  g = t;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

  pthread_t id, id2, id3;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t2_fun, NULL);
  pthread_create(&id3, NULL, t3_fun, NULL);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  assert(g >= h); // UNKNOWN (for protection at least)
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return 0;
}
