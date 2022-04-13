// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.apron.privatization mutex-meet-tid
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

pthread_t other_t;

void *t_fun(void *arg) {
  int x;
  int y;

  pthread_mutex_lock(&A);
  g = x;
  h = y;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  g = x;
  h = x;
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t_benign(void *arg) {
  // Without this, it would even succeed without the must joined analysis.
  // With it, that is required!
  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  pthread_create(&other_t, NULL, t_fun, NULL);
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  g = 10;
  h = 10;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);

  // Force multi-threaded handling
  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);

  pthread_mutex_lock(&A);
  assert(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);

  pthread_join(id2, NULL);

  pthread_mutex_lock(&A);
  assert(g == h); // UNKNOWN!
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  pthread_join(other_t, NULL);
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  assert(g == h);
  pthread_mutex_unlock(&A);

  return 0;
}
