// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.apron.privatization mutex-meet-tid
// Based on 80-tid-toy10
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

int imInnocent() {
    // If all returns side-effect the set of must-joined threads, this leads to imprecision
    // https://github.com/goblint/analyzer/issues/793
    return 8;
}

void *t_evil(void *arg) {
  pthread_mutex_lock(&A);
  g = 8;
  h = 20;
  pthread_mutex_unlock(&A);
}

void *t_benign(void *arg) {
  pthread_t id2;
  pthread_create(&id2, NULL, t_evil, NULL);

  imInnocent();
  pthread_join(id2, NULL);

  pthread_mutex_lock(&A);
  g = 10;
  h = 10;
  pthread_mutex_unlock(&A);

  return NULL;
}

int main(void) {
  int t;

  // Force multi-threaded handling
  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);

  pthread_mutex_lock(&A);
  g = 10;
  h = 10;
  pthread_mutex_unlock(&A);

  pthread_join(id2, NULL);

  pthread_mutex_lock(&A);
  assert(g == h);
  pthread_mutex_unlock(&A);

  return 0;
}
