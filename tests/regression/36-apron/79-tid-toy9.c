// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --sets ana.apron.privatization mutex-meet-tid
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_benign(void *arg) {
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
  pthread_join(id2, NULL);

  // This one succeeds despite what is described in the paper, and even without the must-join analysis :)
  pthread_mutex_lock(&A);
  assert(g == h);
  pthread_mutex_unlock(&A);

  return 0;
}
