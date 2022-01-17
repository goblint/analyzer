// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.apron.privatization mutex-meet-tid --disable ana.thread.include-loc
#include <pthread.h>
#include <assert.h>

int g = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_benign(void *arg) {
  pthread_mutex_lock(&A);
  g = 20;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

  // Force multi-threaded handling
  pthread_t id2;

  if(t) {
    pthread_create(&id2, NULL, t_benign, NULL);
  } else {
    pthread_create(&id2, NULL, t_benign, NULL);
  }

  pthread_join(id2, NULL);


  pthread_mutex_lock(&A);
  g = 12;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  assert(g == 12);
  pthread_mutex_unlock(&A);

  return 0;
}
