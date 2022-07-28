// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.apron.privatization mutex-meet-tid
// Modification of 19 that would fail if pthread_exit was only handled for the top-level thread
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void cheeky() { pthread_exit(NULL); }

void *t_evil(void *arg) {
  pthread_mutex_lock(&A);
  g = 8;
  h = 20;
  pthread_mutex_unlock(&A);
}

void *t_benign(void *arg) {
  pthread_t id2;
  pthread_create(&id2, NULL, t_evil, NULL);

  int top;

  if(top) {
    // If the analysis does unsoundly not account for pthread_exit called from another function, these writes are lost
    cheeky();
  }

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
  assert(g == 10); //UNKNOWN!
  pthread_mutex_unlock(&A);

  return 0;
}
