// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.apron.privatization mutex-meet-tid
// Copy of 80-tid-toy10 making use of pthread_exit
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_benign(void *arg) {
  int top;

  if(top) {
    pthread_mutex_lock(&A);
    g = 10;
    h = 10;
    pthread_mutex_unlock(&A);

    // If the analysis does unsoundly not account for pthread_exit, these writes are lost
    pthread_exit(NULL);
  }

  return NULL;
}

int main(void) {
  int t;

  // Force multi-threaded handling
  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);

  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);

  pthread_join(id2, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(g == 12); //UNKNOWN!
  pthread_mutex_unlock(&A);

  return 0;
}
