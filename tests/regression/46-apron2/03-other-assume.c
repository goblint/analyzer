// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.apron.privatization mutex-meet-tid
#include <pthread.h>
#include <goblint.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  return NULL;
}

void *t_benign(void *arg) {
  pthread_t id2;
  pthread_create(&id2, NULL, t_fun, NULL);
  __goblint_assume_join(id2, NULL);
  // t_fun should be in here

  g = 7;

  return NULL;
}

int main(void) {
  int t;

  pthread_t id2[10];
  for(int i =0; i < 10;i++) {
    pthread_create(&id2[i], NULL, t_benign, NULL);
  }

  __goblint_assume_join(id2[2]);
  // t_benign and t_fun should be in here

  __goblint_check(g==h); //FAIL

  return 0;
}
