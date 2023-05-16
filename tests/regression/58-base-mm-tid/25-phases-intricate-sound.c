// PARAM: --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid --enable ana.int.interval --set ana.activated[+] threadJoins --set ana.activated[+] thread
#include <pthread.h>
#include <goblint.h>

int g = 10;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_benign(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = 10;
  __goblint_check(g == 10); //TODO
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t_benign2(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = 10;
  __goblint_check(g == 10); //TODO
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {

  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);
  pthread_join(id2, NULL);

  g = 20;
  __goblint_check(g == 20);

  // Modified while holding one of the locks that is protecting in MT mode
  pthread_mutex_lock(&A);
  g = g + 5;
  pthread_mutex_unlock(&A);

  pthread_create(&id2, NULL, t_benign2, NULL);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  __goblint_check(g == 25); //UNKNOWN!
  __goblint_check(g == 10); //UNKNOWN!
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);


  return 0;
}
