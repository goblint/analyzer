// PARAM: --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid --enable ana.int.interval --set ana.activated[+] threadJoins
#include <pthread.h>
#include <goblint.h>

int g = 10;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;


void *t_benign(void *arg) {
  return NULL;
}

void *t_benign2(void *arg) {
  pthread_mutex_lock(&A);
  // For evaluations that happen before the side-effect of the unlock of A, g is bot.
  // This caused an excpetion in query_evalint which we now catch when we are not in verify mode.
  __goblint_check(g == 40); //UNKNOWN!
  __goblint_check(g == 30); //UNKNOWN!
  __goblint_check(g == 10); //FAIL
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {

  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);
  pthread_join(id2, NULL);

  pthread_mutex_lock(&A);
  g = 30;
  pthread_create(&id2, NULL, t_benign2, NULL);
  g = 40;
  pthread_mutex_unlock(&A);

  return 0;
}
