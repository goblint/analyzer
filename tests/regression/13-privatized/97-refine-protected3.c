// PARAM: --set ana.base.privatization mutex-meet-tid --set ana.path_sens[+] threadflag
#include <pthread.h>
#include <goblint.h>

int g = 0;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 1;
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&A);
  __goblint_check(g == 1);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  if (g) // protected globals should be refined, but should not emit writes that ruin check in t_fun
    __goblint_check(g);
  else
    __goblint_check(!g);
  pthread_mutex_unlock(&A);
  return 0;
}
