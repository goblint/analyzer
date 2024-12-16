// PARAM: --set ana.base.privatization mine-W-noinit --enable ana.int.enums
#include <pthread.h>
#include <goblint.h>

int g;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  return NULL;
}

void *t_fun2(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_unlock(&A); // spuriously publishes g = 8
  return NULL;
}

int main() {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL); // enter multithreaded

  pthread_mutex_lock(&A);
  g = 8;
  pthread_create(&id2, NULL, t_fun2, NULL); // passes g = 8 and W: A -> {g} to t_fun2
  g = 0;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  __goblint_check(g == 0); // TODO
  pthread_mutex_unlock(&A);
  return 0;
}
