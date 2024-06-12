// SKIP PARAM: --set ana.activated[+] apron --set ana.relation.privatization mutex-meet
// NOTIMEOUT
#include <pthread.h>
#include <goblint.h>

int g;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void fn() {
  // Just do nothing
  return;
}

void *t_fun(void *arg) {
  pthread_mutex_lock(&m);
  g = g+1;
  fn();
  pthread_mutex_unlock(&m);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  return 0;
}
