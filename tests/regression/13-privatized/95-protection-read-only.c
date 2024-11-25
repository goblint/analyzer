// PARAM: --enable ana.int.enums --set ana.base.privatization protection-read
#include <pthread.h>
#include <goblint.h>

int g = 0;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m);
  int x = g;
  __goblint_check(x == 0); // UNKNOWN!
  __goblint_check(x == 1); // UNKNOWN!
  __goblint_check(x != 2); // TODO
  __goblint_check(x == 3); // UNKNOWN!
  pthread_mutex_unlock(&m);
  return NULL;
}

void *t_fun2(void *arg) {
  pthread_mutex_lock(&m);
  g = 2; // not observable by t_fun
  g = 3;
  pthread_mutex_unlock(&m);
  return NULL;
}

int main() {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun2, NULL);

  g = 1; // unprotected write, do g has no write or read+write protection, but is still read-protected by m!
  return 0;
}
