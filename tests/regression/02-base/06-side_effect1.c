#include<pthread.h>
#include <goblint.h>

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;
int glob1 = 5;
int glob2 = 7;
int glob3 = 9;

void *t_fun(void *arg) {
  glob3 = 9;
  __goblint_check(glob3 == 9); // UNKNOWN!
  return NULL;
}

int main() {
  pthread_t id;
  glob3 = 9;
  __goblint_check(glob3 == 9);
  pthread_create(&id, NULL, t_fun, NULL);

  glob1 = 5;
  __goblint_check(glob1 == 5);

  glob2 = 5;
  __goblint_check(glob2 == 5); // TODO

  glob3 = 8;
  __goblint_check(glob3 == 8); // UNKNOWN!

  return 0;
}
