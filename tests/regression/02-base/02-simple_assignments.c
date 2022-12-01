#include<pthread.h>
#include <goblint.h>

void *t_fun(void *arg) {
  return NULL;
}

int glob1 = 5;
int glob2 = 7;

int main() {
  int i = 3;
  pthread_t id;

  // Create the thread
  pthread_create(&id, NULL, t_fun, NULL);

  // Simple assignments to only locals
  __goblint_check(i == 3);
  i = 9;
  __goblint_check(i == 9);

  // simple assignments to globals
  glob1 = 5;
  __goblint_check(glob1 == 5);
  glob2 = 5;
  __goblint_check(glob2 == 5); // TODO

  return 0;
}
