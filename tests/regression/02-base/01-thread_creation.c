#include<pthread.h>
#include <goblint.h>

void *t_fun(void *arg) {
  return NULL;
}

int glob1;
int glob2 = 9;

int main() {
  int i=7;
  pthread_t id;

  // Check that initializers are working
  __goblint_check(glob1 == 0);
  __goblint_check(glob2 == 9);
  __goblint_check(i == 7);

  // Globals are not side-effected yet
  glob1 = 7;
  __goblint_check(glob1 == 7);

  // Creat the thread
  pthread_create(&id, NULL, t_fun, NULL);

  // The values should remain the same
  __goblint_check(glob1 == 7);
  __goblint_check(glob2 == 9);
  __goblint_check(i == 7);

  return 0;
}
