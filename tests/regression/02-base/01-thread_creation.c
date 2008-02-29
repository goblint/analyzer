#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  return NULL;
}

int glob1;
int glob2 = 9;

int main() {
  int i=7;
  pthread_t id;

  // Check that initializers are working
  assert(glob1 == 0);
  assert(glob2 == 9);
  assert(i == 7);

  // Globals are not side-effected yet
  glob1 = 7;
  assert(glob1 == 7);

  // Creat the thread
  pthread_create(&id, NULL, t_fun, NULL);

  // The values should remain the same
  assert(glob1 == 7);
  assert(glob2 == 9);
  assert(i == 7);

  return 0;
}
