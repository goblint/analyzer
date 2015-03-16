// PARAM: --set ana.activated "['base','escape']"

#include<pthread.h>
#include<assert.h>

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER; 
int glob1 = 5;
int glob2 = 7;
int glob3 = 9;

void *t_fun(void *arg) {
  glob3 = 9;
  assert(glob3 == 9); // UNKNOWN
  return NULL;
}

int main() {
  pthread_t id;
  glob3 = 9;
  assert(glob3 == 9);
  pthread_create(&id, NULL, t_fun, NULL);

  glob1 = 5;
  assert(glob1 == 5);

  glob2 = 5;
  assert(glob2 == 5); // UNKNOWN

  glob3 = 8;
  assert(glob3 == 8); // UNKNOWN

  return 0;
}
