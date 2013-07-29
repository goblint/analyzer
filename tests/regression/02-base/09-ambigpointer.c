// PARAM: --set ana.activated "[['base']]"
#include<pthread.h>
#include<assert.h>

int glob1 = 1;
int glob2 = 2;
int glob3 = 3;

void *f1(void *arg) {
  glob1=5;
  return NULL;
}

void *f2(void *arg) {
  glob2=5;
  return NULL;
}

int main() {
  int k;
  void *(*t_fun)(void *);
  pthread_t id;

  if (k)
    t_fun = f1;
  else
    t_fun = f2;

  pthread_create(&id, NULL, t_fun, NULL);

  k = glob1;
  assert(k == 1); // UNKNOWN

  k = glob2;
  assert(k == 2); // UNKNOWN

  k = glob3;
  assert(k == 3);

  return 0;
}

