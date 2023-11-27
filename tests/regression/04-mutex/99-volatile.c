// PARAM: --disable ana.race.volatile
#include <pthread.h>

volatile int g1;

volatile int a1[1];

struct s {
  int f0;
  volatile int f1;
};

struct s s1;
volatile struct s s2;

typedef volatile int t_int1;

t_int1 t1;

typedef int t_int0;

volatile t_int0 t0;

volatile int *p0 = &g1;
int x;
int * volatile p1 = &x;
volatile int * volatile p2 = &g1;

void *t_fun(void *arg) {
  g1++; // NORACE
  a1[0]++; // NORACE
  s1.f1++; // NORACE
  s2.f0++; // NORACE
  t1++; // NORACE
  t0++; // NORACE
  (*p0)++; // NORACE
  p1++; // NORACE
  p2++; // NORACE
  (*p2)++; // NORACE

  struct s ss = {0};
  s2 = ss; // NORACE
  return NULL;
}

int main(void) {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun, NULL);
  pthread_join(id, NULL);
  pthread_join(id2, NULL);
  return 0;
}
