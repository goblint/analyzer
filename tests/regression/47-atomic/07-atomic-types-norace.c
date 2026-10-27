#include <pthread.h>
#include <stdatomic.h>

atomic_int g1;
_Atomic int g2;
_Atomic(int) g3;

atomic_int a1[1];
_Atomic int a2[1];
_Atomic(int) a3[1];

struct s {
  int f0;
  atomic_int f1;
  _Atomic int f2;
  _Atomic(int) f3;
};

struct s s1;
_Atomic struct s s2;
_Atomic(struct s) s3;

typedef atomic_int t_int1;
typedef _Atomic int t_int2;
typedef _Atomic(int) t_int3;

t_int1 t1;
t_int2 t2;
t_int3 t3;

typedef int t_int0;

_Atomic t_int0 t0;
_Atomic(t_int0) t00;

atomic_int *p0 = &g1;
int x;
// int * _Atomic p1 = &x; // TODO: https://github.com/goblint/cil/issues/64
// _Atomic(int*) p2 = &x; // TODO: https://github.com/goblint/cil/issues/64
// atomic_int * _Atomic p3 = &g1; // TODO: https://github.com/goblint/cil/issues/64

atomic_flag flag = ATOMIC_FLAG_INIT;

void *t_fun(void *arg) {
  g1++; // NORACE
  g2++; // NORACE
  g3++; // NORACE
  a1[0]++; // NORACE
  a2[0]++; // NORACE
  a3[0]++; // NORACE
  s1.f1++; // NORACE
  s1.f2++; // NORACE
  s1.f3++; // NORACE
  s2.f0++; // NORACE
  s3.f0++; // NORACE
  t1++; // NORACE
  t2++; // NORACE
  t3++; // NORACE
  t0++; // NORACE
  t00++; // NORACE
  (*p0)++; // NORACE
  // p1++; // TODO NORACE: https://github.com/goblint/cil/issues/64
  // p2++; // TODO NORACE: https://github.com/goblint/cil/issues/64
  // p3++; // TODO NORACE: https://github.com/goblint/cil/issues/64
  // (*p3)++; // TODO NORACE: https://github.com/goblint/cil/issues/64

  struct s ss = {0};
  s2 = ss; // NORACE
  s3 = ss; // NORACE

  atomic_flag_clear(&flag); // NORACE
  atomic_flag_test_and_set(&flag); // NORACE
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
