// PARAM: --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 2
// Casting a pointer to a struct to a pointer to its first member, and back.
// C11 6.7.2.1p15: a pointer to a struct, suitably converted, points to its
// first member, and conversely. Each round trip must give back the struct,
// so that a write through the struct pointer reaches the original object.
#include <goblint.h>

struct P { int *p; };
struct I { int x; };
struct Inner { int x; };
struct Outer { struct Inner in; };
struct A { int a[1]; };

int main(void) {
  // A pointer member: struct P * -> int ** -> struct P *.
  int x = 0;
  struct P s = {&x};
  int **member = (int **)&s;
  struct P *q = (struct P *)member;
  *q->p = 7;
  __goblint_check(x == 7);

  // A scalar member: struct I * -> int * -> struct I *.
  struct I i = {0};
  int *ip = (int *)&i;
  struct I *iq = (struct I *)ip;
  iq->x = 7;
  __goblint_check(i.x == 7);

  // Nested first members: struct Outer * -> int * -> struct Inner * and
  // struct Outer *.
  struct Outer o = {{0}};
  int *op = (int *)&o;
  struct Inner *inner = (struct Inner *)op;
  inner->x = 5;
  __goblint_check(o.in.x == 5);
  struct Outer *outer = (struct Outer *)op;
  outer->in.x = 6;
  __goblint_check(o.in.x == 6);

  // Arrays: int (*)[1] -> int * -> int (*)[1], and a struct holding the
  // array: struct A * -> int * -> struct A *.
  int arr[1] = {0};
  int *ap = (int *)&arr;
  int (*aq)[1] = (int (*)[1])ap;
  (*aq)[0] = 3;
  __goblint_check(arr[0] == 3);
  struct A sa = {{0}};
  int *sp = (int *)&sa;
  struct A *sq = (struct A *)sp;
  sq->a[0] = 4;
  __goblint_check(sa.a[0] == 4);

  // The address of the member itself, cast to the struct.
  struct P t = {&x};
  struct P *tq = (struct P *)&t.p;
  *tq->p = 9;
  __goblint_check(x == 9);
  return 0;
}
