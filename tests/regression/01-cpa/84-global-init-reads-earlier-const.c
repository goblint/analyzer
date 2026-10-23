// GCC and Clang accept a global initializer that reads a const-qualified
// global initialized earlier in the file. Each global below must hold the
// value its initializer computes from the one before it.
#include <goblint.h>

int a = 5;
static int * const pa = &a;
int *q = pa;
int *q2 = pa;
int *q3 = pa;

static const unsigned long k0 = 0;
static const unsigned long k1 = 1;
static const unsigned long k2 = 2;
static const unsigned long k3 = 3;
unsigned long m1 = (1UL << k0) | (1UL << k1);
unsigned long m2 = (1UL << k2) | (1UL << k3);
unsigned long m3 = k3 + 1;
int pad = 3;

int main(void) {
  __goblint_check(m1 == 3);
  __goblint_check(m2 == 12);
  __goblint_check(m3 == 4);
  __goblint_check(q == &a);
  __goblint_check(q2 == &a);
  __goblint_check(q3 == &a);
  *q = 7;
  __goblint_check(a == 7);
  *q2 = 8;
  __goblint_check(a == 8);
  *q3 = 9;
  __goblint_check(a == 9);
  return 0;
}
