// PARAM: --set ana.base.structs.domain sets --enable ana.int.interval
// FIXPOINT
// Reduced from SV-COMP intel-tdx-module tasks.
// The struct is a set of two structs whose union members have different active fields.
// The meet of such unions raises Uncomparable, which escaped from narrowing the struct set,
// so the loop head was narrowed to dead code and the fixpoint was not reached.
#include <goblint.h>
extern int __VERIFIER_nondet_int(void);

union U { int a; int b; };
struct S { union U u; int x; };

int main() {
  struct S s;
  if (__VERIFIER_nondet_int())
    s.u.a = 1;
  else
    s.u.b = 2;
  int i = 0;
  while (i < 10) {
    s.x = 0;
    i++;
  }
  __goblint_check(i == 10);
  return 0;
}
