// PARAM: --set ana.base.structs.domain keyed
// The union u is chosen as the key of the keyed struct domain.
// Joining structs whose keys are unions with different active fields raised Uncomparable from the meet of the keys,
// so the code after the join was considered dead.
#include <goblint.h>
extern int __VERIFIER_nondet_int(void);

union U { int a; int b; };
struct S { union U u; int x; };

int main() {
  int y = 5;
  struct S s;
  s.x = 3;
  if (__VERIFIER_nondet_int())
    s.u.a = 1;
  else
    s.u.b = 2;
  __goblint_check(y == 5);
  __goblint_check(s.x == 3);
  return 0;
}
