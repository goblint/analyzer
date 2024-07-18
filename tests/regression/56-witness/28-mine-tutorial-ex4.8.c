// PARAM: --disable ana.int.def_exc --enable ana.int.interval --enable ana.sv-comp.functions --set ana.activated[+] unassume --set witness.yaml.unassume 28-mine-tutorial-ex4.8.yml
#include <goblint.h>
extern _Bool __VERIFIER_nondet_bool();
int main() {
  int v = 0;
  while (__VERIFIER_nondet_bool() == 0) { // TODO: location invariant before loop doesn't work anymore
    __goblint_check(0 <= v);
    __goblint_check(v <= 1);
    if (v == 0)
      v = 1;
    // ...
  }
  return 0;
}
