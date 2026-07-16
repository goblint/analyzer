// PARAM: --enable ana.int.interval --enable ana.sv-comp.functions --set ana.activated[+] unassume --set witness.yaml.unassume 27-mine-tutorial-ex4.7.yml
#include <goblint.h>
extern _Bool __VERIFIER_nondet_bool();
int main() {
  int x = 0;
  while (__VERIFIER_nondet_bool() == 0) { // TODO: location invariant before loop doesn't work anymore
    __goblint_check(0 <= x);
    __goblint_check(x <= 40);
    if (__VERIFIER_nondet_bool() == 0) {
      x++;
      if (x > 40)
        x = 0;
    }
  }
  return 0;
}
