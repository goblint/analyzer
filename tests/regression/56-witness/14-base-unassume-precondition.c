// PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 14-base-unassume-precondition.yml --set witness.yaml.entry-types[+] loop_invariant --set witness.yaml.entry-types[+] precondition_loop_invariant
#include <assert.h>

void foo(int n) {
  int i = 0;
  while (i < n) { // TODO: (precondition) location invariant before loop doesn't work anymore
    i++;
  }
  assert(i == n);
}

int main() {
  foo(50);
  foo(100);
  return 0;
}

// without unassuming: vars = 25    evals = 32
// with unassuming: vars = 25    evals = 20
