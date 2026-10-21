// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 11-base-unassume-interval.yml
#include <assert.h>

int main() {
  int i = 0;
  while (i < 100) { // TODO: location invariant before loop doesn't work anymore
    i++;
  }
  assert(i == 100);
  return 0;
}

// without unassuming: vars = 13    evals = 14
// with unassuming: vars = 13    evals = 8
