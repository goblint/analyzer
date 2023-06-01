// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 13-base-unassume-branch.yml
#include <assert.h>

int main() {
  int i = 0;
  while (i < 100) {
    i++;
  }
  assert(i == 100);
  return 0;
}

// without unassuming: vars = 13    evals = 14
// with unassuming: vars = 13    evals = 12
