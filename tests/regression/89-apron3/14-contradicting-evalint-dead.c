// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] branchSetFull --set ana.path_sens[+] branchSetFull --enable ana.sv-comp.functions
// Reduced from SV-COMP nla-digbench-scaling/fermat1-ll_unwindbound5.
// In the partition taking the branches b != 0, !(b > 0) and !(b < 0), base only knows b != 0, while the octagon knows b == 0.
// The partition is unreachable, but neither analysis detects this on its own.
// Evaluating b * a there met the contradicting answers to bottom, which crashed with ArithmeticOnIntegerBot during postsolving.
#include <goblint.h>

int main() {
  long a, b;
  while (1) {
    __goblint_check(b * a); // UNKNOWN!
    if (b)
      if (b > 0)
        ;
    if (b < 0)
      ;
  }
}
