// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] branchSetFull --set ana.path_sens[+] branchSetFull --enable ana.sv-comp.functions
// Reduced from SV-COMP nla-digbench-scaling/fermat1-ll_unwindbound5.
// With path-sensitive branch sets, the analysis keeps a separate state (partition) for each combination of branches taken.
// The partition in which b != 0 holds, but neither b > 0 nor b < 0, is unreachable.
// Neither analysis detects this on its own:
// base can only exclude 0 from b, while the octagon cannot express b != 0 and only knows 0 <= b <= 0.
#include <goblint.h>

int main() {
  long a, b;
  while (1) {
    // In the unreachable partition (from the previous iteration), evaluating b meets b != 0 (base) and b == 0 (octagon) to bottom.
    // Without treating this as dead code, multiplying this bottom value raised ArithmeticOnIntegerBot here.
    __goblint_check(b * a); // UNKNOWN!

    if (b != 0) { // base: b != 0
      if (b > 0) { // not taken in the unreachable partition, so the octagon knows b <= 0
      }
    }
    if (b < 0) { // not taken in the unreachable partition, so the octagon knows b >= 0
    }
  }
}
