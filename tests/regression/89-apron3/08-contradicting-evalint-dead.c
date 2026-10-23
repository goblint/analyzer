// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] branchSetFull --set ana.path_sens[+] branchSetFull --enable ana.sv-comp.functions
// Reduced from SV-COMP nla-digbench-scaling/fermat1-ll_unwindbound5.
// With path-sensitive branch sets, the analysis keeps a separate abstract state for each combination of branch outcomes.
// The path with b != 0, but neither b > 0 nor b < 0, is infeasible in the concrete, but the analysis still reaches it abstractly:
// base can only exclude 0 from b, while the octagon cannot express b != 0 and ends up with b == 0.
// Base and the octagon are not reduced against each other after the branches, so neither detects the contradiction on its own.
// The comments at the branches give the outcome on the infeasible path and the resulting states.
#include <goblint.h>

int main() {
  long a, b;
  while (1) {
    // On the infeasible path from the previous iteration, evaluating b meets Not {0} (base) and 0 (octagon) to bottom.
    // Without treating this as dead code, multiplying this bottom value raised ArithmeticOnIntegerBot here.
    __goblint_check(b * a); // UNKNOWN!

    if (b != 0) { // true: base b -> Not {0}, octagon top
      if (b > 0) { // false: base b -> Not {0}, octagon b <= 0
      }
    }
    if (b < 0) { // false: base b -> Not {0}, octagon b == 0
    }
  }
}
