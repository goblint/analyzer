// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] branchSetFull --set ana.path_sens[+] branchSetFull --enable ana.sv-comp.functions
// Like 14-contradicting-evalint-dead, but with a division by b.
// Before the assignment is evaluated by base, the relational analysis asks base whether the division may overflow.
// In the unreachable partition (b != 0 in base, 0 <= b <= 0 in the octagon), the operands evaluate to bottom.
// Without treating this as dead code, checking whether the divisor may be zero failed with "unsupported: equal_to with bottom".
#include <goblint.h>

int main() {
  long a, b;
  while (1) {
    a = 100 / b; // WARN (b may be zero in reachable partitions)

    if (100 / b) { // WARN (b may be zero); for conditions, the overflow check is asked as well
      a = 1;
    }

    if (b != 0) { // base: b != 0
      if (b > 0) { // not taken in the unreachable partition, so the octagon knows b <= 0
      }
    }
    if (b < 0) { // not taken in the unreachable partition, so the octagon knows b >= 0
    }
  }
}
