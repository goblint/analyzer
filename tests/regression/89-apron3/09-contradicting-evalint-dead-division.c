// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] branchSetFull --set ana.path_sens[+] branchSetFull --enable ana.sv-comp.functions
// Like 08-contradicting-evalint-dead, but with a division by b.
// Before the assignment is evaluated by base, the relational analysis asks base whether the division may overflow.
// On the infeasible path (base b -> Not {0}, octagon b == 0), the operands evaluate to bottom.
// Without treating this as dead code, checking whether the divisor may be zero failed with "unsupported: equal_to with bottom".
#include <goblint.h>

int main() {
  long a, b;
  while (1) {
    a = 100 / b; // WARN (b may be zero on feasible paths)

    if (100 / b) { // WARN (b may be zero); for conditions, the overflow check is asked as well
      a = 1;
    }

    if (b != 0) { // true: base b -> Not {0}, octagon top
      if (b > 0) { // false: base b -> Not {0}, octagon b <= 0
      }
    }
    if (b < 0) { // false: base b -> Not {0}, octagon b == 0
    }
  }
}
