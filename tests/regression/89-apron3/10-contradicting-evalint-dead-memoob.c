// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] branchSetFull --set ana.path_sens[+] branchSetFull --enable ana.sv-comp.functions --set ana.activated[+] memOutOfBounds --enable ana.arrayoob
// Like 08-contradicting-evalint-dead, but b is used as an array index or as a memset size.
// In the unreachable partition (b != 0 in base, 0 <= b <= 0 in the octagon), b evaluates to bottom.
// Without treating this as dead code, the out-of-bounds checks raised ArithmeticOnIntegerBot on these accesses.
#include <string.h>
#include <goblint.h>

int main() {
  long a, b;
  int arr[10];
  char buf[10];
  int use_array; // separate loops, so that each access is reached in the unreachable partition
  if (use_array) {
    while (1) {
      arr[b] = 1; // WARN (out of bounds in reachable partitions)

      if (b != 0) { // base: b != 0
        if (b > 0) { // not taken in the unreachable partition, so the octagon knows b <= 0
        }
      }
      if (b < 0) { // not taken in the unreachable partition, so the octagon knows b >= 0
      }
    }
  }
  else {
    while (1) {
      memset(buf, 0, b); // WARN (out of bounds in reachable partitions)

      if (b != 0) { // base: b != 0
        if (b > 0) { // not taken in the unreachable partition, so the octagon knows b <= 0
        }
      }
      if (b < 0) { // not taken in the unreachable partition, so the octagon knows b >= 0
      }
    }
  }
}
