// SKIP TERM PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set ana.activated[+] termination
// fully works with --set ana.widen.delay.local 6
// Somehow we prove termination even without and don't have good bounds on termination counter.
// From Nicolas Halbwachs' PhD thesis, section 5.6.2.
#include <goblint.h>

int main() {
  unsigned int i = 0;
  unsigned int j = 0;
  while (i <= 10) {
    __goblint_check(j >= 0);
    __goblint_check(i - 2 * j >= 0);
    __goblint_check(i <= 10);

    int r; // rand
    if (r) {
      i += 2;
      j++;
    }
    else
      i += 4;
  }

  __goblint_check(j >= 0);
  __goblint_check(i - 2 * j >= 0);
  __goblint_check(11 <= i);
  __goblint_check(i <= 14); // TODO: needs one narrowing iteration
  __goblint_check(i + 2 * j <= 24); // TODO: needs one narrowing iteration
  return 0;
}
