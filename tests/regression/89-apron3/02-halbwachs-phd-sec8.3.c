// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra
// fully works with --set ana.widen.delay.local 6
// From Nicolas Halbwachs' PhD thesis, section 8.3.
// Same as 01-halbwachs-phd-sec5.6.2 annotated with an explicit termination counter.
#include <goblint.h>

int main() {
  unsigned int i = 0;
  unsigned int j = 0;
  unsigned int k = 0; // explicit termination counter
  while (k++, i <= 10) {
    __goblint_check(j >= 0);
    __goblint_check(i + 2 * j + 4 == 4 * k);
    __goblint_check(i <= 10);
    __goblint_check(2 * k <= i + 2);

    int r; // rand
    if (r) {
      i += 2;
      j++;
    }
    else
      i += 4;

    __goblint_check(j >= 0);
    __goblint_check(i + 2 * j == 4 * k);
    __goblint_check(i <= 14);
    __goblint_check(1 <= k);
    __goblint_check(k <= 6);
    __goblint_check(2 * k <= i);
  }

  __goblint_check(j >= 0);
  __goblint_check(i + 2 * j + 4 == 4 * k); // TODO: needs one narrowing iteration?
  __goblint_check(11 <= i);
  __goblint_check(i <= 14); // TODO: needs one narrowing iteration?
  __goblint_check(2 * k <= i + 2); // TODO: needs one narrowing iteration?

  // if i and j are projected out:
  __goblint_check(4 <= k);
  __goblint_check(k <= 7); // TODO: needs one narrowing iteration?
  return 0;
}
