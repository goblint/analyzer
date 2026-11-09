// PARAM: --set ana.activated[+] branchSeq --set ana.branchSeq.length 2
// CRAM: Bounded branch histories retain correlations until the decision expires.
#include <goblint.h>

int x, y;

void check(void) {
  __goblint_check(x == y);
}

void choose(void) {
  int r;
  if (r)
    x = y = 5;
  else
    x = y = 10;
}

int main(void) {
  int s, z;
  choose();
  check(); // The callee's decision survives the return and next call.
  if (s)
    z = 1;
  else
    z = 2;
  __goblint_check(x == y); // Needs both recent decisions.
  return 0;
}
