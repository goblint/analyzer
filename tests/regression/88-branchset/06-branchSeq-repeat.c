// PARAM: --set ana.activated[+] branchSeq --set ana.branchSeq.k 2
// CRAM: Only consecutive identical branch decisions are skipped.
#include <goblint.h>

int marker;

void visit(int r) {
  if (r)
    marker = 1;
  else
    marker = 2;
}

int main(void) {
  int r, x, y;
  if (r)
    x = y = 5;
  else
    x = y = 10;
  visit(1);
  visit(1);
  __goblint_check(x == y); // The repeated decision must not evict the first one.
  visit(0);
  __goblint_check(x == y); // UNKNOWN: A changed outcome consumes a history slot.
  return 0;
}
