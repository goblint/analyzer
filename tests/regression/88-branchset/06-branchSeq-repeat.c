// PARAM: --set ana.activated[+] branchSeq --set ana.branchSeq.length 2
// Consecutive identical decisions do not consume history slots: the first branch
// still separates x and y after visiting the same node twice with the same outcome.
// A different outcome at that node consumes a slot and evicts the first branch.
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
