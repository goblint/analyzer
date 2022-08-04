// PARAM: --set ana.activated[+] expRelation
#include <assert.h>
#include <stddef.h>

int main() {
  int x, y, z;

  // expRelation EvalInt
  __goblint_check(x + y - z == x - z + y);

  // base eval_rv_ask_mustbeequal via expRelation
  __goblint_check((x + y - z) - (x - z + y) == 0);
  __goblint_check(x + y - z <= x - z + y);
  __goblint_check(x + y - z >= x - z + y);
  __goblint_check(!(x + y - z != x - z + y));
  __goblint_check(!(x + y - z < x - z + y));
  __goblint_check(!(x + y - z > x - z + y));

  int *p, *q;

  // base eval_rv_ask_mustbeequal via expRelation
  __goblint_check(p - p == 0);
  return 0;
}