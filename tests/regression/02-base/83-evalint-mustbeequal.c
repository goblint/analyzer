// PARAM: --set ana.activated[+] expRelation
#include <assert.h>
#include <stddef.h>

int main() {
  int x, y, z;

  // expRelation EvalInt
  assert(x + y - z == x - z + y);

  // base eval_rv_ask_mustbeequal via expRelation
  assert((x + y - z) - (x - z + y) == 0);
  assert(x + y - z <= x - z + y);
  assert(x + y - z >= x - z + y);
  assert(!(x + y - z != x - z + y));
  assert(!(x + y - z < x - z + y));
  assert(!(x + y - z > x - z + y));

  int *p, *q;

  // base eval_rv_ask_mustbeequal via expRelation
  assert(p - p == 0);
  return 0;
}