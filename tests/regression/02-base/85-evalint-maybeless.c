// PARAM: --set ana.activated[+] expRelation
#include <assert.h>
#include <stddef.h>

int main() {
  int x;

  // expRelation EvalInt
  assert(!(x + 1 < x));
  assert(!(x < x + (-1)));
  assert(!(x - (-1) < x));
  assert(!(x < x - 1));
  return 0;
}