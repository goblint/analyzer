// SKIP PARAM: --set ana.activated[+] apron --disable ana.base.eval.deep-query
// Torture EvalInt queries using large number of (relational) expressions.

// Previously using with MustBeEqual, MayBeEqual and MayBeLess queries:
// Ran in 3.0-3.5s.
// ./goblint --set ana.activated[+] apron --enable printstats ./tests/regression/46-apron2/18-evalint-torture.c

// Without those queries, but with deep-query:
// Ran in 2.4-2.7s.
// ./goblint --set ana.activated[+] apron --enable printstats ./tests/regression/46-apron2/18-evalint-torture.c --enable ana.base.eval.deep-query

// Without those queries and deep-query:
// Ran in 1.6-1.7s.
// ./goblint --set ana.activated[+] apron --enable printstats ./tests/regression/46-apron2/18-evalint-torture.c --disable ana.base.eval.deep-query

#include <assert.h>

int main() {
  int x, y, z, w;
  x = 1;
  y = 2;
  z = 3;
  w = 4;

#define A1 __goblint_check(x + y + z + w == 10); // NOWARN (macro)
#define A2 A1 A1
#define A4 A2 A2
#define A8 A4 A4
#define A16 A8 A8
#define A32 A16 A16
#define A64 A32 A32
#define A128 A64 A64
#define A256 A128 A128
#define A512 A256 A256
#define A1024 A512 A512
#define A2048 A1024 A1024
#define A4096 A2048 A2048

  A4096 // SUCCESS (via macros)

  return 0;
}