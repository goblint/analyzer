// SKIP PARAM: --set ana.activated[+] apron
// Torture EvalInt queries using large number of (relational) expressions.
// Ran in ~7s with MustBeEqual, MayBeEqual and MayBeLess queries.
#include <assert.h>

int main() {
  int x, y, z, w;
  x = 1;
  y = 2;
  z = 3;
  w = 4;

#define A1 assert(x + y + z + w == 10);
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

  A4096

  return 0;
}