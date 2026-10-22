// PARAM: --enable ana.int.interval
// Reading a scalar member of a union through another member than the one written reinterprets its bytes.
// The value of the written member can only be used if both members have the same representation.
#include <goblint.h>
#include <limits.h>

union CQ { unsigned char c; unsigned long long q; };
union SI { signed char c; int i; };
union IC { int i; char c; };
union IU { int i; unsigned int u; };
union II { int x; int y; };

int main() {
  union CQ cq;
  cq.c = 1;
  __goblint_check(cq.q == 1); // UNKNOWN! (the other bytes of q are unspecified)

  union SI si;
  si.c = -1;
  __goblint_check(si.i == -1); // UNKNOWN! (the other bytes of i are unspecified)

  union IC ic;
  ic.i = 0x0102;
  __goblint_check(ic.c == 2); // UNKNOWN! (depends on the byte order)

  union IU iu;
  iu.i = -1;
  __goblint_check(iu.u == UINT_MAX); // same size, only the signedness differs

  union II ii;
  ii.x = 3;
  __goblint_check(ii.y == 3); // same type

  // after joining writes through different members, the member written last is unknown
  int r;
  union II jj;
  if (r) jj.x = 0; else jj.y = 0;
  __goblint_check(jj.x == 0);
  union SI sj;
  if (r) sj.c = 1; else sj.i = 1;
  __goblint_check(sj.i == 1); // UNKNOWN!
  __goblint_check(sj.c == 1); // UNKNOWN!
  return 0;
}
