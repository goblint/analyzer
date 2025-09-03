// PARAM: --disable ana.int.def_exc --enable ana.int.bitfield
#include <goblint.h>

int main() {
  int i1 = 1;
  int i123 = 123;
  int im123 = -123;
  int i128 = 128;
  int im128 = -128;
  int i0 = 0;
  int irand; // rand

  // assigning constant from other variable instead of integer constant, because integer constant swallows implicit cast to _Bool
  _Bool b1 = i1;
  _Bool b123 = i123; // TODO NOWARN (overflow)
  _Bool bm123 = im123; // TODO NOWARN (underflow)
  _Bool b128 = i128; // TODO NOWARN (overflow)
  _Bool bm128 = im128; // TODO NOWARN (underflow)
  _Bool bf = i0;
  _Bool brand = irand; // TODO NOWARN (underflow, overflow)

  __goblint_check(b1);
  __goblint_check(b1 == 1);
  __goblint_check(b123);
  __goblint_check(b123 == 1);
  __goblint_check(bm123);
  __goblint_check(bm123 == 1);
  __goblint_check(b128); // TODO
  __goblint_check(b128 == 1); // TODO
  __goblint_check(bm128); // TODO
  __goblint_check(bm128 == 1); // TODO
  __goblint_check(!bf);
  __goblint_check(bf == 0);
  __goblint_check(brand); // UNKNOWN!
  __goblint_check(brand == 0); // UNKNOWN!
  __goblint_check(brand == 1); // UNKNOWN!
  return 0;
}
