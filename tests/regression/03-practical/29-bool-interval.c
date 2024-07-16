//PARAM: --enable ana.int.interval --disable ana.int.def_exc
#include <stdbool.h>
#include <goblint.h>

int main() {
  int a;
  int *p = &a;

  bool x = p;
  __goblint_check(x); //UNKNOWN (not null cannot be expressed in interval domain)
  bool y = !!p;
  __goblint_check(y);
  bool z = p != 0;
  __goblint_check(z);

  int b = 10;
  bool bb = b;
  __goblint_check(bb);

  return 0;
}