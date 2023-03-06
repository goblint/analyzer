// PARAM: --enable ana.int.interval
#include <stdlib.h>
#include <goblint.h>

int main() {
  unsigned long ul;

  if (ul <= 0UL) {
    __goblint_check(ul == 0UL);
  } else {
    __goblint_check(ul != 0UL);
  }

  if (ul > 0UL) {
    __goblint_check(ul != 0UL);
  } else {
    __goblint_check(ul == 0UL);
  }

  if (! (ul > 0UL)) {
    __goblint_check(ul == 0UL);
  } else {
    __goblint_check(ul != 0UL);
  }

  unsigned int iu;
  if (iu <= 0UL) {
    __goblint_check(iu == 0UL);
  } else {
    __goblint_check(iu != 0UL);
  }

  if (iu > 0UL) {
    __goblint_check(iu != 0UL);
  } else {
    __goblint_check(iu == 0UL);
  }

  if (! (iu > 0UL)) {
    __goblint_check(iu == 0UL);
  } else {
    __goblint_check(iu != 0UL);
  }


  int i;
  if (! (i > 0)) {

  } else {
    __goblint_check(i != 0);
  }

  return 0;
}
