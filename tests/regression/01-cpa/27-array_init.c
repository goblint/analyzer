// SKIP: This is a known problem! For arrays, it is addressed by enabling the partitioned arrays.
// But we assume uninitialized stuff is not used.

#include<stdio.h>
#include <goblint.h>

int main () {
  int a[] = {2,2,2};
  int b[2];

  __goblint_check(a[0] == 2);
  __goblint_check(a[1] == 2);
  __goblint_check(a[2] == 2);

  b[0] = 3;
  __goblint_check(b[1] == 3); // UNKNOWN

  return 0;
}
