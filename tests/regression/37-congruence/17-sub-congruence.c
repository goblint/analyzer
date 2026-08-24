// PARAM: --enable ana.int.congruence --disable ana.int.bitfield --disable ana.int.enums  --disable ana.int.interval --disable ana.int.def_exc

#include<limits.h>
#include<goblint.h>

int main(void)
{
  unsigned x = 0;
  unsigned y = 1;

  unsigned int z = x - y;
  __goblint_check(UINT_MAX == z);

  int a = 1;
  int b = 2;

  int c = a - b;

  __goblint_check(c == -1);

  int min = INT_MIN;
  int ov = min - 1;

  __goblint_check(ov == INT_MAX); //UNKNOWN!

  int ov2 = 0 - min;
  __goblint_check(ov2 == INT_MAX); //UNKNOWN!

  return (0);

}