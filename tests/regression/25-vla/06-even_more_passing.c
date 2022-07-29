//PARAM: --set solver td3 --enable ana.int.interval --disable ana.int.def_exc --set ana.base.arrays.domain partitioned --set ana.base.privatization none
#include <assert.h>

void foo2(int n , int (*a)[n] )
{
  int x ;
  int y ;

  int *ptr = *(a+7);
  __goblint_check(ptr[13] == 23);

  x = (*(a + 29))[7];
  __goblint_check(x == 23); //FAIL

  y = (*(a + 7))[13];
  __goblint_check(y == 23);

  return;
}

int main(void)
{
  int r = 40;
  int c[40][40];
  int d[r][r];

  for(int i = 0; i < 40;i++) {
    for(int j=0;j < 40;j++) {
      c[i][j] = 0;
      d[i][j] = 0;
    }
  }

  c[7][13] = 23;
  d[7][13] = 23;


  foo2(40, c);
  foo2(40, d);

  return (0);
}
