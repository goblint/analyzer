// PARAM: --set solver td3 --enable ana.int.interval --disable ana.int.def_exc --set ana.base.arrays.domain partitioned  --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper','assert']" --set ana.base.privatization none
#include <assert.h>

int main(void)
{
  int a[40];
  int n = 30;
  int m = 20;

  // Check one-dimensional first
  int b[n];
  b[29] = 5;
  assert(b[29] = 5);

  int c[n+4];
  c[31] = 2;
  assert(c[31] = 2);

  // Two dimensional, one variable, first
  int d[n][30];
  d[2][2] = 42;
  assert(d[2][2] == 42);

  // Two dimensional, one variable, last
  int e[20][n];
  e[2][2] = 42;
  assert(e[2][2] == 42);

  // Two dimensional, two variable
  int f[m][n];
  f[2][2] = 42;
  assert(f[2][2] == 42);
}
