//PARAM: --set solver td3 --enable ana.int.interval --disable ana.int.def_exc --set ana.base.arrays.domain partitioned --set ana.base.privatization none
#include<stdio.h>
#include <assert.h>

void foo(int n, int a[n]) {
  int x = a[7];
  __goblint_check(x == 42);
}

void fooo(int n, int a[n][n]) {
  __goblint_check(a[29][7] == 42);
  int *ptr = a[29];
  int x = *(ptr+7);
  printf("x is %d", x);
  __goblint_check(x == 42);
}

void foo2(int n, int a[50][n]) {
  __goblint_check(a[29][7] == 42);
  __goblint_check(a[29][7] == 0); //FAIL
}

// This is quite ugly, but valid C99
void foo3(int n, int b[n], int a[n][b[0]]) {
  __goblint_check(a[29][7] == 42);
}

void foo4(int n, int m, int r, int a[n][m][r]) {
  __goblint_check(a[3][3][2] == 42);
}

int main(void)
{
  // One-dimensional arrays
  int a[40];
  a[7] = 42;
  foo(40, a);

  int x;

  if(x < 8) {
     x = 347;
  }

  int b[x];
  b[7] = 42;

  foo(x, b);

  //Two dimensional arrays
  int c[50][50];

  for(int i = 0; i < 50;i++) {
    for(int j=0;j < 50;j++) {
      c[i][j] = 0;
    }
  }

  c[29][7] = 42;

  foo2(50,c);
  fooo(50, c);

  int x[50];
  b[0] = 50;
  foo3(50, x, c);

  int n = 15;
  int m = 47;
  int r = 11;

  int d[n][m][r];
  d[3][3][2] = 42;
  foo4(n,m,r,d);
}
