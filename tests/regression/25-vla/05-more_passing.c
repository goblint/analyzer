//PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.trier --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
# include<stdio.h>

void foo(int n, int a[n]) {
  int x = a[7];
  assert(x == 42);
}

void fooo(int n, int a[n][n]) {
  assert(a[29][7] == 42);
  int *ptr = a[29];
  int x = *(ptr+7);
  printf("x is %d", x);
  assert(x == 42);
}

void foo2(int n, int a[50][n]) {
  assert(a[29][7] == 42);
  assert(a[29][7] == 0); //FAIL
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
}
