// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.trier --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
int main(void)
{
  int a[40];
  int n = 30;

  // Check one-dimensional first
  int b[n];
  b[29] = 5;
  assert(b[29] = 5);

  int c[n+4];
  c[31] = 2;
  assert(c[31] = 2);

  // Two dimensional, one variable
  int d[n][30];
  d[2][2] = 42;
  assert(d[2][2] == 42);

}
