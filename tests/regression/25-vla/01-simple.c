// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.trier --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
int main(void)
{
  int a[40];
  int n = 30;
  int b[n];
  b[29] = 5;
  assert(b[29] = 5);
}
