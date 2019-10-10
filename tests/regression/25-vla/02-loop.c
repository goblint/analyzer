// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.trier --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
int main(void)
{
  for(int i=1;i<10;i++) {
      int a[i];
      a[i-1] = 0;
      assert(a[i-1] == 0);
  }
}
