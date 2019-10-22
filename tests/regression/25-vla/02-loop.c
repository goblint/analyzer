// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.trier --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
int main(void)
{
  example1();
  example2();
}

void example1(void) {
  for(int i=1;i<10;i++) {
      int a[i];
      a[i-1] = 0;
      assert(a[i-1] == 0);
  }
}

void example2(void) {
  for(int i=0; i < 47; i++) {
    int a[i+2];

    for(int j = 0; j < 2; j++) {
      a[j] = 0;
    }

    assert(a[0] == 0);
  }
}
