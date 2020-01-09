// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
int main(void)
{
  example1();
  example2();
  example3();
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

void example3(void) {
  for(int i = 2; i < 47; i++) {
    int n = 1;
    int a[1];

    if(i == 2) {
      a[0] = 42;
    }

    assert(a[0] == 42); //UNKNOWN
  }
}
