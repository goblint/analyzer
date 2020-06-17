//PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"

void foo2(int n , int (*a)[n] )
{
  int x ;
  int y ;

  int *ptr = *(a+7);
  assert(ptr[13] == 23);

  x = (*(a + 29))[7];
  assert(x == 23); //FAIL

  y = (*(a + 7))[13];
  assert(y == 23);

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
