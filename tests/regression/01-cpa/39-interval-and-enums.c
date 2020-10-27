// PARAM: --enable ana.int.interval --enable ana.int.def_exc --enable ana.int.enums  --set ana.int.enums_max 2
#include<stdio.h>
#include<assert.h>


int main () {
  int a = 1,b = 2,c = 3;
  int x,y,z;
  int w;
  int false = 0;
  int true = 42;

  if (x){
    assert(x != 0);
  } else {
    assert(x == 0);
  }

  assert(!! true);
  assert(!  false);

  if (a){
    a = a;
  } else
    assert(0); // NOWARN


  if (!a)
    assert(0); // NOWARN
  else
    a = a;

  if (z != 0){
    a = 8;
    b = 9;
  } else {
    a = 9;
    b = 8;
  }

  assert(a);
  assert(a!=b); //UNKNOWN
  assert(a<10);
  assert(a<=9);
  assert(!(a<8));
  assert(a==8); //UNKNOWN
  assert(b>7);
  assert(b>=8);
  assert(!(a>9));
  assert(b==8); //UNKNOWN

  for(x = 0; x < 10; x++){
    assert(x >= 0);
    // Because the false branch remained unreachable for more iterations, the analysis behaved differently, meaning
    // with ana.int.enums enabled, we didn't know (x >= 0) here
    assert(x <= 9);
  }
  assert(x == 10);

  if (0 <= w)
  {
  }
  else
  {
      return 0;
  }

  if (w > 0)
  {
      assert(1);
  }

  return 0;
}
