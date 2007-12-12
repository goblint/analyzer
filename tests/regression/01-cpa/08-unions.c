#include<stdio.h>
#include<assert.h>

struct nested {
  int x;
  int y;
};

union kala {
  int i;
  int j;
  float p;
  struct nested struk;
};

struct maja {
  int arv;
  union kala kala;
};

int main () {
  union kala k;
  struct maja maja;

  k.i = 5;
  k.j = 7;
  assert(k.i == 7);
  assert_unknown(k.p);

  maja.arv = 8;
  maja.kala.i = 3;
  assert(maja.kala.j == 3);
  assert_unknown(maja.kala.p);

  k.struk.x = 3;
  assert(k.struk.x == 3);
  assert_unknown(k.i);

  return 0;
}
