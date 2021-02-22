#include<stdio.h>

extern void abort(void);
void reach_error(){}

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: {reach_error();abort();}
  }
  return;
}


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
  __VERIFIER_assert(k.i == 7);
  // __VERIFIER_assert(k.p == 7.0); // UNKNOWN!

  maja.arv = 8;
  maja.kala.i = 3;
  __VERIFIER_assert(maja.kala.j == 3);
  // __VERIFIER_assert(maja.kala.p == 3.0); // UNKNOWN!

  k.struk.x = 3;
  __VERIFIER_assert(k.struk.x == 3);
  // __VERIFIER_assert(k.i == 3); // UNKNOWN!

  return 0;
}
