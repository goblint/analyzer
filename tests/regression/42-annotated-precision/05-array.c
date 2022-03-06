// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint
#include<stdio.h>
#include<stdbool.h>
#include<assert.h>

void f(int in[], int len) __attribute__ ((goblint_precision("no-def_exc","interval", "congruence")));
void g(bool in[], int len) __attribute__ ((goblint_precision("interval", "enums", "congruence")));
int main() __attribute__ ((goblint_precision("interval")));


void f(int in[], int len) {
  assert(in[0]); // FAIL!
  int c[len];
  for (int i = 0; i < len; i++) {
    c[i] = 1;
  }
  //memcpy(in, c, len); // not working "Spawning functions from unkown function: a"
  return;
}

void g(bool in[], int len) {
  for (int i = 0; i < len; i++) {
    in[i] ^= true;
  }
  return;
}

int main() {
  int a[] = {0,0,0};
  bool b[] = {true, false};
  char s[][] = {"Edward","Tom","Julia"};

  assert(a[0]); // FAIL!
  assert(a[0] == a[1]);

  f(a, (int) (sizeof(a) / sizeof(int)));
  assert(a[0]); // FAIL!
  g(b, (int) (sizeof(b) / sizeof(bool)));
  a[1] = 1;
  assert(a[1]); // UNKNOWN!
  return 0;
}
