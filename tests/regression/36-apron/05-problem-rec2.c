// SKIP PARAM: --set ana.activated[+] apron
// Example from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/recursive-simple/afterrec_2calls-1.c
#include <goblint.h>

void f(int);
void f2(int);

void f(int n) {
  if (n<3) return;
  n--;
  f2(n);
  __goblint_check(1);
}

void f2(int n) {
  if (n<3) return;
  n--;
  f(n);
  __goblint_check(1);
}

int main(void) {
  f(4);
}
