// SKIP PARAM: --set ana.activated[+] apron
// Example from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/implicitunsignedconversion-1.c

#include <assert.h>

int main() {
  unsigned int plus_one = 1;
  int minus_one = -1;

  if(plus_one < minus_one) {
    __goblint_check(1); // reachable
  }

  return (0);
}
