// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron']" --set ana.base.privatization none --set ana.apron.privatization dummy
// Example from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/implicitunsignedconversion-1.c

#include <assert.h>

int main() {
  unsigned int plus_one = 1;
  int minus_one = -1;

  if(plus_one < minus_one) {
    assert(1); // reachable
  }

  return (0);
}
