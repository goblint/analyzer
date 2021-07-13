// SKIP PARAM: --sets solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','assert']"
// Example from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/signextension-1.c
// TODO: make this work for octApron by handling variable casts
#include "stdio.h"
#include <assert.h>
int main() {
  // This test is a part taken from of 24 13
  unsigned short int allbits = -1 ;
  short int signedallbits = allbits;
  int signedtosigned = signedallbits;

  assert(allbits == 65535);
  assert(signedallbits == -1);
  assert(signedtosigned == -1);


  if (signedtosigned == -1) {
    assert(1);
  }

  return (0);
}
