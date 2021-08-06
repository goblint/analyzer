// SKIP PARAM: --sets solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','octApron']"
// Example from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/signextension-1.c

#include <assert.h>

int main() {

  unsigned short int allbits = -1;
  short int signedallbits = allbits;
  int unsignedtosigned = allbits;
  unsigned int unsignedtounsigned = allbits;
  int signedtosigned = signedallbits;
  unsigned int signedtounsigned = signedallbits;

  if (unsignedtosigned == 65535 && unsignedtounsigned == 65535
      && signedtosigned == -1 && signedtounsigned == 4294967295) {
    assert(1); // reachable
  }

  return (0);
}
