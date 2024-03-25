// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
// This was problematic earlier where both branches were dead with lin2vareq
// Thus worth having even if it can be answered by base alone

#include <stdio.h>
int main() {

  unsigned int allbits = -255 - 25; // choose a value which is not representable in short
  int signedallbits = allbits;
  short unsignedtosigned = allbits;
  unsigned short unsignedtounsigned = allbits;

  printf("allbits: %u\n", allbits);
  printf("signedallbits: %d\n", signedallbits);
  printf("unsignedtosigned: %hd\n", unsignedtosigned);
  printf("unsignedtounsigned: %hu\n", unsignedtounsigned);

  if (unsignedtounsigned == 4294967295) {
    __goblint_check(0); // NOWARN (unreachable)
    return (-1);
  }
  if (allbits == 4294967295 && signedallbits == -1 && unsignedtosigned == -1 &&
    unsignedtounsigned == 65535) {
    __goblint_check(0); // NOWARN (unreachable)
    return (-1);
  }

  return (0);
}
