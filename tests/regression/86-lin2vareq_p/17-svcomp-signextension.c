// SKIP PARAM: --set ana.activated[+] lin2vareq_p --set sem.int.signed_overflow assume_none
// This was problematic earlier where we were unsound with lin2vareq
// Thus worth having even if it can be answered by base alone

#include <stdio.h>
int main() {

  unsigned short int allbits = -1;
  short int signedallbits = allbits;
  int unsignedtosigned = allbits;
  unsigned int unsignedtounsigned = allbits;
  int signedtosigned = signedallbits;
  unsigned int signedtounsigned = signedallbits;

  /*
  printf ("unsignedtosigned: %d\n", unsignedtosigned);
  printf ("unsignedtounsigned: %u\n", unsignedtounsigned);
  printf ("signedtosigned: %d\n", signedtosigned);
  printf ("signedtounsigned: %u\n", signedtounsigned);
  */

  if (signedtounsigned == 4294967295) {
    __goblint_check(1); // reachable
    return (-1);
  }
__goblint_check(0); // NOWARN (unreachable)
  return (0);
}
