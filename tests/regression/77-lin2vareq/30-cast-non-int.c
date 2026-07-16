// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
#include <goblint.h>
//#include <stdio.h>
int main(void) {
  float b = 2.5;
  float a = 1.5;
  int c = (int) a; 
  int d = (int) b;
  //printf("c: %d\nd: %d\n", c, d);
  __goblint_check(d -c  -1 == 0); // UNKNOWN
}
