// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>
#include <stdlib.h>

void main(void) {
  // no problem if they are all ints
  int *ipt = (int *)malloc(sizeof(int));
  int *ipt2;
  int i;
  *ipt = i;
  // *ipt: 0; i: 0
  __goblint_check(*ipt == i);
  ipt2 = (int *)ipt;
  *(ipt2 + 1) = 'a';
  // *ipt: 0; i: 0
  __goblint_check(*ipt == i);

  // long pointer is cast to char pointer -> *(cpt + 1) overwrites *lpt
  long *lpt = (long *)malloc(sizeof(long));
  char *cpt;
  long l;
  *lpt = l;
  // *lpt: 0; l: 0
  __goblint_check(*lpt == l);
  cpt = (char *)lpt;
  *(cpt + 1) = 'a';

  // *lpt: 24832; l: 0
  __goblint_check(*lpt == l); // UNKNOWN!

  l = 0;
  *lpt = l;
  // *lpt: 0; l: 0
  __goblint_check(*lpt == l);
  *((char *)lpt + 1) = 'a';
  // *lpt: 24832; l: 0
  __goblint_check(*lpt == l); // UNKNOWN!
}
