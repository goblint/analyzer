#include <goblint.h>

extern int scanf (char *, ...);

main () {
  int a;
  a = 0;
  a = scanf("%d",&a);
  __goblint_check(a == 0); //UNKNOWN!
}
