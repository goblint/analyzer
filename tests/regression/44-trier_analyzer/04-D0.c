#include <assert.h>

extern int scanf (char *, ...);

main () {
  int a, b;
  a = 0;
  b = 0;
  b = scanf("%d",&a);
  __goblint_check(a == 0); //UNKNOWN!
  __goblint_check(b == 0); //UNKNOWN!
}
