#include <assert.h>

extern int printf (char *, ...);

main () {
  int a;
  a = 0;
  a = printf("%d\n",a);
  __goblint_check(a == 0); //UNKNOWN!
}
