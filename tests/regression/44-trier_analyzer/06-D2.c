#include <assert.h>

extern int printf (char *, ...);

main () {
  int a;
  a = 0;
  a = printf("%d\n",a);
  assert(a == 0); //UNKNOWN!
}
