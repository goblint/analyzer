#include <assert.h>

extern int scanf (char *, ...);

main () {
  int a, b;
  a = 0;
  b = 0;
  b = scanf("%d",&a);
  assert(a == 0); //UNKNOWN!
  assert(b == 0); //UNKNOWN!
}
