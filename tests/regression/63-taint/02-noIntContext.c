//PARAM --set "ana.activated[+]" taintPartialContexts -- disable ana.base.context.int
#include <goblint.h>

int g;

int identity (int a) {
  return a;
}

void addOnePtr (int *a, int *b) {
  *a = *a + 1;
}

int main() {
  int y, z;

  g = 42;
  identity (10);

  g = -42;
  identity(9);

  __goblint_check(g <= 0); //SUCCESS


  z = 3;
  y = 3;
  addOnePtr(&y, &z);

  z = -3;
  y = -3;
  addOnePtr(&y, &z);

  __goblint_check(z == -3); //SUCCESS



    

  
}