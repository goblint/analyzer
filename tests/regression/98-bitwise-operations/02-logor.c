// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned int x;

  unsigned int y = x | 2;

  __goblint_check(y >= 2); // TODO 
  
  unsigned int z = y | 3;

  __goblint_check(z >= 3); // TODO 

  return 0;
}