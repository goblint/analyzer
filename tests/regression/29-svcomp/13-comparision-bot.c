// PARAM: --enable ana.int.interval --enable ana.int.def_exc
// NOCRASH
#include <stdio.h>
int main(){
  int a = 0;
  unsigned int b = (unsigned int) a - 256U;
  if ((unsigned int) a - 256U <= 511U) {
      a += 4;
  }
  printf("%u\n", (unsigned int) a - 256U);
}
