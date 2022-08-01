// PARAM: --enable ana.int.interval
#include <assert.h>
int main(){
  int b = 0;
  if ((unsigned long )b == (unsigned long )((void *)0)) {
    b = b + 1;
  }
  __goblint_check(b == 1); //
  return 0;
}
