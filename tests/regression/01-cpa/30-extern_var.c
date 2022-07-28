// PARAM: --set dbg.debug true
#include <assert.h>

extern int q;

int main(){
  int i = q ? 1 : 2 ;
  __goblint_check(0); // FAIL
  return 0;
}
