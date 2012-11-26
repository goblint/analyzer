// PARAM: --set dbg.debug true
#include <assert.h>

extern int q;

int main(){
  int i = q ? 1 : 2 ;
  assert(0); // FAIL
  return 0;
}
