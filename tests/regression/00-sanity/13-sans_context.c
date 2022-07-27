// PARAM: --set ana.activated "['escape', 'base','threadid','threadflag','mallocWrapper','assert']" --set ana.ctx_insens[+] base --set ana.base.privatization none
#include <assert.h>

void f(int v, int i){
  __goblint_check(v   == 2);
  __goblint_check(i*i == 9); // UNKNOWN
}

int main(){
  f(2,-3);
  f(2, 3);
  return 0;
}
