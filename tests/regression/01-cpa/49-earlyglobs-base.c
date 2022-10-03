// PARAM: --set ana.activated ["'base'","'mallocWrapper'","'assert'"]  --enable exp.earlyglobs --set ana.base.privatization none
// intentional explicit ana.activated to have only base
// same as 32-earlyglobs.c but only using the base analysis instead of all default analyses
// earlyglobs was unsound without the threadflag analysis
// https://github.com/goblint/analyzer/issues/177
#include <goblint.h>

int g = 10;

int main(void){
  int top;
  if(top) {
      g = 100;
      // This is only unknown because exp.earlyglobs is on
      __goblint_check(g == 100); //UNKNOWN!
  }

  // This assert is also unknown in the concrete!
  __goblint_check(g == 100); //UNKNOWN!
  return 0;
}
