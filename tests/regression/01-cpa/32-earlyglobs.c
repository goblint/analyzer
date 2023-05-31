// PARAM: --enable exp.earlyglobs
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
