#include <goblint.h>

extern int q;

int main(){
  int i = q ? 1 : 2 ;
  __goblint_check(0); // FAIL
  return 0;
}
