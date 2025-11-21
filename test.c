//PARAM: --disable ana.int.def_exc --enable ana.int.interval
#include <goblint.h>


int main() {
  int x;

  for(x=0; x < 50; x++){
  }
  // HERE
  __goblint_check(x == 50); // NOWARN (unreachable)
  return x;
}

