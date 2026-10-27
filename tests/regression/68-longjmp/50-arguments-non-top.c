// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;

int fun() {
  int r;
  __goblint_assume(0 <= r);
  __goblint_assume(r <= 10);
  longjmp(env_buffer, r); //WARN
}


int main () {
  int val;
  if (val = setjmp( env_buffer )) {
    __goblint_check(val == 1); // UNKNOWN!
    __goblint_check(val != 1); // UNKNOWN!
    __goblint_check(1 <= val);
    __goblint_check(val <= 10);
    return 8;
  }

  fun();

  __goblint_check(0); // NOWARN
  return(0);
}
