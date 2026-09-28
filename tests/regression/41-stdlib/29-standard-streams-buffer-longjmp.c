// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr
#include <stdio.h>
#include <setjmp.h>

// A longjmp that leaves a function ends its frame without a return.
static jmp_buf env;

static void leaves(void) {
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // NOWARN
  printf("a");
  longjmp(env, 1); // WARN
}

int main(void) {
  if (!setjmp(env))
    leaves();
  fflush(stdout); // reads leaves's buffer after its lifetime
  return 0;
}
