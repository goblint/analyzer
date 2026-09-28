// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr
#include <stdio.h>

// A variable-length array's lifetime ends where a goto jumps back before its
// declaration (C11 6.2.4p7), without a return and without leaving the block,
// so a VLA handed to a stream is assumed to stay live even in a function's
// outermost block. The fixed-size array beside it ends only at the return.
int main(int argc, char **argv) {
  int round = 0;
  char fixed[BUFSIZ];
  setvbuf(stderr, fixed, _IOFBF, sizeof fixed); // NOWARN
again:
  if (round == 1) {
    fflush(stdout);
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
    return 0;
  }
  char buffer[BUFSIZ + argc];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // ASSUMED libc.stream-buffer-lifetime
  printf("a");
  round++;
  goto again;
}
