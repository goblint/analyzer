// CRAM PARAM: --disable warn.imprecise --disable warn.unsound --enable cil.addNestedScopeAttr
#include <stdio.h>

// Once the program assigns stdout, the variable may point to a stream other
// than the standard one, whose buffer the analysis does not track.
static void attach(void) {
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // ASSUMED libc.stream-buffer-lifetime
  fputs("a", stdout);
}

int main(void) {
  FILE *g = fopen("/dev/null", "w");
  if (!g)
    return 1;
  stdout = g;
  attach();
  fflush(stdout);
  return 0;
}
