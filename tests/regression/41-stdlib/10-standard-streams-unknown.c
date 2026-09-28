// CRAM
#include <stdio.h>
#include <goblint.h>

static struct { int x; char rest[BUFSIZ]; } buf;
struct { int x; char rest[BUFSIZ]; } gbuf;
extern void opaque(FILE *f, char *b); // no definition: may call setvbuf(f, b, ...)
extern void redirect(void); // no definition: may assign stdout

int main(void) {
  // A function without a definition may assign stdout, which the file only
  // declares, and hand any stream any buffer, so stdout and the streams'
  // objects become unknown: printf may write gbuf, and says it writes through
  // an unknown address.
  opaque(stdout, (char *)&gbuf);
  gbuf.x = 1;
  printf("x");
  __goblint_check(gbuf.x == 1); // UNKNOWN!
  redirect();
  __goblint_check(stdout != 0); // UNKNOWN!
  // A stream from fopen has an unknown address: setvbuf and the calls that
  // write the stream go through an unknown address and say so.
  FILE *f = fopen("/dev/null", "w");
  if (f) {
    setvbuf(f, (char *)&buf, _IOFBF, sizeof buf);
    fputc('x', f);
    stdout = f;
    printf("x");
  }
  return 0;
}
