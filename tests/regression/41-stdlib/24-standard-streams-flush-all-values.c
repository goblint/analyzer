#include <stdio.h>
#include <goblint.h>

// fflush(NULL), and fflush of a stream that may be null, may write the buffer
// of every standard stream.
static struct { int x; char rest[BUFSIZ]; } out, err;

int main(int argc, char **argv) {
  setvbuf(stdout, (char *)&out, _IOFBF, sizeof out);
  setvbuf(stderr, (char *)&err, _IOFBF, sizeof err);
  out.x = 1;
  fflush(NULL);
  __goblint_check(out.x == 1); // UNKNOWN!
  FILE *maybe = argc > 1 ? stdin : NULL;
  err.x = 1;
  fflush(maybe);
  __goblint_check(err.x == 1); // UNKNOWN!
  return 0;
}
