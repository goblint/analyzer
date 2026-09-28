#define _GNU_SOURCE
#include <stdio.h>
#include <goblint.h>

// Functions whose specification reaches the stream only one level: each still
// reads or writes the buffer the stream holds.
extern int __overflow(FILE *, int);
extern int __uflow(FILE *);
static struct { int x; char rest[BUFSIZ]; } out, in;

int main(void) {
  setvbuf(stdout, (char *)&out, _IOFBF, sizeof out);
  out.x = 1;
  fputs_unlocked("x", stdout);
  __goblint_check(out.x == 1); // UNKNOWN!
  out.x = 1;
  __overflow(stdout, 'c');
  __goblint_check(out.x == 1); // UNKNOWN!
  setvbuf(stdin, (char *)&in, _IOFBF, sizeof in);
  in.x = 3;
  __uflow(stdin);
  __goblint_check(in.x == 3); // UNKNOWN!
  in.x = 3;
  ungetc('q', stdin);
  __goblint_check(in.x == 3); // UNKNOWN!
  return 0;
}
