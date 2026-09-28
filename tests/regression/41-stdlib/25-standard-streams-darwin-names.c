// CRAM
#include <goblint.h>

// Darwin's <stdio.h> declares the standard streams as __stdinp, __stdoutp
// and __stderrp and defines stdin, stdout and stderr as macros for them. This
// program declares them the same way, so that it is analyzed as on Darwin.
typedef struct __sFILE { int _flags; } FILE;
extern FILE *__stdinp;
extern FILE *__stdoutp;
extern FILE *__stderrp;
#define stdin __stdinp
#define stdout __stdoutp
#define stderr __stderrp
#define _IOFBF 0
extern int setvbuf(FILE *stream, char *buf, int mode, unsigned long size);
extern int fputc(int c, FILE *stream);
extern int printf(const char *format, ...);

static struct { int x; char rest[1024]; } out;

int main(void) {
  __goblint_check(stdout != 0);
  __goblint_check(stdout != stderr);
  setvbuf(stdout, (char *)&out, _IOFBF, sizeof out);
  out.x = 1;
  fputc('x', stdout);
  __goblint_check(out.x == 1); // UNKNOWN!
  out.x = 1;
  printf("x");
  __goblint_check(out.x == 1); // UNKNOWN!
  return 0;
}
