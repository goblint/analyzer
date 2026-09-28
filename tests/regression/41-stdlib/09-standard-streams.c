#include <stdio.h>
#include <goblint.h>
#include <wchar.h>

extern int __wprintf_chk(int flag, const wchar_t *format, ...);

// The buffers are structs with an int first member, so that base tracks the
// value of that member exactly. setvbuf and setbuf hand a buffer to a stream,
// and every later call that reads or writes the stream may write the buffer.
static struct { int x; char rest[BUFSIZ]; } out, err, in;

int main(void) {
  __goblint_check(stdout != 0);
  __goblint_check(stdout != stderr);

  setvbuf(stdout, (char *)&out, _IOFBF, sizeof out);
  out.x = 1;
  __goblint_check(out.x == 1);
  fputc('x', stdout);
  __goblint_check(out.x == 1); // UNKNOWN!
  out.x = 1;
  printf("x");
  __goblint_check(out.x == 1); // UNKNOWN!
  out.x = 1;
  puts("x");
  __goblint_check(out.x == 1); // UNKNOWN!
  out.x = 1;
  FILE *alias = stdout;
  fputs("x", alias);
  __goblint_check(out.x == 1); // UNKNOWN!
  out.x = 1;
  fflush(stdout);
  __goblint_check(out.x == 1); // UNKNOWN!

  // stderr's buffer is written only by calls on stderr.
  setvbuf(stderr, (char *)&err, _IOFBF, sizeof err);
  err.x = 2;
  printf("y");
  __goblint_check(err.x == 2);
  perror("y");
  __goblint_check(err.x == 2); // UNKNOWN!

  setbuf(stdin, (char *)&in);
  in.x = 3;
  getchar();
  __goblint_check(in.x == 3); // UNKNOWN!

  // putwchar writes stdout's buffer too, here a local one.
  struct { int x; char rest[BUFSIZ]; } local;
  setvbuf(stdout, (char *)&local, _IOFBF, sizeof local);
  local.x = 1;
  putwchar(L'x');
  __goblint_check(local.x == 1); // UNKNOWN!

  // So do glibc's fortified entry points.
  local.x = 1;
  __wprintf_chk(1, L"x");
  __goblint_check(local.x == 1); // UNKNOWN!

  // Given no buffer, stdout uses one of the library's own, so printf no
  // longer writes out.
  setvbuf(stdout, 0, _IONBF, 0);
  out.x = 1;
  printf("v");
  __goblint_check(out.x == 1);

  // Once stdout is assigned stderr, printf writes stderr's buffer.
  FILE *saved = stdout;
  stdout = stderr;
  err.x = 2;
  printf("z");
  __goblint_check(err.x == 2); // UNKNOWN!
  stdout = saved;
  err.x = 2;
  printf("w");
  __goblint_check(err.x == 2);
  return 0;
}
