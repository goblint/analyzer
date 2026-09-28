// PARAM: --disable warn.imprecise
#define _DEFAULT_SOURCE
#include <stdio.h>
#include <stdlib.h>

// Without cil.addNestedScopeAttr a local may be declared in a nested block,
// whose end is not a return, so it is assumed to stay live while the stream
// holds it, as is thread-local storage. Static and heap buffers need no
// assumption: heap memory ends only at free, which is checked.
static char global_buffer[BUFSIZ];
static __thread char thread_buffer[BUFSIZ]; // ends when the thread that uses it exits

void local(void) {
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // ASSUMED libc.stream-buffer-lifetime
  setvbuf(stdout, 0, _IONBF, 0); // NOWARN
  setbuffer(stdout, buffer, sizeof buffer); // ASSUMED libc.stream-buffer-lifetime
  setvbuf(stdout, thread_buffer, _IOFBF, sizeof thread_buffer); // ASSUMED libc.stream-buffer-lifetime
  setvbuf(stdout, 0, _IONBF, 0); // NOWARN
}

int main(void) {
  static char static_buffer[BUFSIZ];
  setvbuf(stdout, global_buffer, _IOFBF, sizeof global_buffer); // NOWARN
  setbuf(stderr, static_buffer); // NOWARN
  local();
  char *heap = malloc(BUFSIZ);
  setvbuf(stdout, heap, _IOFBF, BUFSIZ); // NOWARN
  printf("x\n");
  return 0;
}
