// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr
#include <stdio.h>
#include <pthread.h>

// pthread_exit in main ends main's frame while the process continues; when
// the last thread exits, the streams are flushed as by exit, from main's
// ended buffer.

static void finish(void) {
  pthread_exit(NULL); // WARN
}

int main(int argc, char **argv) {
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // NOWARN
  printf("a");
  if (argc > 1)
    finish();
  pthread_exit(NULL); // WARN
}
