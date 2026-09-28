// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr --set ana.activated[+] threadJoins
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

// main gives stdout a buffer of its own for the whole run. A thread other than
// the main one has no frame of main on its stack, since the program neither
// calls main nor takes its address, so neither a worker's pthread_exit nor
// cancelling a worker ends main's buffer.

static void *worker(void *arg) {
  puts("worker");
  pthread_exit(NULL); // NOWARN
}

static void *spinner(void *arg) {
  for (;;)
    pthread_testcancel();
  return NULL;
}

int main(void) {
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // ASSUMED libc.stream-buffer-lifetime (the program calls pthread_cancel)
  pthread_t t, s;
  pthread_create(&t, NULL, worker, NULL);
  pthread_create(&s, NULL, spinner, NULL);
  pthread_join(t, NULL);
  pthread_cancel(s); // NOWARN
  pthread_join(s, NULL);
  pthread_cancel(pthread_self()); // WARN (the main thread's own frames)
  puts("main");
  exit(0); // NOWARN (exit flushes before main's frame ends)
}
