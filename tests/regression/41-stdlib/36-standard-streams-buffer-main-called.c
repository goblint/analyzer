// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr --set ana.activated[+] threadJoins
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

// 35-standard-streams-buffer-main-local-threads.c where a worker may call
// main: a frame of main may then be on the worker's stack, so its
// pthread_exit and cancelling it may end main's buffer.

int main(int argc, char **argv);

static void *worker(void *arg) {
  if (arg)
    main(0, NULL);
  pthread_exit(NULL); // WARN
}

int main(int argc, char **argv) {
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // ASSUMED libc.stream-buffer-lifetime (the program calls pthread_cancel)
  pthread_t t, s;
  pthread_create(&t, NULL, worker, NULL);
  pthread_create(&s, NULL, worker, NULL);
  pthread_join(t, NULL);
  pthread_cancel(s); // WARN
  pthread_join(s, NULL);
  exit(0); // NOWARN
}
