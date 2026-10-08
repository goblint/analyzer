// PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] closedStdStreams --enable ana.int.interval --disable warn.info --disable warn.race
// A standard stream closed by another thread must not be used.
#include <stdio.h>
#include <pthread.h>

void *t(void *arg) {
  fclose(stdout);
  return NULL;
}

int main() {
  pthread_t id;
  fflush(stdout); // NOWARN
  pthread_create(&id, NULL, t, NULL);
  fflush(stdout); // WARN
  fflush(stderr); // NOWARN
  return 0;
}
