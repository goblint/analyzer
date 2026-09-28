#include <stdio.h>
#include <pthread.h>

// 11-standard-streams-threads.c with a write of the program's own to the
// buffer attached to stdout. The library writes buf under the lock it takes on
// stdout, which the program's write does not hold, so the write races with
// the calls in main, printf included.
static char buf[BUFSIZ];

void *t_fun(void *arg) {
  fputs("a\n", stdout); // NORACE
  printf("b\n"); // NORACE
  buf[0] = 'x'; // RACE
  return 0;
}

int main(void) {
  setvbuf(stdout, buf, _IOFBF, sizeof buf);
  pthread_t id;
  pthread_create(&id, 0, t_fun, 0);
  fputs("c\n", stdout); // RACE
  printf("d\n"); // RACE
  pthread_join(id, 0);
  return 0;
}
