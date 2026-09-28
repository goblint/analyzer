// CRAM
#include <pthread.h>

// Without <stdio.h> the file declares no stdout, but printf still writes the
// standard stream's object, which holds null.
extern int printf(const char *, ...);

void *t_fun(void *arg) {
  printf("t");
  return 0;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, 0, t_fun, 0);
  printf("m");
  pthread_join(id, 0);
  return 0;
}
