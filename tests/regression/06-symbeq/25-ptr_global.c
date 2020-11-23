// PARAM: --set ana.activated[+] "'var_eq'"
// Own version of 29/20
// Somehow has NullPtr and UnknownPtr for global instead of Addr like the original. How???
#include <pthread.h>
#include <stdio.h>
#include <assert.h>

int *global;

void *t_fun(void *arg) {
  int *p = global;
  *p = 2;
  assert(*p == 2); // UNKNOWN!
  return NULL;
}

int main(void) {
  global = malloc(sizeof(int));
  int *p = global;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  *p = 1;
  assert(*p == 1); // UNKNOWN!

  pthread_join (id, NULL);
  return 0;
}
