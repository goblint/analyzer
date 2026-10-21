// PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 15-base-unassume-query.yml
#include <pthread.h>
#include <assert.h>

void *t_fun(void *arg) {
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL); // enter multithreaded

  int i, j;
  i = 2;
  j = 3;
  assert(i == 2);
  assert(j == 3);

  // screw with ThreadEscape.is_escaped via vaddrof
  int *p, *q;
  p = &i;
  q = &j;
  return 0;
}