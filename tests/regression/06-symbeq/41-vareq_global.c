//PARAM: --set ana.activated[+] var_eq
#include <goblint.h>
#include <pthread.h>
#include <unistd.h>
int g;

void *t_fun1(void *arg) {
  int *zptr = (int*) arg;
  int* gptr = &g;

  *zptr = 42;
  *gptr = 42;
  sleep(10);
  // here the other thread (id2) could potentially change *zptr, as it is linked to the same int z
  __goblint_check(*zptr == 42);  //UNKNOWN!
  __goblint_check(*gptr == 42);  //UNKNOWN!
  return NULL;
}

int main() {
  pthread_t id1;
  int z = 8;

  pthread_create(&id1, NULL, t_fun1, &z);
  z = 8;
  g = 8;
  pthread_join (id1, NULL);
}