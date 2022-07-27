// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mutexEvents','mutex','access','mallocWrapper','assert']"

#include<pthread.h>
#include<stdio.h>
#include<assert.h>

void *t_fun(void *arg) {
  int *p = (int *) arg;
  int x = 10;
  (*p) = x;
  __goblint_check(x == 10);
  x = *p;
  __goblint_check(x == 10); //UNKNOWN!
  return NULL;
}

int main(void) {
  pthread_t id;
  int i = 5;
  __goblint_check(i == 5);
  pthread_create(&id, NULL, t_fun, (void *) &i);
  i = 7;
  __goblint_check(i == 7); //UNKNOWN!
  pthread_join (id, NULL);
  return 0;
}
