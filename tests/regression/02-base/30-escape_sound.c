// PARAM: --set ana.activated "['base','escape']"

#include<pthread.h>
#include<stdio.h>
#include<assert.h>

void *t_fun(void *arg) {
  int *p = (int *) arg;
  int x = 10;
  (*p) = x; 
  assert(x == 10);
  x = *p;
  assert(x == 10); //UNKNOWN!
  return NULL;
}

int main(void) {
  pthread_t id;
  int i = 5;
  assert(i == 5);
  pthread_create(&id, NULL, t_fun, (void *) &i);
  i = 7;
  assert(i == 7); //UNKNOWN!
  pthread_join (id, NULL);
  return 0;
}
