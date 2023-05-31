// PARAM: --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
#include <stdlib.h>
#include <pthread.h>

extern int __VERIFIER_nondet_int();
typedef struct { int myint; pthread_mutex_t mymutex; } mystruct;


void *foo(void *arg) {
  mystruct *p1 = (mystruct *) arg;
  mystruct *p2 = (mystruct *) arg;

  pthread_mutex_lock(&p1->mymutex);
  if (__VERIFIER_nondet_int()) p2 = NULL;
  pthread_mutex_unlock(&p2->mymutex);
  p1->myint++; // TODO RACE

  return NULL;
}

int main() {
  mystruct *s = (mystruct *) malloc(sizeof(*s));
  pthread_mutex_init(&s->mymutex, NULL);

  pthread_t id1, id2;
  pthread_create(&id1, NULL, foo, s);
  pthread_create(&id2, NULL, foo, s);
  pthread_join(id1, NULL);
  pthread_join(id2, NULL);
}
