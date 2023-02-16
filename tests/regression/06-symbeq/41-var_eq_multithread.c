// SKIP PARAM: --set ana.activated[+] var_eq
#include <goblint.h>
//#include <assert.h>
#include <pthread.h>
#include <unistd.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void f(int *zptr){
  pthread_mutex_unlock(&mutex);
}

void *t_fun1(void *arg) {
  int x;
  int *zptr;
  zptr = arg;

  *zptr = x;
  sleep(10);
  // here the other thread (id2) could potentially change *zptr, as it is linked to the same int z
  //assert(*zptr == x); //UNKNOWN!
  __goblint_check(*zptr == x);  //UNKNOWN!
  return NULL;
}

void *t_fun2(void *arg) {
  *(int*)arg = 27;
}

int main() {
  pthread_t id1, id2;
  int z;

  pthread_create(&id1, NULL, t_fun1, &z);
  pthread_create(&id2, NULL, t_fun2, &z);
  pthread_join (id1, NULL);
  pthread_join (id2, NULL);
}
