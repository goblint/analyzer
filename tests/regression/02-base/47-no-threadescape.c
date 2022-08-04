// PARAM: --set ana.activated[-] escape
#include <pthread.h>
#include <assert.h>

int g = 10;

void* t(void *v) {
  int* p = (int*) v;
  *p = 4711;
}


int main(void){
  int l = 42;

  pthread_t tid;
  pthread_create(&tid, NULL, t, (void *)&l);
  pthread_join(tid, NULL);

  __goblint_check(l==42); //UNKNOWN!
  return 0;
}
