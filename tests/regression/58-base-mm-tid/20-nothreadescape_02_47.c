// PARAM: --set ana.activated[-] escape --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid
#include <pthread.h>
#include <goblint.h>

int g = 10;

void* t(void *v) {
  int* p = (int*) v;
  *p = 4711;
}


int main(void){
  int l = 42;
  int top;

  pthread_t tid;
  pthread_create(&tid, NULL, t, (void *)&l);

  if(top) {
    pthread_join(tid, NULL);
  }

  __goblint_check(l == 42); //UNKNOWN!
  return 0;
}
