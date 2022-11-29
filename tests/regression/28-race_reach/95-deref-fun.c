// PARAM: --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"
#include<pthread.h>

void foo(int (*callback)()) {
}

int bar() {
}

void *t_fun(void *arg) {

}

int main() {
  pthread_t t;
  pthread_create(&t,NULL,t_fun,NULL);
  foo(bar);
  return 0;
}
