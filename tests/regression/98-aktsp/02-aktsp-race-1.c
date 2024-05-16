// PARAM: --set ana.activated[-] mhp --set ana.activated[-] threadid
#include <pthread.h>

int g = 0;

void *foo(void *arg) {
  g++;
  return NULL;
}

int main() {
  pthread_t t1;
  pthread_create(&t1, NULL, &foo, NULL);

  g++;
  return 0;
}
