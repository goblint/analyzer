#include<stdio.h>
#include<assert.h>
#include<pthread.h>

void *http_get(void *arg ) {
  int x = (int) arg;
  __goblint_check(x == 43);
  return  NULL;
}

int main() {
  pthread_t tid;
  pthread_create(&tid,  NULL, & http_get, (void *) 43);
  return 0;
}

