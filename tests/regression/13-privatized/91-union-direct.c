// PARAM: --set ana.base.privatization write
#include<pthread.h>
#include<stdlib.h>
#include<string.h>
union g {
  int h;
  int i;
};

void* af(void* arg) {
  // Go MT
}

void main() {
  pthread_t ah;
  pthread_create(&ah, 0, af, 0);

  int top;
  union g* jptr = (union g*) malloc(sizeof(union g));
  memset(jptr, 0, sizeof(union g));

  while (top) {
    if (jptr->i);
    __goblint_check(jptr->i == 0); // Should alo work for write!
  }
}
