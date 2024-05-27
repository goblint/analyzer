// PARAM: --set ana.base.privatization write
#include<pthread.h>
#include<stdlib.h>
#include<string.h>
union g {
  int h;
  int i;
};
struct j {
  union g data;
};


void* af(void* arg) {
  int top;
  struct j* jptr = (struct j*) malloc(sizeof(struct j));
  memset(jptr, 0, sizeof(struct j));

  while (top) {
    if (jptr->data.i);

    __goblint_check(jptr->data.i == 0); // Should alo work for write!
  }
}

void main() {
  pthread_t ah;
  pthread_create(&ah, 0, af, 0);
}
