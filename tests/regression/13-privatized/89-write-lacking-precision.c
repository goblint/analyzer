// PARAM: --set ana.base.privatization write --disable exp.priv-distr-init
#include<pthread.h>
struct a {
  char* b;
};

struct a *c;
struct a h = {""};
struct a i = {"string"};

void* d(void* args) {
  struct a r;
  if (c->b) {
    __goblint_check(strlen(h.b) == 0); // Should also work for write!
  }
}

int main() {
  int top;

  if(top) {
    c = &h;
  } else {
    c = &i;
  }

  pthread_t t;
  pthread_create(&t, 0, d, 0);
  return 0;
}
