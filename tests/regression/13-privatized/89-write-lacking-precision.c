// PARAM: --set ana.base.privatization write
#include<pthread.h>
#include <string.h>
#include <goblint.h>

struct a {
  char* b;
};

struct a *c;
struct a h = {""};
struct a i = {"string"};

void* d(void* args) {
  if (c->b) {
    // Handled by privatization as a write
    // Without fix (#1468) causes both h.b and i.b to become unknown string
    __goblint_check(strlen(h.b) == 0); // Check h.b is still known
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
