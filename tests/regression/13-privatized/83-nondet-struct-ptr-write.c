// PARAM: --set ana.base.privatization write --enable ana.int.enums
#include<pthread.h>
#include<stdlib.h>
struct a {
  int b;
};

struct a *ptr;
struct a *straightandnarrow;

pthread_mutex_t m;

void doit() {
  void *newa = malloc(sizeof(struct a));

  pthread_mutex_lock(&m);
  ptr->b = 5;

  int fear = straightandnarrow->b;
  __goblint_check(fear == 5); //UNKNOWN!

  ptr = newa;
  pthread_mutex_unlock(&m);

  pthread_mutex_lock(&m);
  int hope = straightandnarrow->b;
  __goblint_check(hope == 5); //UNKNOWN!
  pthread_mutex_unlock(&m);

}

void* k(void *arg) {
  doit();
  return NULL;
}

int main() {
    int top;
    struct a other = { 0 };
    struct a other2 = { 42 };
    if(top) {
      ptr = &other;
    } else {
      ptr = &other2;
    }

    straightandnarrow = &other;

    pthread_t t1;
    pthread_create(&t1, 0, k, 0);

    doit();
    return 0;
}
