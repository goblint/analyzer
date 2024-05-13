#include<pthread.h>
#include<stdlib.h>
struct a {
  struct a *b;
};

struct a *ptr;


pthread_mutex_t m;

void doit() {
  void *newa = malloc(sizeof(struct a));

  pthread_mutex_lock(&m);
  ptr->b = newa;
  ptr = newa;
  pthread_mutex_unlock(&m);
}

void* k(void *arg) {
  doit();
  return NULL;
}

int main() {
    struct a other;
    ptr = &other;

    pthread_t t1;
    pthread_create(&t1, 0, k, 0);

    doit();
    return 0;
}
