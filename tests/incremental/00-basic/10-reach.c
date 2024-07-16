#include <goblint.h>
#include<pthread.h>

void foo() {
  int x = 2;
  __goblint_check(x == 2);
}

int main() {
    pthread_t id;
    pthread_create(&id, NULL, foo, NULL); // just go multithreaded

  return 0;
}
