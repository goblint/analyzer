#include <assert.h>
#include<pthread.h>

void foo() {
  int x = 2;
  assert(x == 2);
}

int main() {
    pthread_t id;
    pthread_create(&id, NULL, foo, NULL); // just go multithreaded

  return 0;
}
