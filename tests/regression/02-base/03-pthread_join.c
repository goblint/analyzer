#include<pthread.h>
#include<assert.h>
#include<stdio.h>

void *t_fun(void *arg) {
  return (void*) 7;
}

void *t_fun2(void *arg) {
  pthread_exit((void*) 9);
  return NULL;
}

int foo() {
  return 4;
}

void *t_fun3(void *arg) {
  foo();
  return (void*) 11;
}

int glob1 = 5;
int glob2 = 7;

int main() {
  int i = 3;
  pthread_t id;
  assert(i == 3);

  // Create the thread
  pthread_create(&id, NULL, t_fun, NULL);

  // Join the thread
  pthread_join(id, (void**) &i);
  assert(i == 7);
  printf("%d\n", i);

  // Create the thread 2
  pthread_create(&id, NULL, t_fun2, NULL);

  // Join the thread 2
  pthread_join(id, (void**) &i);
  assert(i == 9);
  printf("%d\n", i);

  // Create the thread 3
  pthread_create(&id, NULL, t_fun3, NULL);

  // Join the thread 3
  pthread_join(id, (void**) &i);
  assert(i == 11);
  printf("%d\n", i);

  // Another test
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join(id, NULL);

  return 0;
}
