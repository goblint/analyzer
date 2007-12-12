#include<stdio.h>
#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  return (void*) 7;
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
  pthread_join(id, (void*) &i);
  // Some day maybe assert(i == 7) 
  assert_unknown(i);
  printf("%d\n", i);

  // Another test
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join(id, NULL);

  return 0;
}
