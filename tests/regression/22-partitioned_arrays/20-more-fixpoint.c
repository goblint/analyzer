// PARAM: --set ana.base.arrays.domain partitioned
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <assert.h>

pthread_mutex_t m;

int arr[2];
void g() {
  int x = 8;
  if (!(arr[0] <= arr[0])) {x = 5;}
}

void f(int i) {
  pthread_mutex_lock(&m);
  arr[i]++;
  pthread_mutex_unlock(&m);
}

void* nop(void* unused) {
  return NULL;
}

int main(void) {
  pthread_t tid1;
  pthread_create(&tid1, NULL, &nop, NULL);

  f(1);

  pthread_mutex_lock(&m);
  arr[1]--;
  g();
  pthread_mutex_unlock(&m);

  return 0;
}
