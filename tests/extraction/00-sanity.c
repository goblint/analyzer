#include <pthread.h>
#include <stdio.h>

int g1, g2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;


int main(void) {
  pthread_t id1, id2;
  int i;

  printf("%d: g1 = %d, g2 = %d.\n", i, g1, g2);
  pthread_mutex_lock(&mutex1);

  return 0;
}
