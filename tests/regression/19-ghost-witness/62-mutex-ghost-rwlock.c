#include <pthread.h>
#include <stdio.h>

static int data1;
static int data2;
static pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;


void *t_fun(void *arg) {
  pthread_rwlock_rdlock(&rwlock); // ghost_1 = 1
  data1++;
  pthread_rwlock_unlock(&rwlock); // ghost_1 = 0
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_rwlock_rwlock(&rwlock); // ghost_1 = 1
  data2++;
  pthread_rwlock_unlock(&rwlock); // ghost_1 = 0

  pthread_join (id, NULL);
  return 0;
}

