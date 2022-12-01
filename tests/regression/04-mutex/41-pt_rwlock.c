#include <pthread.h>
#include <stdio.h>

static int data1;
static int data2;
static pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;


void *t_fun(void *arg) {
  pthread_rwlock_wrlock(&rwlock);
  data1++;            // NORACE
  printf("%d",data2); // NORACE
  pthread_rwlock_unlock(&rwlock);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_rwlock_rdlock(&rwlock);
  printf("%d",data1); // NORACE
  data2++;            // NORACE
  pthread_rwlock_unlock(&rwlock);

  pthread_join (id, NULL);
  return 0;
}

