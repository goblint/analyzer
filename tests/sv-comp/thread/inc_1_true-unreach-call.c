extern void __VERIFIER_error() __attribute__ ((__noreturn__));

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

#include <pthread.h>

/* pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void __VERIFIER_atomic_begin() {
  pthread_mutex_lock(&mutex);
}
void __VERIFIER_atomic_end() {
  pthread_mutex_unlock(&mutex);
} */

int i=1;

void *
t1(void* arg)
{
  __VERIFIER_atomic_begin();
  i+=1;
  __VERIFIER_atomic_end();
  pthread_exit(NULL);
}

void *
t2(void* arg)
{
  __VERIFIER_atomic_begin();
  i+=1;
  __VERIFIER_atomic_end();
  pthread_exit(NULL);
}

int
main(int argc, char **argv)
{
  pthread_t id1, id2;

  pthread_create(&id1, NULL, t1, NULL);
  pthread_create(&id2, NULL, t2, NULL);

  __VERIFIER_atomic_begin();
  int condI = i > 2;
  __VERIFIER_atomic_end();

  if (condI) {
    ERROR: __VERIFIER_error();
  }

  return 0;
}
