extern void __VERIFIER_error() __attribute__ ((__noreturn__));

#include <pthread.h>

void* t1(void* arg)
{
  pthread_exit(NULL);
  // return 0;
  __VERIFIER_error();
}

int main(int argc, char **argv)
{
  pthread_t id1;
  pthread_create(&id1, NULL, t1, NULL);
  return 0;
}
