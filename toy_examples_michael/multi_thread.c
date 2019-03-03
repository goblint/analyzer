#include<pthread.h>


void *init_a(void *a_void_ptr)
{
  int *a = (int *)a_void_ptr;
  *a = 0;

  int x = *a;

  /* the function must return something - NULL will do */
  return NULL;
}

int main(void) {
  static int top;     // If I do not delcare them static, it misbehaves, but that also is the case for ints
  static int a[10];

  pthread_t init_array_thread;
  pthread_t init_scalar_thread;

  pthread_create(&init_array_thread, NULL, init_a, &a); // why is there an array get here?
  pthread_create(&init_scalar_thread, NULL, init_a, &top);

  for(int i=0; i < 42; i++) {
    a[i] = 2; /// trying to update an index, but array is unknown
  }

  top = 7;

  pthread_join(init_array_thread, NULL);
  pthread_join(init_scalar_thread, NULL);

  assert(top == 7);

  int blubb = a[0];
}


