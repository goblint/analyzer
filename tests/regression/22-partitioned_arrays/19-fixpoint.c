// PARAM: --set ana.base.arrays.domain partitioned
#include <pthread.h>

int stored_elements[20];

void *t1(void *arg)
{
  int value, i;

  stored_elements[0]=value;

  for(i=0; i<19; i++)
  {
    stored_elements[i+1]=value;
  }

  return NULL;
}

int main(void)
{
  pthread_t id1;
  pthread_create(&id1, NULL, t1, NULL);

  return 0;
}
