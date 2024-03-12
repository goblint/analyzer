// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 10
#include <pthread.h>
#include <goblint.h>

int f(int i)
{
  if (i == 0)
  {
    return 1;
  }
  if (i > 0)
  {
    return f(i - 1);
  }
}

int g(int i)
{
  if (i == 0)
  {
    return 2;
  }
  if (i > 0)
  {
    return g(i - 1);
  }
}

int h(int i)
{
  if (i == 0)
  {
    return 3;
  }
  if (i > 0)
  {
    return g(i - 1);
  }
}

int procedure(int num_iterat)
{
  int res1 = f(num_iterat);
  int res2 = g(num_iterat);
  int res3 = h(num_iterat);
  int res4 = h(num_iterat);
  return res1 + res2 + res3 + res4;
}

void *t_sens(void *arg)
{
  int result = procedure(0);
  __goblint_check(result == 9);

  result = procedure(6);
  __goblint_check(result == 7);
  return NULL;
}

void *t_sens2(void *arg)
{
  int result = procedure(1);
  __goblint_check(result == 7);

  result = procedure(8);
  __goblint_check(result == 7);
  return NULL;
}

int main()
{
  pthread_t id;
  pthread_t id2;

  // Create the thread
  pthread_create(&id, NULL, t_sens, NULL);

  // Create the thread
  pthread_create(&id2, NULL, t_sens2, NULL);
  return 0;
}
