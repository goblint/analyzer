// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set ana.context.ctx_gas_value 10
#include <pthread.h>
#include <goblint.h>

int f(int i)
{
  int res = 0;
  if (i == 0)
  {
    res = 1;
  }
  if (i > 0)
  {
    res = f(--i);
  }
  return res;
}

int g(int i)
{
  int res = 0;
  if (i == 0)
  {
    res = 3;
  }
  if (i > 0)
  {
    res = g(--i);
  }
  return res;
}

int h(int i)
{
  int res = 0;
  if (i == 0)
  {
    res = 2;
  }
  if (i > 0)
  {
    res = g(--i);
  }
  return res;
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
  __goblint_check(result == 8);

  result = procedure(5);
  __goblint_check(result == 10);
  return NULL;
}

void *t_insens(void *arg)
{
  int result = procedure(6);
  __goblint_check(result == 10); // UNKNOWN

  result = procedure(60);
  __goblint_check(result == 10); // UNKNOWN
  return NULL;
}

int main()
{
  pthread_t id;

  // Creat the thread
  pthread_create(&id, NULL, t_sens, NULL);

  // Creat the thread
  pthread_create(&id, NULL, t_insens, NULL);
  return 0;
}
