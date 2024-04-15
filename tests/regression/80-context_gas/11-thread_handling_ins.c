// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 10
// Note: 11 function calls are possible and the analysis is still context-sensitive since the domain tracks the parameter value
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
  return 11;
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
  return 12;
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
  return 13;
}

int procedure(int num_iterat)
{
  int res1 = f(num_iterat);
  int res2 = g(num_iterat);
  int res3 = h(num_iterat);
  int res4 = h(num_iterat);
  return res1 + res2 + res3 + res4;
}

void *t_insens(void *arg)
{
  int result = procedure(9);
  __goblint_check(result == 7); // UNKNOWN

  result = procedure(60);
  __goblint_check(result == 7); // UNKNOWN
  return NULL;
}

int main()
{
  pthread_t id;

  pthread_create(&id, NULL, t_insens, NULL);
  return 0;
}
