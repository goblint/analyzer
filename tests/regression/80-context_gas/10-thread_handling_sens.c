// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 10
// Note: 11 function calls are analyzed context-sensitively
// -> tracked parameter in domain enables one additional context-sensitively analyzed call
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

void *t_sens(void *arg)
{
  // main -> t_sens2 -> procedure -> f(8) -> ... -> f(0)
  // main -> t_sens2 -> procedure -> g(8) -> ... -> g(0)
  // main -> t_sens2 -> procedure -> h(8) -> g(7) -> ... -> g(0)
  __goblint_check(procedure(8) == 7);
  return NULL;
}

int main()
{
  pthread_t id;

  pthread_create(&id, NULL, t_sens, NULL);
  return 0;
}
