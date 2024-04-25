// PARAM: --set "ana.activated[+]" loopfree_callstring --set ana.ctx_sens "['loopfree_callstring']" --enable ana.int.interval_set
#include <pthread.h>
#include <stdio.h>
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
    return 3;
  }
  if (i > 0)
  {
    return g(i - 1);
  }
  return 13;
}

int h(int i)
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

int procedure(int num_iterat)
{
  int res1 = f(num_iterat);
  int res2 = g(num_iterat);
  int res3 = h(num_iterat);
  int res4 = h(num_iterat);
  return res1 + res2 + res3 + res4;
}

void *t_ins(void *arg)
{
  // main -> t_ins -> procedure -> f(12) -> ... -> f(0)
  // [main, t_ins, procedure, f] and [main, t_ins, procedure, {f}] (12 times)
  // main -> t_ins -> procedure -> g(12) -> g(11) -> ... -> g(0)
  // main -> t_ins -> procedure -> h(12) -> g(11) -> ... -> g(0)
  __goblint_check(procedure(12) == 10); // UNKNOWN
  return NULL;
}

int main()
{
  pthread_t id;

  pthread_create(&id, NULL, t_ins, NULL);
  return 0;
}
