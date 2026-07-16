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

void *t_sens(void *arg)
{
  // main -> t_sens -> procedure -> f(0)
  // main -> t_sens -> procedure -> g(0)
  // main -> t_sens -> procedure -> h(0)
  __goblint_check(procedure(0) == 8);
  return NULL;
}

void *t_sens2(void *arg)
{
  // main -> t_sens2 -> procedure -> f(3) -> ... -> f(0)
  // [main, t_sens2, procedure, f] and [main, t_sens2, procedure, {f}] (3 times)
  // main -> t_sens2 -> procedure -> g(3) -> ... -> g(0)
  // [main, t_sens2, procedure, g] and [main, t_sens2, procedure, {g}] (3 times)
  // main -> t_sens2 -> procedure -> h(3) -> g(2) -> ... -> g(0)
  // [main, t_sens2, procedure, h, g] and [main, t_sens2, procedure, h, {g}] (2 times)
  __goblint_check(procedure(3) == 10);
  return NULL;
}

int main()
{
  pthread_t id;
  pthread_t id2;

  pthread_create(&id, NULL, t_sens, NULL);
  pthread_create(&id2, NULL, t_sens2, NULL);
  return 0;
}
