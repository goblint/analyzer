// PARAM: --set ana.context.callString_length 10 --set "ana.activated[+]" call_site --set ana.ctx_sens "['call_site']" --enable ana.int.interval_set
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
  // main -> t_sens2 -> procedure -> f(8) -> ... -> f(0)
  // main -> t_sens2 -> procedure -> g(8) -> g(7) -> ... -> g(0)
  // main -> t_sens2 -> procedure -> h(8) -> g(7) -> ... -> g(0)
  __goblint_check(procedure(8) == 10);
  return NULL;
}

void *t_sens3(void *arg)
{
  // main -> t_sens3 -> procedure -> f(12) -> ... -> f(0)
  // main -> t_sens3 -> procedure -> g(12) -> g(11) -> ... -> g(0)
  // main -> t_sens3 -> procedure -> h(12) -> g(11) -> ... -> g(0)
  __goblint_check(procedure(12) == 10);
  return NULL;
}

int main()
{
  pthread_t id;
  pthread_t id2;
  pthread_t id3;

  pthread_create(&id, NULL, t_sens, NULL);
  pthread_create(&id2, NULL, t_sens2, NULL);
  pthread_create(&id3, NULL, t_sens3, NULL);
  return 0;
}
