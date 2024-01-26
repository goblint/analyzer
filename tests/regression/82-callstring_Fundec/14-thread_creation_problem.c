// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
#include <pthread.h>
#include <stdio.h>
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
  __goblint_check(result == 8); // TODO

  result = procedure(3);
  __goblint_check(result == 10); // TODO
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
  pthread_t id2;

  // Creat the thread
  pthread_create(&id, NULL, t_sens, NULL);
  // pthread_join(id, NULL);

  // Creat the thread
  pthread_create(&id2, NULL, t_insens, NULL);
  return 0;
}
