// PARAM: --set ana.base.privatization write-tid --enable ana.int.interval --set ana.path_sens[+] mutex
// Based on the example {e:write-centered} from the draft of Michael Schwarz's PhD thesis.
#include <pthread.h>
#include <goblint.h>

int g;
pthread_mutex_t a;
pthread_mutex_t b;
pthread_mutex_t c;


void* t1()
{
  pthread_mutex_lock(&a);
  pthread_mutex_lock(&b);
  g = 42;
  pthread_mutex_unlock(&a);
  g = 17;
  pthread_mutex_unlock(&b);
}

void* t2()
{
  pthread_mutex_lock(&c);
  g = 59;
  pthread_mutex_unlock(&c);
  return 0;
}

void* there_i_ruined_it()
{
  pthread_mutex_lock(&a);
  g = 45;
  pthread_mutex_unlock(&a);
  return 0;
}

int main()
{
  int x;
  pthread_t tid1;
  pthread_t tid2;
  pthread_t tid3;

  pthread_create(&tid1, 0, t1, 0);
  pthread_create(&tid2, 0, t2, 0);

  pthread_mutex_lock(&c);
  g=31;
  pthread_mutex_lock(&a);
  pthread_mutex_lock(&b);

  x = g;

  // Succeed with write & lock
  __goblint_check(x >= 17);

  // Succeeds with write-tid & lock-tid
  __goblint_check(x <= 42);

  // Succeeds with write, fails with lock
  // Needs the -tid variant to work here because of the there_i_ruined_it thread
  __goblint_check(x <= 31);

  pthread_create(&tid3, 0, there_i_ruined_it, 0);
  return 0;
}
