// PARAM: --set ana.base.privatization lock-tid --enable ana.int.interval --set ana.path_sens[+] mutex
// Based on the example {e:lock-centered-beats-write-centered} from the draft of Michael Schwarz's PhD thesis.
#include <pthread.h>
#include <goblint.h>

int g;
pthread_mutex_t a;
pthread_mutex_t d;


void* other()
{
  pthread_mutex_lock(&d);
  pthread_mutex_lock(&a);
  g = 42;
  pthread_mutex_unlock(&a);
  g = 17;
  pthread_mutex_unlock(&d);
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
  pthread_create(&tid1, 0, other, 0);

  pthread_mutex_lock(&d);
  pthread_mutex_lock(&a);
  pthread_mutex_unlock(&d);

  x = g;
  // Succeeds with lock, fails with write
  // Needs the -tid variant to work here because of the there_i_ruined_it thread
  __goblint_check(x <= 17);

  pthread_create(&tid2, 0, there_i_ruined_it, 0);
  return 0;
}
