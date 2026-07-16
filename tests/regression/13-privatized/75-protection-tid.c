// PARAM: --set ana.base.privatization protection-tid
#include <pthread.h>
#include <goblint.h>

int g;
pthread_mutex_t m;

void* spoiler() {
  int x;
  pthread_mutex_lock(&m);
  x=g;
  pthread_mutex_unlock(&m);
}

void* producer()
{
  pthread_mutex_lock(&m);
  g = 8;
  pthread_mutex_unlock(&m);
  return 0;
}

int main()
{
  pthread_t tid1;
  pthread_t tid2;

  pthread_create(&tid1, 0, spoiler, 0);

  pthread_mutex_lock(&m);
  __goblint_check(g == 0);
  pthread_mutex_unlock(&m);

  pthread_create(&tid2, 0, producer, 0);


  pthread_mutex_lock(&m);
  __goblint_check(g == 0); //UNKNOWN!
  pthread_mutex_unlock(&m);

  return 0;
}
