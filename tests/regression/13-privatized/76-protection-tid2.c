// PARAM: --set ana.base.privatization protection-tid --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>

int g;
pthread_mutex_t m;

void* spoiler() {
  pthread_mutex_lock(&m);
  g = 4;
  pthread_mutex_unlock(&m);
}

void* otherproducer() {
  pthread_mutex_lock(&m);
  g = 9;
  pthread_mutex_unlock(&m);
}

void* producer()
{
  pthread_t tid1;
  pthread_mutex_lock(&m);
  g = 8;
  pthread_mutex_unlock(&m);

  pthread_mutex_lock(&m);
  __goblint_check(g < 9);
  pthread_mutex_unlock(&m);

  pthread_create(&tid1, 0, otherproducer, 0);

  return 0;
}

int main()
{
  pthread_t tid1;
  pthread_t tid2;

  pthread_create(&tid1, 0, spoiler, 0);

  pthread_mutex_lock(&m);
  __goblint_check(g < 5);
  pthread_mutex_unlock(&m);

  pthread_create(&tid2, 0, producer, 0);


  pthread_mutex_lock(&m);
  __goblint_check(g < 5); //UNKNOWN!
  __goblint_check(g < 10);
  pthread_mutex_unlock(&m);

  return 0;
}
