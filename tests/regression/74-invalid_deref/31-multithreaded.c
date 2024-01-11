//PARAM: --set ana.path_sens[+] threadflag  --set ana.activated[+] memOutOfBounds --set ana.base.privatization mutex-meet-tid
#include <pthread.h>

int data;
int *p = &data, *q;
pthread_mutex_t mutex;
void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  *p = 8;
  pthread_mutex_unlock(&mutex);
  return ((void *)0);
}
int main() {
  pthread_t id;
  pthread_create(&id, ((void *)0), t_fun, ((void *)0));
  q = p;
  pthread_mutex_lock(&mutex);
  *q = 8; //NOWARN
  pthread_mutex_unlock(&mutex);
  return 0;
}
