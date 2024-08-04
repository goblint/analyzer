#include<pthread.h>
pthread_mutex_t c;
int d, e, f;

void b(void* arg);


void main(int argc, char *argv) {
  pthread_t t;

  e = pthread_create(&t, 0, b, &f);

  pthread_mutex_lock(&c);
  d=0;
  pthread_mutex_unlock(&c);
  pthread_mutex_lock(&c);
}
