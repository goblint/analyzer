#include<pthread.h>

pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

struct s {
  int a;
  int b;
} s;


void *tf1(void* arg) {
  pthread_mutex_lock(&m1);
  s.a = 1;
  pthread_mutex_unlock(&m1);
  return NULL;
}
void *tf2(void* arg) {
  pthread_mutex_lock(&m1);
  s.a = 1;
  pthread_mutex_unlock(&m1);
  return NULL;
}

int main () {
  pthread_t t1, t2;
  pthread_create(&t1, NULL, tf1, NULL);
  pthread_mutex_lock(&m2);
  s.a = 2;
  pthread_mutex_unlock(&m2);
  pthread_create(&t2, NULL, tf2, NULL);
  pthread_create(&t2, NULL, tf2, NULL);
}