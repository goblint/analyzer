// PARAM: --set ana.activated[+] deadlock --set ana.activated[+] threadJoins
// From https://arxiv.org/abs/1607.06927
#include <pthread.h>

int x;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m3 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m4 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m5 = PTHREAD_MUTEX_INITIALIZER;

void func1() {
  x = 0; // RACE!
}

int func2(int a) {
  pthread_mutex_lock(&m5); // NODEADLOCK (thread joined)
  pthread_mutex_lock(&m4); // NODEADLOCK (thread joined)
  if (a)
    x = 3; // NORACE (thread joined)
  else
    x = 4;
  pthread_mutex_unlock(&m4);
  pthread_mutex_unlock(&m5);
  return 0;
}

void *thread() {
  pthread_mutex_lock(&m1); // NODEADLOCK
  pthread_mutex_lock(&m2); // NODEADLOCK (common m1)
  pthread_mutex_lock(&m3); // NODEADLOCK (common m1)
  x = 1; // NORACE (thread joined)
  pthread_mutex_unlock(&m3);
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);

  pthread_mutex_lock(&m4); // NODEADLOCK (thread joined)
  pthread_mutex_lock(&m5); // NODEADLOCK (thread joined)
  x = 2; // RACE!
  pthread_mutex_unlock(&m5);
  pthread_mutex_unlock(&m4);

  return NULL;
}

int main() {
  pthread_t tid;

  pthread_create(&tid, NULL, thread, NULL);

  pthread_mutex_lock(&m1); // NODEADLOCK
  pthread_mutex_lock(&m3); // NODEADLOCK (common m1)
  pthread_mutex_lock(&m2); // NODEADLOCK (common m1)
  func1();
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m3);
  pthread_mutex_unlock(&m1);

  pthread_join(tid, NULL);

  int r;
  r = func2(5);

  return 0;
}
