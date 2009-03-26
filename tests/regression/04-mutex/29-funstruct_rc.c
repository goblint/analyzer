#include<pthread.h>

int x;

pthread_mutex_t A_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B_mutex = PTHREAD_MUTEX_INITIALIZER;

struct ops {
  int (*f)(int);
  void (*g)();
};

extern void register_dev(struct ops *);

int inc(int x) {
  return x+1;
}
void glob() {
  pthread_mutex_lock(&B_mutex);
  x++; // RACE
  pthread_mutex_unlock(&B_mutex);
  return;
}

void *t_fun(void *arg) {
  pthread_mutex_lock(&A_mutex);
  x++; // RACE
  pthread_mutex_unlock(&A_mutex);
  return NULL;
}

struct ops dev_ops = {
  .f = inc,
  .g = glob,
};

int main () {
  pthread_t t1;
  register_dev(&dev_ops);

  pthread_create(&t1, NULL, t_fun, NULL);
  pthread_mutex_lock(&A_mutex);
  x++; // RACE
  pthread_mutex_unlock(&A_mutex);
  return 0;
}
