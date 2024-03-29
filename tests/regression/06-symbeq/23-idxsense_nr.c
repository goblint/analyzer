// PARAM: --set ana.activated[+] symb_locks --set ana.activated[+] var_eq
// Copy of 05/10 with symb_locks enabled
#include <pthread.h>

int data[10];
pthread_mutex_t m[10];

void *t_fun(void *arg) {
  pthread_mutex_lock(&m[4]);
  data[4]++; // NORACE
  pthread_mutex_unlock(&m[4]);
  return NULL;
}

int main() {
  for (int i = 0; i < 10; i++)
    pthread_mutex_init(&m[i], NULL);

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m[3]);
  data[3]++; // NORACE
  pthread_mutex_unlock(&m[3]);
  pthread_mutex_lock(&m[4]);
  data[4]++; // NORACE
  pthread_mutex_unlock(&m[4]);
  return 0;
}

