// PARAM: --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>

void *t_fun(void *arg) {
  return NULL; // NOWARN (overflow)
}

void *t_fun2(void *arg) {
  return 1; // NOWARN (overflow)
}

void *t_fun3(void *arg) {
  return -1; // WARN (overflow)
}

void *t_fun4(void *arg) {
  pthread_exit(NULL); // NOWARN (overflow)
  return NULL; // NOWARN (unreachable)
}

void *t_fun5(void *arg) {
  pthread_exit(1); // NOWARN (overflow)
  return NULL; // NOWARN (unreachable)
}

void *t_fun6(void *arg) {
  pthread_exit(-1); // WARN (overflow)
  return NULL; // NOWARN (unreachable)
}

int main() {
  pthread_t id;

  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id, NULL, t_fun2, NULL);
  pthread_create(&id, NULL, t_fun3, NULL);
  pthread_create(&id, NULL, t_fun4, NULL);
  pthread_create(&id, NULL, t_fun5, NULL);
  pthread_create(&id, NULL, t_fun6, NULL);

  int r; // rand
  switch (r) {
    case 1:
      return 0; // NOWARN (overflow)
    case 2:
      return 1; // NOWARN (overflow)
    case 3:
      return -1; // NOWARN (overflow)
    case 4:
      pthread_exit(0); // NOWARN (overflow)
    case 5:
      pthread_exit(1); // NOWARN (overflow)
    case 6:
      pthread_exit(-1); // WARN (overflow)
    default:
      return 0;
  }
}
