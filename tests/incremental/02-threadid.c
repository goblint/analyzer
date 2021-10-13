#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t_fun(void *arg) {
  myglobal=40; // NORACE
  return NULL;
}

int f(){
  return 1;
}

pthread_t create_thread(){
  int x = 0;
  pthread_t id;
  x = f();
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join (id, NULL);
  x = 0;
  return id;
}

int main(void) {
  pthread_t id = create_thread();
  return 0;
}
