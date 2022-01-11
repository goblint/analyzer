// SKIP
// TODO: unskip after merging interactive, works there due to incremental warnings?
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
  // Shifting the lines below down in the incremental run should not require a re-evalatuion,
  // but the threadid in the HTML-Output should contain the new location.
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join (id, NULL);
  x = 0;
  return id;
}

int main(void) {
  pthread_t id = create_thread();
  return 0;
}
