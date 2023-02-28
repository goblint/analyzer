//PARAM: --set ana.activated[+] threadJoins --set ana.ctx_insens[+] threadJoins
#include <pthread.h>

int g = 10;

void *t_fun(void *arg) {
  g++; //NORACE
  return NULL;
}

void benign() { 
  // Causes must-joined set to be empty here!
}

int main(void) {
  int t;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  
  benign();
  pthread_join(id, NULL);
  benign();
  g++; //NORACE


  return 0;
}
