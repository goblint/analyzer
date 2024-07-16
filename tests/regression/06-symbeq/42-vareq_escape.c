//PARAM: --set ana.activated[+] var_eq
#include <goblint.h>
#include <pthread.h>
#include <unistd.h>
int g;

void *t_fun1(void *arg) {
  int** ptrptr = (int**) arg;
  int* iptr = *ptrptr;

  *iptr = 12;
}

int main() {
  pthread_t id1;
  int z = 8;
  int i;
  int* zptr = &z;

  int j = i;
  pthread_create(&id1, NULL, t_fun1, &zptr);
  zptr = &i; 
  __goblint_check(i == j); //UNKNOWN!
  pthread_join (id1, NULL);
}