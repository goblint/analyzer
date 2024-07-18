// PARAM: --set ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t_fun(void *arg) {
  myglobal=40; //RACE
  return NULL;
}

int main(void) {
  // This should spawn a non-unique thread
  unknown(t_fun);
  return 0;
}
