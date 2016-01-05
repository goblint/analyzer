// PARAM: --sets ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal1;
int myglobal2;

void *f1(void *arg) {
  myglobal1=42; //NOWARN
  return NULL;
}

void *f2(void *arg) {
  myglobal2=42; //NOWARN
  return NULL;
}

int main(void) {
  pthread_t t1, t2;
  pthread_create(&t1, NULL, f1, NULL);
  pthread_create(&t2, NULL, f2, NULL);
  return 0;
}
