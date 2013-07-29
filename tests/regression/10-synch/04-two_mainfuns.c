// SKIP PARAM: --set exitfun "['f1','f2']"
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
