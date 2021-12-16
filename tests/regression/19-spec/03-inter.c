//PARAM: --enable ana.int.interval
//  --enable exp.earlyglobs

#include <pthread.h>
#include <stdio.h>

int myglobal = 4;

int readG(int x ){
  return myglobal;
}

int main(void) {
  int a = 43;
  myglobal = 3;
  int b = readG(a);
  return 0;
}
