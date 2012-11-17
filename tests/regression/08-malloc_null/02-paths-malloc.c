// PARAM: --set ana.activated "[['base','escape','malloc_null']]"
#include <stdlib.h>
#include <stdio.h>


int main(void) {
  int *v, *u, r;
  
  u = (int*)malloc(sizeof(*u));
  v = (int*)malloc(sizeof(*v));

  *u = 10; // WARN
  *v = 10; // WARN

  if (v == 0)
    exit(-1);

  *u = 15; // WARN
  *v = 15; // NOWARN

  if (u == 0)
    exit(-1);

  *u = 20; // NOWARN
  *v = 20; // NOWARN

  if (r){
    u = (int*)malloc(sizeof(*u));
    v = (int*)malloc(sizeof(*v));
  }
  
  *u = 30; // WARN
  *v = 30; // WARN
  
  printf("???");
  
  if (r){
    if (u == 0 || v == 0)
      exit(-1);
  }

  assert(0); // FAIL
  *u = 40; // NOWARN
  *v = 40; // NOWARN
        
  return 0;
}
