// PARAM: --set ana.activated[+] malloc_null
#include <stdlib.h>
#include <stdio.h>


int main(void) {
  int *v, *u, r;

  u = (int*)malloc(sizeof(*u)); // NOWARN (u may be NULL, but sizeof does not evaluate)
  v = (int*)malloc(sizeof(*v)); // NOWARN (v may be NULL, but sizeof does not evaluate)

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
    u = (int*)malloc(sizeof(*u)); // NOWARN (u is not NULL and sizeof does not evaluate anyway)
    v = (int*)malloc(sizeof(*v)); // NOWARN (v is not NULL and sizeof does not evaluate anyway)
  }

  *u = 30; // WARN
  *v = 30; // WARN

  printf("???");

  if (r){
    if (u == 0 || v == 0)
      exit(-1);
  }

  *u = 40; // NOWARN
  *v = 40; // NOWARN

  return 0;
}
