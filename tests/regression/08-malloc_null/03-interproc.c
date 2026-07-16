// PARAM: --set ana.activated[+] malloc_null --set ana.ctx_sens "['thread']"
// Making all analyses context-insensitive except an irrelevant one, we want to rely on malloc_null here
#include <stdlib.h>
#include <goblint.h>

void fun(int* a, int c) {
  if(c == 1) {
    if(a == NULL) { return; }
  }

  *a = 5; //NOWARN
}


int main(void) {
  int *x;

  x = malloc(sizeof(int));
  fun(x,1);

  int y;
  fun(&y, 2);

  return 0;
}
