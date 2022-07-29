// PARAM: --set ana.ctx_insens[+] 'base'  --set ana.base.privatization none
#include<stdlib.h>
int f(int, void*);

int g () {
  int i;
  int a[10];

  f(1, &i);
  // check that i is an integer and can be used for array indexing without Goblint crashing
  int r = a[i];
  a[i] = r;

  int* p = &i;
  int j = f(0, &p);

  return 0;
}

int f(int op, void *pArg){
  if (op == 0) {
    // If op is 0, make *pArg point to a new chunk of memory
    int *fd = malloc(sizeof(int));
    *(int**)pArg = fd;
  } else {
    // If op is 0, set *pArg to 0
    *(int*)pArg = 0;
  }
  return 0;
}

int main () {
  return g();
}
