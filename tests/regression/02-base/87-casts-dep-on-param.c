// PARAM: --set ana.activated "['base', 'mallocWrapper']" --set ana.ctx_insens[+] 'base' --set ana.ctx_insens[+] 'mallocWrapper' --set ana.base.privatization none
// should be fully context-insensitive to activate minimal analyses
// none privatization because mutex deactivated
#include<stdlib.h>
int f(int, void*);

int g () {
  int i;
  int a[10];

  f(1, &i);
  // NOCRASH: check that i is an integer and can be used for array indexing without Goblint crashing
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
