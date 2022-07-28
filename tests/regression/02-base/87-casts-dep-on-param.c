// PARAM: --set ana.activated "['base', 'mallocWrapper']" --set ana.ctx_insens[+] 'base'  --set ana.base.privatization none

int f(int, void*);

int g () {
  int i;
  char const *j;
  int a[10];

  int k = f(1, &i);
  // check that i is an integer and can be used for array indexing without Goblint crashing
  j = a[i];

  int* p = &i;
  int j = f(0, &p);

  return 0;
}

int f(int op, void *pArg){
  int op;
  int *fd;
  if (op == 0) {
    *(int**)pArg = fd;
  } else {
    *(int*)pArg = fd;
  }
  return 0;
}

int main () {
  return g();
}
