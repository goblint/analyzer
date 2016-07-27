// PARAM: --disable ana.mutex.disjoint_types --set dbg.debug true --set ana.activated[+] "'var_eq'" 
#include<stdio.h>
#include<assert.h>

int main() {
  int x;
  int y;

  scanf("%d", &x);
  y = x;

  assert(x==y);

  return 0;
}
