// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"
#include <assert.h>

void inc(int * a){
  (*a)++;
}

int four(){
  return 4;
}

void test1(int *q, int *p){
  int v = 10;
}


int main () {
  int x, y, z, uk;

  x = y = z;

  __goblint_check(x == y);
  __goblint_check(z == y);
  __goblint_check(x == z);

  test1(&x, &x);
//  __goblint_check(x == y); wontfix?  i think?
  __goblint_check(z == y);
//  __goblint_check(x == z);

  x = y = z;

  test1(&x, &y);
  __goblint_check(x == y);
//  __goblint_check(z == y);
//  __goblint_check(x == z);

  x = y = z;

  inc(&x);
  __goblint_check(x == y); // UNKNOWN
  __goblint_check(z == y);

  y = four();
  __goblint_check(z == y); // UNKNOWN
  __goblint_check(x == y); // UNKNOWN

  return 0;
}
