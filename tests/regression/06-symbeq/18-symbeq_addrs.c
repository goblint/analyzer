// PARAM: --disable ana.mutex.disjoint_types --set dbg.debug true --set ana.activated[+] "'var_eq'" 
#include<stdlib.h>

typedef struct {
  int x,y;
} S;

int main () {
  int x, y, z, uk;
  int *p, *q, *r;
  S a, b, *ps;
  
  x = y = z;
  
  assert(x == y);
  assert(x == z);
  assert(z == y);
  
  x = uk+10; y = uk+20; z = uk+30;
  
  x = y;
  x = z;
  
  assert(x == z);
  assert(x == y); // UNKNOWN
  x = 40+uk;
  
  if (uk) {
    p = &x;
    ps = &a;
  } else {
    p = &y;
    ps = &b;
  }
  
  y = *p;
  assert(y == *p); 
  p = &z; 
  assert(y == *p); // UNKNOWN
  p = NULL+10;
  
  r = &ps->x; 
  assert(r == &ps->x);
  ps = &a;
  assert(r == &ps->x);//UNKNOWN
  
  return 0;
}
