// PARAM: --set dbg.debug true --set ana.activated[0][+] "'var_eq'" 
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

  assert(x == y); 
  assert(z == y); 
  assert(x == z); 
  
  test1(&x, &x);
//  assert(x == y); wontfix?  i think?
  assert(z == y); 
//  assert(x == z); 

  x = y = z;

  test1(&x, &y);
  assert(x == y); 
//  assert(z == y); 
//  assert(x == z); 

  x = y = z;

  inc(&x);
  assert(x == y); // UNKNOWN
  assert(z == y); 
  
  y = four();
  assert(z == y); // UNKNOWN
  assert(x == y); // UNKNOWN
  
  return 0;
}