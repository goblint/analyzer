#include<stdio.h>
#include<assert.h>

int f() { return 3; }
int add(int x, int y) { return x+y; }
int add1(int x) { return x+1; }

int glob;
void g_inc() { glob++; }

int main () {
  int i=3;

  // check that function calls don't destroy the local state
  f();
  assert(i == 3);

  //check parameter passing
  i = add1(5);
  assert(i == 6);

  i = add1(17);
  assert(i == 18);

  // Check some more...
  // How many different states are analyzed?
  i = add(6,4);
  assert(i == 10);

  i = add(3,8);
  assert(i == 11);

  i = add(6,4);
  assert(i == 10);

  i = add(3,8);
  assert(i == 11);

  // Check on global variables
  g_inc();
  assert(glob == 1);
  g_inc();
  assert(glob == 2);
  glob=0;
  g_inc();
  assert(glob == 1);

  return 0;
}
