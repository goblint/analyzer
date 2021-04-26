#include<stdio.h>
#include<assert.h>

int fun_5() { return 5; }
int fun_6() { return 6; }
int fun_5b() { return 5; }

int main () {
  int i,j,k1,k2,k3;
  int *p, **pp;
  int (*fp)(); //pointer to function

  // reading through pointer
  i = 5;
  p = &i;
  assert(*p == 5);

  // writing through pointer
  *p = 8;
  assert(i == 8);

  // reading from a set of addresses
  // if p -> {i,j} and both are 8:
  j = 8;
  if (k1) p = &j;
  assert(*p == 8);
  // if j -> 7 and i -> 8, we don't know:
  j = 7;
  assert(*p == 8); // UNKNOWN!

  // writing to a set of addresses
  // p still points to i or j, writing to p should update both:
  i = 3; j = 7;
  *p = 3;
  assert(i == 3);
  assert(j == 7); // UNKNOWN!


  // pointer to pointers
  i = 7;
  pp = &p;
  *pp = &i;
  // assert(p == &i); wontfix!
  assert(*p == 7);
  assert(**pp == 7);

  **pp = 1;
  assert(i == 1);

  (**pp)++;
  assert(i == 2);


  // pointers to functions
  fp = fun_6;
  i = fp();
  assert(i == 6);

  // pointer to a set of functions
  if (k2) fp = fun_5; else fp = fun_6;
  i = fp();
  assert(i == 5); // UNKNOWN!

  if (k3) fp = fun_5; else fp = fun_5b;
  i = fp();
  assert(i == 5);

  return 0;
}
