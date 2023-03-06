#include<stdio.h>
#include <goblint.h>

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
  __goblint_check(*p == 5);

  // writing through pointer
  *p = 8;
  __goblint_check(i == 8);

  // reading from a set of addresses
  // if p -> {i,j} and both are 8:
  j = 8;
  if (k1) p = &j;
  __goblint_check(*p == 8);
  // if j -> 7 and i -> 8, we don't know:
  j = 7;
  __goblint_check(*p == 8); // UNKNOWN!

  // writing to a set of addresses
  // p still points to i or j, writing to p should update both:
  i = 3; j = 7;
  *p = 3;
  __goblint_check(i == 3);
  __goblint_check(j == 7); // UNKNOWN!


  // pointer to pointers
  i = 7;
  pp = &p;
  *pp = &i;
  // __goblint_check(p == &i); wontfix!
  __goblint_check(*p == 7);
  __goblint_check(**pp == 7);

  **pp = 1;
  __goblint_check(i == 1);

  (**pp)++;
  __goblint_check(i == 2);


  // pointers to functions
  fp = fun_6;
  i = fp();
  __goblint_check(i == 6);

  // pointer to a set of functions
  if (k2) fp = fun_5; else fp = fun_6;
  i = fp();
  __goblint_check(i == 5); // UNKNOWN!

  if (k3) fp = fun_5; else fp = fun_5b;
  i = fp();
  __goblint_check(i == 5);

  return 0;
}
