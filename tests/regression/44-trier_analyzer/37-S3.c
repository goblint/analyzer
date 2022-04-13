#include<stdio.h>
#include<assert.h>

typedef struct {
  int re;
  int im;
} complex;

main () {
  complex c, d;
  int x;
  d.re = d.im = 0;
  scanf("%d",&(c.re));
  scanf("%d",&(c.im));
  c.re = 1;
  d = c;
  x = d.im;
  printf("%d\n",x);
  assert(x); //UNKNOWN!
  assert(d.re == 1);
  return 0;
}
