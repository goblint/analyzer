#include <goblint.h>

int x;
int a;
int *xptr;
int **xptr2;


int f(int *y){
  int z;
  z = 9;
  *xptr = 6;
  **xptr2 = 8;
  *xptr2 = xptr;
  y = 6;

  return 0;
}

int g(){
  return 0;
}


int main ()
{
  int y;
  if (y)
    xptr = &x;
  else
    xptr = &x;
  xptr2 = &xptr;

  f(&y);
  g();
  



}