#include "string.h"
#include "stdio.h"

int main(){
  int a[] = {1,2,3};
  int b[] = {4,5};
  memcpy(a, b, sizeof(int)*2);
  printf("%d %d %d %d %d\n", a[0], a[1], a[2], b[0], b[1]);
  return 0;
}
