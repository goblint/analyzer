#include<stdlib.h>
#include<stdio.h>
#include<assert.h>

main () {
  int x;
  scanf("%d",&x);
  if (x != 7) {
    printf("Immediate exit.\n");
    exit(0);
  }
  printf("The number was not zero.\n");
  assert(x == 7);
  return 0;
}
