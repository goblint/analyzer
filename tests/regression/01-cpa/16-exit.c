#include<stdlib.h>
#include<stdio.h>
#include <goblint.h>

main () {
  int x;
  scanf("%d",&x);
  if (x != 7) {
    printf("Immediate exit.\n");
    exit(0);
  }
  printf("The number was not zero.\n");
  __goblint_check(x == 7);
  return 0;
}
