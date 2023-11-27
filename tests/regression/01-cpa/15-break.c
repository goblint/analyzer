#include<stdio.h>
#include <goblint.h>

main () {
  int sum = 0, k = 7;
  int i;
  for (i = 0; i < 100; i++) {
    if (i < 11)
      sum += i;
    else
      break;
  }
  printf("%d\n",sum);
  __goblint_check(sum == 1); // UNKNOWN
  __goblint_check(k == 7);
  return 0;
}
