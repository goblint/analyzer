#include<stdio.h>
#include<assert.h>

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
  assert(sum == 1); // UNKNOWN
  assert(k == 7);
  return 0;
}
