#include <assert.h>
#include <stdio.h>

int main(void) {
  int k = 4;
  char *x = "18";

  // scanf?
  scanf("%d", &k);
  assert(k == 4); // UNKNOWN

  // printf?
  printf("Your number is %d", k);
  assert(k == 4); // UNKNOWN

  k = 8;
  printf("Your number is %d", k);
  assert(k == 8);

  //scanf
  sscanf(x, "%d", &k);
  assert(k == 8); // UNKNOWN
  printf("This is %d", k);

  return 0;
}
