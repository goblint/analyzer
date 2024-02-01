#include <stdlib.h>

int main(void) {
  char *x = malloc(64);
  char *y = x;
  free(y);
exit:
  return 0;
}
