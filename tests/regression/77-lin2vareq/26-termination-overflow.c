// SKIP TERM PARAM: --set "ana.activated[+]" lin2vareq

#include <stdio.h>

int main() {
  int i = 2147483647;
  i++;
  while (i <= 10) {
    printf("%d\n", i);
  }

  return 0;
}
