// TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main() {
  int i;

  // Loop with a continue statement
  for (i = 1; i <= 10; i++) {
    if (i % 2 == 0) {
      continue;
    }
    printf("%d ", i);
  }
  printf("\n");

  // Loop with complex conditions
  for (i = 1; i <= 10; i++) {
    if (i > 5 && i % 2 == 0) {
      printf("%d ", i);
    }
  }
  printf("\n");

  // Loop with complex conditions
  i = 1;
  while (i <= 10) {
    if (i > 5 && i % 2 == 0) {
      printf("%d ", i);
    }
    i++;
  }
  printf("\n");
}