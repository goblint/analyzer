// TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main() {
  int i = 1;

  while (i != 0) {
    printf("%d\n", i);
    i++;
    if (i>10) {
        return 0;
    }
  }
}
