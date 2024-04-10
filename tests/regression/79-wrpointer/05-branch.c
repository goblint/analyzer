// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>

void main(void) {
  int *i;
  int **j;
  int *k;
  i = *(j + 3);
  *j = k;
  j = &k + 1;
  int *f;
  if (j != &k) {
    f = i;
    printf("branch1");
  } else {
    f = k;
    printf("branch2");
  }
}
