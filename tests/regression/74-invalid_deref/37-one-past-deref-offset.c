// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

struct S {
  unsigned char a;
  unsigned char b:2;
  unsigned char c:2;
  unsigned char d;
} __attribute__((packed));

int main(void) {
  struct S *p = malloc(2);
  p->d = 1; //WARN
  free(p);
  return 0;
}
