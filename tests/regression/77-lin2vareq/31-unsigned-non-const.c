// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval
#include <goblint.h>

int main(void) {
  int top;
  unsigned int i = 0;
  unsigned int j;

  if(top) {
    i = 1;
  }

  j = i+1;

  __goblint_check(j == i+1);
}
