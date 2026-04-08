// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval
#include <goblint.h>
#include <limits.h>

int main(void) {
  int top;
  unsigned int i;
  unsigned int j;

  if(top) {
    i = 3;
    j = i + UINT_MAX;
  } else {
    i = 2;
    j = i + UINT_MAX;
  }


  // Both hold in the concrete
  __goblint_check(j == i-1); //UNKNOWN
  __goblint_check(j == i + UINT_MAX); //UNKNOWN
}
