// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval
#include <goblint.h>

int main(void) {
  short b;
  short a;
  int c = a; 
  int d = b;
  int cc = c + 20;
  int dd = d - 30;
  a = 3 * 1543;
  if (a*(c - cc) == a*(d -dd - 50)){
    __goblint_check(1);// (reachable)
    return 0;
  }else{
    __goblint_check(0);// NOWARN (unreachable)
    return -1;
  }
}
