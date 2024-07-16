// PARAM: --set ana.activated[+] taintPartialContexts
#include <goblint.h>

int rec_f(int *ptr) {
  // function is called two times:
  // 1. from main where *ptr<1> = x_main = 0. x_f<1> = 6 is initialized, 
  //   then rec_f(&x_f<1>) is called and the return value is returned to main
  // 2. from rec_f<1> where *ptr<2> = x_f<1> = 6. This is changed to -6, tainting x_f. 
  // It is important that at this point x_f is not removed from the taint_set (because it is local), 
  //   otherwise in rec_f<1> x_f will be untainted and the old (wrong) value of 6 is kept
  int x_f = 6;
  if (*ptr == 0){
    rec_f(&x_f);
  } else {
    *ptr = -6;
  }
  return x_f;
}

void main () { 
  int x_main = 0;
  int c;
  c = rec_f(&x_main);
  __goblint_check(c == 6); //UNKNOWN
}
