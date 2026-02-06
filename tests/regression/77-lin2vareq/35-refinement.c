//SKIP PARAM:  --enable ana.int.interval --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
//  initial test for interval-refinement by lin2vareq

#include <goblint.h>

void main() {
  int a;
  int b = -7*a+3;
  int c = -5*a+5;
  int d = 13*a+11;
  int e = a;

  if (b < 5){ // a  > -1
    
    __goblint_check(1 == 1); //SUCCESS
    __goblint_check(a > -1); //SUCCESS
    __goblint_check(c <  7); //SUCCESS
    __goblint_check(d >  8); //SUCCESS
    __goblint_check(e > -1); //SUCCESS

    if (a < 1){

      __goblint_check(1 == 1); //SUCCESS

      // for invariants that solely depend on the reference variable without a factor, we get:
      // short node: 2linvar does not know this, we get this from the interval domain
      __goblint_check(a == 0); //SUCCESS
      __goblint_check(e == 0); //SUCCESS
      
      // TODO: a==0 is not fed back to 2linvar, 
      // thus 2linvar can not infer abcde being constant

      // for all the others, we can not infer their constant value
      __goblint_check(b > -4);  //SUCCESS
      __goblint_check(c >  0);  //SUCCESS
      __goblint_check(d < 24);  //SUCCESS

      if (b < 3) {
        __goblint_check(0); //NOWARN (unreachable)
        b = 1701;
      }

      // in theory, if we knew about a being constant, we could infer the following:
      __goblint_check(b == 3);  //UNKNOWN
      __goblint_check(c == 5);  //UNKNOWN
      __goblint_check(d ==11);  //UNKNOWN

      b=42;
    }

    a=69;
  }
  else {
    
    __goblint_check(1 == 1); //SUCCESS
    __goblint_check(a <=-1); //SUCCESS
    __goblint_check(a <=-1); //SUCCESS
    __goblint_check(c >= 7); //SUCCESS
    __goblint_check(d <= 8); //SUCCESS
    __goblint_check(e <=-1); //SUCCESS
    
    a = 6;
  }
  //
  //if (a<1)  {
  ////  __goblint_assert(b<5); //SUCCESS
  //}
}