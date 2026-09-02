//SKIP PARAM:  --enable ana.int.interval  --set sem.int.signed_overflow assume_none  --set ana.activated[+] lin2vareq

#include <goblint.h>

// produces top value
int nondet() {
  int x;
  return x;
}

// check, that due to internal (apron-) representation via GEQ instead of LT we not by accident 
// impose -x>=-100 as a constraint, since this has a diffrent semantics then x<100
int geqrep()
{
  int a;
  int x=a;
  if(x<100) 
  {
    __goblint_check(x<100); //SUCCESS
    x=x;
  }                           
  __goblint_check(x>=-2147483647); //UNKNOWN
  return 0;
}

int SIZE = 1;
int rand;

// check, that we do not infer a wrong overflow warning, due to internally computing 
// with possibly overflowing signed values in sharedFunctions / texpr1_expr_of_cil_exp
int overflow() {
  unsigned int n=2,i=8;
  n = i%(SIZE+2); //NOWARN

  rand=nondet();

  if (rand>5 && rand<10) {
    n= i%(rand+2); //NOWARN
  }


  return 0;
}

int main(){

  geqrep();

  overflow();

}