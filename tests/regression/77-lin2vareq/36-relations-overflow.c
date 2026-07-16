//SKIP PARAM:  --enable ana.int.interval  --set sem.int.signed_overflow assume_none  --set ana.activated[+] lin2vareq

#include <goblint.h>

int nondet() {
  int x;
  return x;
}
int SIZE = 1;
int rand;

int main() {
  unsigned int n=2,i=8;
  n = i%(SIZE+2); //NOWARN

  rand=nondet();

  if (rand>5 && rand<10) {
    n= i%(rand+2); //NOWARN
  }

  return 0;
}

