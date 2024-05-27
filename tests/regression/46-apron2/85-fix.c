// SKIP PARAM: --set ana.activated[+] apron --set sem.int.signed_overflow assume_none
#include<pthread.h>

int main() {
  int d = 1;
  while (d < 6) {
    if (0)
      return 0; //FIXPOINT: Earlier this fixpoint was not reached here
    d++;
  }

  return 0;
}
