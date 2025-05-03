// SKIP PARAM: --set ana.activated[+] lin2vareq_p --set sem.int.signed_overflow assume_none
#include <stdio.h>

int main() {
  int i, j, k;
  int size; //with a fixed size, the inequalities lead to a discovery of exact constants and no relations 
  i = 0;
  j = 0;
  k = 5;

  if (size > 0) { 
    for (i = 1; i < size; ++i) {
      j = i;
      k = j + 5;
    }

    __goblint_check(j + 1 == i); // SUCCESS

    __goblint_check(k == i + 4); // SUCCESS
  
  }
  return 0;
}
