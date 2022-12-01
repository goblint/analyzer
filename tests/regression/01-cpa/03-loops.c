#include<stdio.h>
#include <goblint.h>

int main () {
  int i,j,k;

  i = k = 0; j = 7;
  while (i < 10) {
    i++;
    j = 7;
    k = 5;
  }
  __goblint_check(i == 10); //UNKNOWN
  __goblint_check(k); //UNKNOWN
  // k is currenlty 0 \sqcup 5, if we unfolded the loops it would be 5
  __goblint_check(j==7);
  return 0;
}
