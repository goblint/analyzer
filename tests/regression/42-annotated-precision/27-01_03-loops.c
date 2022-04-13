// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint
#include<stdio.h>
#include<assert.h>
int main () __attribute__((goblint_precision("def_exc")));

int main () {
  int i,j,k;

  i = k = 0; j = 7;
  while (i < 10) {
    i++;
    j = 7;
    k = 5;
  }
  assert(i == 10); //UNKNOWN
  assert(k); //UNKNOWN
  // k is currenlty 0 \sqcup 5, if we unfolded the loops it would be 5
  assert(j==7);
  return 0;
}
