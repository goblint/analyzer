// PARAM: --enable ana.int.interav --set ana.int.refinement fixpoint
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int a = rand();

  if (a % 8 == 3) {
    int b = a & 0x7;
    assert(b == 3);  // SUCCESS
  }
}
