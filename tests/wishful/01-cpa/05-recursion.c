#include<stdio.h>
#include<assert.h>

int fact(int n) {
  if (n > 0)
    return n * fact(n-1);
  else
    return 1;
}

int main() {
  int i,j;

  i = fact(0);
  assert(i == 1);
  
  // we will not go into demand-driven recursion:
  j = fact(100);
  assert_unknown(j);

  return 0;
}
