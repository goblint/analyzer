// SKIP PARAM: --set ana.apron.narrowing_gas 1 --set ana.activated[+] apron --set ana.apron.domain polyhedra
// Apron is not precise enough for some nested loops
#include <goblint.h>
#include <stdio.h>

int loops0(){
  int i, j, k;
  int a = 0;
  for (i = 500; i >= 1; i--)
  {
    a++;
    __goblint_check(a + i - 501 == 0); // needs 1x narrowing or octagons
    int b = 0;
    for (j = i; j >= 1; j--)
    {
        __goblint_check(a + b + j == 501); // needs 1x narrowing, octagons insufficient
        b++;
    }
  }
  return 0;
}

int main()
{
  loops0();
 
  return 0;
}
