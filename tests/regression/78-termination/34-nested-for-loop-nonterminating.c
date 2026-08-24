// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  unsigned int outerCount, innerCount;

  for (outerCount = 1; outerCount <= 3; outerCount++)
  {
    for (innerCount = 1; innerCount > 0; innerCount += 4) // NONTERMLOOP termination analysis shall mark beginning of for as non-terminating loop
    {
      printf("(%d, %d) ", outerCount, innerCount);
    }

    printf("\n");
  }

  return 0;
}
