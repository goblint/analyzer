// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  int outerCount = 1;

  while (outerCount <= 3)
  {
    unsigned int innerCount = 1;

    while (1) // NONTERMLOOP termination analysis shall mark beginning of while as non-terminating loop
    {
      printf("(%d, %d) ", outerCount, innerCount);
      innerCount++;
    }

    printf("\n");
    outerCount++;
  }

  return 0;
}
