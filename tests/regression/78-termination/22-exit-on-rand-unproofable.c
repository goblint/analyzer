// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdlib.h>

int main()
{
  int forever = 1;

  // This loop is not provable, therefore it should throw a warning
  while (forever == 1) // NONTERMLOOP termination analysis shall mark beginning of while as non-terminating loop
  {
    if (rand()) // May exit, may not
    {
      forever = 0;
    }
  }
}