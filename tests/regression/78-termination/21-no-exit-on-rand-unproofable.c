// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdlib.h>

int main()
{
  unsigned int forever, i = 0;

  // This loop is not provable, therefore it should throw a warning
  while (i < 4 || forever == 1) // NONTERMLOOP termination analysis shall mark beginning of while as non-terminating loop
  {
    i++;
    if (i == 4)
    {
      if (rand())
      {
        forever = 1;
      }
    }
  }
}
