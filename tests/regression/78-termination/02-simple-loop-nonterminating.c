// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  while (1) // NONTERMLOOP termination analysis shall mark beginning of while as non-terminating loop
  {
    continue;
  }

  return 0;
}
