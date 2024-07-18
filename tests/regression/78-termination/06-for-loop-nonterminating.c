// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  for (;;) // NONTERMLOOP termination analysis shall mark beginning of for as non-terminating loop
  {
    printf("This loop does not terminate.\n");
  }

  return 0;
}
