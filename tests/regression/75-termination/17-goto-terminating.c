// NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
// The program terminates but the analysis is currently only meant to detect up-jumping gotos as potentially NonTerminating, therefore we expect an NonTerm
#include <stdio.h>

int main()
{
  int num = 1;

loop:
  printf("Current number: %d\n", num);
  num++;

  if (num <= 10)
  {
    goto loop; // NONTERMGOTO termination analysis shall mark goto statement up-jumping goto
               // We are not able to detect up-jumping gotos as terminating, we
               // just warn about them might being nonterminating.
  }

  return 0;
}
