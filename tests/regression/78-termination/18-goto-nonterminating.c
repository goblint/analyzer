// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  unsigned int num = 1;

loop:
  printf("Current number: %d\n", num);
  num++;

  goto loop; // NONTERMGOTO termination analysis shall mark goto statement up-jumping goto

  return 0;
}
