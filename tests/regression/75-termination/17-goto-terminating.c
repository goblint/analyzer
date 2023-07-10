// TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  int num = 1;

loop:
  printf("Current number: %d\n", num);
  num++;

  if (num <= 10)
  {
    goto loop; // We are not able to detect up-jumping gotos as terminating, we
               // just warn about them might being nonterminating.
  }

  return 0;
}
