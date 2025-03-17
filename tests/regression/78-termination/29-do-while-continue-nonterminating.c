// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  unsigned int i = 1;

  do
  {
    printf("Inside the do-while loop\n");
    i++;

    if(i ==0) {i = 2;}

    if (i % 2)
    {
      printf("Continue as %i is odd\n", i);
      continue;
    }
  } while (i >= 2); // NONTERMLOOP termination analysis shall mark beginning of while as non-terminating loop

  printf("Exited the loop\n");
  return 0;
}
