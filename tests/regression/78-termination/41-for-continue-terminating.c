// SKIP TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  // Loop with a continue statement
  for (int i = 1; i <= 10; i++)
  {
    if (i % 2 == 0)
    {
      continue; // Converted to an goto to "for" in line 7
    }
    printf("%d ", i);
  }
  printf("\n");


  // Loop with a continue statement
  for (int r = 1; r <= 10; r++)
  {
    if (r % 3 == 0)
    {
      continue; // Converted to an goto to "for" in line 19
    }
    printf("Loop with Continue: %d\n", r);
  }
}