// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  // Loop with a continue statement
  for (unsigned int i = 1; i <= 10; i++)
  {
    if (i % 2 == 0)
    {
      continue; // Forward goto to the increment. Not considered an upjumping goto: its label has the location of "for" in line 7, but its sid is larger.
    }
    printf("%d ", i);
  }
  printf("\n");


  // Loop with a continue statement
  for (unsigned int r = 1; r <= 10; r++)
  {
    if (r % 3 == 0)
    {
      continue; // Forward goto to the increment. Not considered an upjumping goto: its label has the location of "for" in line 19, but its sid is larger.
    }
    printf("Loop with Continue: %d\n", r);
  }
}
