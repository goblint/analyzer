// SKIP TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  unsigned int rows = 3;
  unsigned int columns = 4;

  // Nested loop to iterate over rows and columns
  for (int i = 1; i <= rows; i++)
  {
    for (int j = 1; j <= columns; j++)
    {
      printf("(%d, %d) ", i, j);
    }
    printf("\n");
  }

  return 0;
}
