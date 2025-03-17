// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
  // Seed the random number generator
  srand(time(NULL));

  if (rand())
  {
    // Loop inside the if part
    for (unsigned int i = 1; i <= 5; i++)
    {
      printf("Loop inside if part: %d\n", i);
    }
  }
  else
  {
    // Loop inside the else part
    unsigned int j = 1;
    while (j <= 5)
    {
      printf("Loop inside else part: %d\n", j);
      j++;
    }
  }

  return 0;
}
