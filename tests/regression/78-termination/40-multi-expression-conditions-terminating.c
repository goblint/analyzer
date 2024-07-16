// SKIP TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  int i;

  // Loop with complex conditions
  for (i = 1; i <= 10; i++)
  {
    if (i > 5 && i % 2 == 0) // CIL defines new jump labels to default location (-1)
    {
      printf("%d ", i);
    }
  }
  printf("\n");

  // Loop with complex conditions
  i = 1;
  while (i <= 10)
  {
    if (i > 5 && i % 2 == 0) // CIL defines new jump labels to default location (-1)
    {
      printf("%d ", i);
    }
    i++;
  }
  printf("\n");

  // Loop with multiple conditions
  int s = 1;
  while (s <= 10 && s % 2 == 0) // CIL defines new jump labels to default location (-1)
  {
    printf("Loop with Multiple Conditions: %d\n", s);
    s++;
  }

  // Loop with multiple variables
  int t, u;
  for (t = 1, u = 10; t <= 5 && u >= 5; t++, u--) // CIL defines new jump labels to default location (-1)
  {
    printf("Loop with Multiple Variables: %d %d\n", t, u);
  }
}