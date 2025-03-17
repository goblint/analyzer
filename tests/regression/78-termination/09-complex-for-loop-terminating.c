// SKIP TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none
// Apron is not precise enough for some nested loops
#include <stdio.h>

int loops0(){
  unsigned int i, j, k;

  // Outer loop
  for (i = 1; i <= 5; i++)
  {
    // Inner loop 1
    for (j = 1; j <= i; j++)
    {
      printf("%d ", j);
    }
    printf("\n");

    // Inner loop 2
    for (k = i; k >= 1; k--)
    {
      printf("%d ", k);
    }
    printf("\n");
  }

  // Additional loop
  for (i = 5; i >= 1; i--)
  {
    for (j = i; j >= 1; j--)
    {
      printf("%d ", j);
    }
    printf("\n");
  }
  return 0;
}

int loops1(){
  unsigned int i, j, k;

  // Loop with conditions
  for (i = 1; i <= 10; i++)
  {
    if (i % 2 == 0)
    {
      printf("%d is even\n", i);
    }
    else
    {
      printf("%d is odd\n", i);
    }
  }

  // Loop with nested conditions
  for (i = 1; i <= 10; i++)
  {
    printf("Number: %d - ", i);
    if (i < 5)
    {
      printf("Less than 5\n");
    }
    else if (i > 5)
    {
      printf("Greater than 5\n");
    }
    else
    {
      printf("Equal to 5\n");
    }
  }

  // Loop with a break statement
  for (i = 1; i <= 10; i++)
  {
    printf("%d ", i);
    if (i == 5)
    {
      break;
    }
  }
  printf("\n");

  // Loop with multiple variables
  unsigned int a, b, c;
  for (a = 1, b = 2, c = 3; a <= 10; a++, b += 2, c += 3)
  {
    printf("%d %d %d\n", a, b, c);
  }
  return 0;
}

int main()
{
  loops0();
  loops1();

  return 0;
}
