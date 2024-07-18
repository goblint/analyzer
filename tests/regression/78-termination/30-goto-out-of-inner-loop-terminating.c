// SKIP TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  int rows = 5;
  int columns = 5;

  // Outer loop for rows
  for (int i = 1; i <= rows; i++)
  {
    // Inner loop for columns
    for (int j = 1; j <= columns; j++)
    {
      if (j == 3)
      {
        goto outer_loop; // Jump to the label "outer_loop"
      }
      printf("(%d, %d) ", i, j);
    }
    printf("Not Skipped?\n");
  outer_loop:; // Label for the outer loop
    printf("Skipped!\n");
  }

  return 0;
}

/*
NOTE: In case we do NOT assume no-overflow:
Test 30: terminates (test case "30-goto-out-of-inner-loop-terminating.c")
Test 35: does not terminate (test case
"35-goto-out-of-inner-loop-with-print-terminating.c")

The reason is explained in "35-goto-out-of-inner-loop-with-print-terminating.c"
*/
