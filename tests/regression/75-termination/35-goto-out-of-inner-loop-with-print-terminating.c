// TERM PARAM: --set "ana.activated[+]" termination --set "ana.activated[+]" apron --enable ana.int.interval --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none
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
  outer_loop:; // Label for the outer loop
    printf("\n");
  }

  return 0;
}

/*
NOTE: In case we do NOT assume no-overflow:
Test 30: terminates (test case "30-goto-out-of-inner-loop-terminating.c")
Test 35: does not terminate (test case
"35-goto-out-of-inner-loop-with-print-terminating.c")

The only difference between Test 30 and Test 35 is line 17. Test 30 has an
additional statement, and Test 35 continues already with the label. This
difference in Test 35 leads to an overflow in line 11, and hence to the
non-termination. This overflow is created by a WPoint Issue. By enabling the
no-overflow option this issue can be fixed and, both test cases are correctly
detected as terminating.

(The overflow also happens without the termination analysis enabled.)
*/
