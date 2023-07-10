// TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  int counter = 0;

  goto jump_point;

  while (1)
  {
    counter++;

    // Dummy code
    printf("Iteration %d\n", counter);
    int result = counter * 2;
  jump_point:
    printf("Result: %d\n", result);

    // Condition to terminate the loop
    if (result >= 10)
    { // Apron is not able to detect this
      goto end;
    }
  }

end:
  printf("Loop exited. Result is greater than or equal to 10.\n");

  return 0;
}
