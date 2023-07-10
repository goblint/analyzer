// TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

void innerRecursiveFunction(int n)
{
  if (n == 0)
  {
    printf("Terminating inner recursion\n");
    return;
  }

  printf("Inner recursive call with n = %d\n", n);

  // Recursive call to the innerRecursiveFunction
  innerRecursiveFunction(n - 1);
}

void outerRecursiveFunction(int n)
{
  if (n == 0)
  {
    printf("Terminating outer recursion\n");
    return;
  }

  printf("Outer recursive call with n = %d\n", n);

  // Recursive call to the outerRecursiveFunction
  outerRecursiveFunction(n - 1);

  // Call to the innerRecursiveFunction
  innerRecursiveFunction(n);
}

int main()
{
  // Call the outerRecursiveFunction with an initial value
  outerRecursiveFunction(3);

  return 0;
}
