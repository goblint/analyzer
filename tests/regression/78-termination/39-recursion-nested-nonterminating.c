// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

void innerRecursiveFunction() // TODO NONTERMFUNDEC termination analysis shall mark fundec of non-terminating function but can not as dead code is not analysed
{
  printf("Nested recursive call\n");

  // Recursive call to the innerRecursiveFunction
  innerRecursiveFunction();
}

void outerRecursiveFunction()  // NONTERMFUNDEC termination analysis shall mark fundec of non-terminating function
{
  printf("Outer recursive call\n");

  // Recursive call to the outerRecursiveFunction
  outerRecursiveFunction();

  // Call to the innerRecursiveFunction
  innerRecursiveFunction();
}

int main()
{
  // Call the outerRecursiveFunction
  outerRecursiveFunction();

  return 0;
}
