// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
  unsigned int i = 1;

  do
  {
    printf("Inside the do-while loop\n");
    i++;
  } while (i <= 5);

  printf("Exited the loop\n");
  return 0;
}
