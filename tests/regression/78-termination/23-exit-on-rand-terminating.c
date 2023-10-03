// SKIP TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>
#include <stdlib.h>

int main()
{
  int short_run, i = 0;
  // Currently not able to detect this as terminating due to multiple conditions
  while (i < 90 && short_run != 1)
  {
    i++;
    if (rand())
    {
      short_run = 1;
    }
  }
}