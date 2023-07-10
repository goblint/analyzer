// TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>
#include <stdlib.h>

int main() {
  int short_run, i = 0;

  while (i < 90 &&
         short_run != 1) // Currently not able to detect this as terminating
  {
    i++;
    if (rand()) {
      short_run = 1;
    }
  }
}