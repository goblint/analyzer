// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>

int c = 2;
int z = 7;

void main() {
  c = z + 3;
  int x = c;
}