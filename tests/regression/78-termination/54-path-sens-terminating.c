// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --set ana.activated[+] expsplit --enable ana.int.interval --set ana.apron.domain polyhedra
// From https://github.com/goblint/analyzer/pull/2305#issuecomment-6687235679.
#include <stdio.h>
#include <goblint.h>

int main()
{
  int c = 0;
  int x = 0;
  int mode = 0;

  __goblint_split_begin(mode);

  while (c < 20) {
    c++;
    mode = !mode;
    x = mode ? c - 100 : c;
  }

  return 0;
}
