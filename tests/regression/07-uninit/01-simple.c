// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"
#include <stdio.h>

int main() {
  int i,j,k;

  j = 6;
  k = j + 4; // NOWARN

  k = i + 8; // WARN

  return 0;
}
