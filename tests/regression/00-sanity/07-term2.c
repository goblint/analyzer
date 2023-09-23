#include<stdlib.h>

void f() {
  exit(0);
}

int main() {
  while (1);
  __goblint_check(0); // NOWARN (unreachable), formerly NONTERM
  return 0;
}
