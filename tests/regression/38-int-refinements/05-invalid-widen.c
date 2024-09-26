//PARAM: --set ana.int.refinement once --enable ana.int.enums
// NOCRASH
#include <goblint.h>

int main() {
  int a = 3;
  while (1)
    a += 2;
}
