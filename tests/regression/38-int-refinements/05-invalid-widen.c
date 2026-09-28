//PARAM: --set ana.int.refinement once --enable ana.int.enums
// NOCRASH
// TODO: This test is ineffective since https://github.com/goblint/analyzer/pull/1675 because refinement no longer moves Not{2} from def_exc to enums like in https://github.com/goblint/analyzer/issues/864.
#include <goblint.h>

int main() {
  int a = 3;
  while (1)
    a += 2;
}
