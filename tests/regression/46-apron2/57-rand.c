// SKIP PARAM: --disable ana.int.def_exc --enable ana.int.congruence --set ana.activated[+] apron --set ana.base.privatization none --set ana.relation.privatization top
// Strange int domains, I know: The ranges of def_exc are already able to prove this assertion, meaning it is no longer about apron also knowing this.
// Disabling all int domains leads to all queries returning top though, meaning the assertion is not proven.
// Congruence offers support for constants, but does not contain any poor man's intervals. => That's why we use it here.
#include <goblint.h>
#include <stdlib.h>

int main() {
    int i = rand();
    __goblint_check(i >= 0);
}
