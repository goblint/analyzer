//PARAM: --set ana.activated '["base", "mallocWrapper", "assert"]'  --set ana.base.privatization none
// intentional explicit ana.activated to have non-dead bot local state
// Copied & modified from 33/04.
#include <assert.h>

int main() {
    // state: {bot}, because no locals/globals
    __goblint_check(1); // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while reduce would remove
    __goblint_check(1); // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while reduce would remove
    return 0;
}
