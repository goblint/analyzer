//PARAM: --set ana.activated '["base", "mallocWrapper"]'
// Copied & modified from 33/04.
#include <assert.h>

int main() {
    // state: {bot}, because no locals/globals
    assert(1); // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while reduce would remove
    assert(1); // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while reduce would remove
    return 0;
}
