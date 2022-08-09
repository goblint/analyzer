//PARAM: --set ana.activated '["constants"]'
// intentional explicit ana.activated to do tutorial in isolation

int g;

int main() {
    // state: {bot}, because no locals
    if (g) {
        g = 1; // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while reduce would remove
    }
    else {
        g = 2; // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while reduce would remove
    }
    // old broken state: {}, because reduce is applied after join and that (old broken) removed bot
    g = 3; // old broken state: {} and PathSensitive2 map raises Deadcode
    return 0;
}
