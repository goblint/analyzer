//PARAM: --set ana.activated '["constants"]'

int g;

int main() {
    // state: {bot}, because no locals
    if (g) {
        g = 1; // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while reduce would remove
    }
    else {
        g = 2; // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while reduce would remove
    }
    // state: {}, because reduce is applied after join and that removes bot
    g = 3; // state: {} and PathSensitive2 map raises Deadcode
    return 0;
}
