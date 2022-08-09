//PARAM: --set ana.activated '["constants"]'
// intentional explicit ana.activated to do tutorial in isolation

int g;

int main() {
    // state: {bot}, because no locals
    g = 1; // state: {bot}, because Hoare set add (in PathSensitive2 map) keeps bot, while old broken reduce would have removed
    return 0;
}
