// PARAM: --set ana.activated "['base']"  --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true

int main () {
    int a = 1;
    int b;
    int c;
    if (c == 0) {
        a = 2;
        b = 3;
    } else {
        a = 3;
        b = 2;
    }
    c = a + b;
    assert(c == 5);
    return c;
}

