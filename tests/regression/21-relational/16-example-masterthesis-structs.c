// PARAM: --set ana.activated "['base']"  --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true

typedef struct  {
    int a;
    int b;
} P;

int main () {
    P p;
    p.a = 1;
    int c;
    if (c == 0) {
        p.b = 3;
    } else {
        p.b = 2;
    }
    return 0;
}

