// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --set ana.int.queries true --sets solver slr3 --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.apron true --set ana.structs.equations false
typedef struct  {
    int i;
    int k;
} S;

S glob1;
S glob2;
int globInt;

int return_int(){
    int xx;
    xx = 5;
    glob2.k = glob1.i;
    glob2.i = glob1.k;
    S z;
    z.i = 5;
    for (z.i = 5; z.i > 0; z.i--) {
        xx = xx - 1;
    }
    xx = 5;
    z.i = 6;
    for (xx = 5; xx > 0; xx--) {
        z.i = z.i - 1;
    }
    glob1.i = -1;
    globInt = 6;
    return xx;
}

S return_struct(){
    int xx;
    xx = 5;
    glob2.k = glob1.i;
    glob2.i = glob1.k;
    S z;
    z.i = 5;
    globInt = 7;
    glob1.i = -1;
    return glob1;
}

int main(){
    S a;
    a.i = 5;
    a.k = 7;
    glob1.i = 5 * 12;
    glob1.k = 27;
    globInt = 5;
    assert (glob1.i == 60);
    assert (glob1.k == 27);
    assert (globInt == 5);
    a = return_struct();
    assert (globInt == 7);
    //a.i = return_int();
    //assert (globInt == 6);
    assert (glob2.k == 60);
    assert (glob2.i == 27);
    assert (glob1.i == -1);
    return a.i;
}