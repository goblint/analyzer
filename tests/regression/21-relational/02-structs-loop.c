// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --set ana.int.queries true --sets solver slr3 --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']"
typedef struct  {
    int i;
    int k;
    
} S;

S some_function(){
    S xx;
    xx.i = 5;
    S z;
    z.i = 5;
    for (z.i = 5; z.i > 0; z.i--) {
        xx.i = xx.i - 1;
    }
    return xx;
}

int main(){
    S a;
    a.i = 5;
    a.k = 7;
    a = some_function();
    return a.i;
}