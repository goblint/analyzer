// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --set ana.int.queries true --sets solver widen2 --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']"
typedef struct  {
    int i;
    int k;
    
} S;

int some_function(){
    int xx;
    xx = 5;
    S z;
    z.i = 5;
    for (z.i = 5; z.i > 0; z.i--) {
        xx = xx - 1;
    }
    z.i = 6;
    for (xx = 5; xx > 0; xx--) {
        z.i = z.i - 1;
    }
    return xx;
}

int main(){
    S a;
    a.i = 5;
    a.k = 7;
    a.i = some_function();
    return a.i;
}