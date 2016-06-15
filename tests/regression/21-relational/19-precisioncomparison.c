// Non-Relational trier:
// goblint precisioncomparison.c --set ana.activated "['base']"

// Non-Relational interval:
// goblint precisioncomparison.c --set ana.int.interval true --set ana.int.trier false --sets solver slr3 --set ana.activated "['base']"

// Relational Integer Equation trier:
// goblint precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval false --set ana.int.trier true --set ana.int.relational true --set ana.int.equations true

// Relational Integer Equation interval:
// goblint precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true

// Relational Int Apron:
// goblint precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.aprondomain true --sets solver slr3

// Relational Struct Equation trier:
// goblint precisioncomparison.c --set ana.activated "['base']" --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.equations true

// Relational Struct Equation interval:
// goblint precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.equations true --sets solver widen2

// Relational Struct Apron:
// goblint precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational false --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.apron true --set ana.structs.equations false --sets solver slr3

typedef struct  {
    int i;
    int k;
} S;

S globalStruct;
int globalInt;

S return_struct(){
    int localInt1;
    localInt1 = 5;
    globalInt = 7;
    for(globalStruct.i = 5; globalStruct.i > 0; globalStruct.i--){
        localInt1 = localInt1 - 1;
    }
    return globalStruct;
}

int return_int(){
    int localInt2, localInt3, localInt4;
    if (localInt4 < 0) {
        localInt2 = 6;
        localInt3 = 5;
    } else {
        localInt2 = 5;
        localInt3 = 6;
    }
    return localInt2 + localInt3;
}

int main(){
    S localStruct;
    int localInt5;
    localStruct.i = 5;
    localStruct.k = 7;
    globalStruct.i = 5 * 12;
    globalStruct.k = 27;
    localInt5 = return_int();
    localStruct = return_struct();
    return localStruct.i;
}