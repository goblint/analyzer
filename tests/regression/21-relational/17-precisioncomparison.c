// GEHT Struct KEIN FIXPOINT PROB: Apron PARAM: goblint --html -o ../results/results_final_analysis_thesis/struct-apron/  tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational false --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.apron true --set ana.structs.equations false --sets solver slr3

// GEHT Struct KEIN FIXPOINT PROB: Equations trier PARAM: goblint --html -o ../results/results_final_analysis_thesis/struct-equations-trier/ tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval false --set ana.int.trier true --set ana.int.relational false --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.equations true
// GEHT Struct FIXPOINT PROB: Equations interval PARAM: goblint --html -o ../results/results_final_analysis_thesis/struct-equations-interval/ tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.equations true

// GEHT Struct + Int: Equations interval + trier + relational int PARAM FIXPOINT PROB: goblint --html -o ../results/results_final_analysis_thesis/struct-integers-equations-interval-trier/  tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.equations true
// GEHT Struct + Int: Apron KEIN FIXPOINT PROB: goblint --html -o ../results/results_final_analysis_thesis/struct-integers-apron/  tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.aprondomain true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.apron true --set ana.structs.equations false --sets solver slr3

// GEHT Relational Int KEIN FIXPOINT PROB: Equation trier: PARAM: goblint --html -o ../results/results_final_analysis_thesis/int-equations-trier/  tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval false --set ana.int.trier true --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true
// GEHT FIXPOINT PROB Relational Int: Equation interval PARAM: goblint --html -o ../results/results_final_analysis_thesis/int-equations-interval/  tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true
// GEHT Relational Int KEIN FIXPOINT PROB: Apron PARAM: goblint --html -o ../results/results_final_analysis_thesis/int-apron/ tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.aprondomain true --set ana.int.queries true --sets solver slr3
// GEHT Relational Int: Poly PARAM KEIN FIXPOINT PROB: goblint --html -o ../results/results_final_analysis_thesis/int-poly/  tests/regression/21-relational/17-precisioncomparison.c --set ana.activated "['base','poly']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational false --set ana.int.queries true
typedef struct  {
    int i;
    int k;
} S;

S globalStruct1;
S globalStruct2;
int globalInt;

S return_struct(){
    int localInt1;
    localInt1 = 5;
    globalStruct2.k = globalStruct1.i;
    globalStruct2.i = globalStruct1.k;
    globalInt = 7;
    for(globalStruct1.i = 5; globalStruct1.i > 0; globalStruct1.i --){
        localInt1 = localInt1 - 1;
    }
    return globalStruct1;
}

int return_int(){
    int localInt2, localInt3;
    if (globalInt < 0) {
        localInt2 = 6;
        localInt3 = 5;
    } else {
        localInt2 = 5;
        localInt3 = 6;
    }
    return localInt2;
}

int main(){
    S localStruct;
    int localInt4;
    localStruct.i = 5;
    localStruct.k = 7;
    globalStruct1.i = 5 * 12;
    globalStruct1.k = 27;
    globalInt = 5;
    //assert (globalStruct1.i == 60);
    //assert (globalStruct1.k == 27);
    //assert (globalInt == 5);
    localStruct = return_struct();
    //assert (globalInt == 7);
    //assert (localStruct.i == 0);
    localInt4 = return_int();
    //assert (globalInt == 7);
    //assert (localInt4 == 5);
    //assert (globalStruct2.i == 27);
    //assert (globalStruct1.i == 0);
    return localStruct.i;
}

