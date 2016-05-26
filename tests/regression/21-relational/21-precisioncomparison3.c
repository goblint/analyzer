// GEHT Struct KEIN FIXPOINT PROB: Apron PARAM: goblint --html -o ../results/results_final_analysis_thesis/struct-apron/  tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational false --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.apron true --set ana.structs.equations false --sets solver slr3

// GEHT Struct KEIN FIXPOINT PROB: Equations trier PARAM: goblint --html -o ../results/results_final_analysis_thesis/struct-equations-trier/ tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base']" --set ana.int.interval false --set ana.int.trier true --set ana.int.relational false --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.equations true
// GEHT Struct FIXPOINT PROB: Equations interval PARAM: goblint --html -o ../results/results_final_analysis_thesis/struct-equations-interval/ tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.equations true

// GEHT Struct + Int: Equations interval + trier + relational int PARAM FIXPOINT PROB: goblint --html -o ../results/results_final_analysis_thesis/struct-integers-equations-interval-trier/  tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.equations true
// GEHT Struct + Int: Apron KEIN FIXPOINT PROB: goblint --html -o ../results/results_final_analysis_thesis/struct-integers-apron/  tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.aprondomain true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']" --set ana.structs.apron true --set ana.structs.equations false --sets solver slr3

// GEHT Relational Int KEIN FIXPOINT PROB: Equation trier: PARAM: goblint --html -o ../results/results_final_analysis_thesis/int-equations-trier/  tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base']" --set ana.int.interval false --set ana.int.trier true --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true
// GEHT FIXPOINT PROB Relational Int: Equation interval PARAM: goblint --html -o ../results/results_final_analysis_thesis/int-equations-interval/  tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --sets solver slr3
// GEHT Relational Int KEIN FIXPOINT PROB: Apron PARAM: goblint --html -o ../results/results_final_analysis_thesis/int-apron/ tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.aprondomain true --set ana.int.queries true --sets solver slr3
// GEHT Relational Int: Poly PARAM KEIN FIXPOINT PROB: goblint --html -o ../results/results_final_analysis_thesis/int-poly/  tests/regression/21-relational/21-precisioncomparison3.c --set ana.activated "['base','poly']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational false --set ana.int.queries true

// BASE INTERVAL: goblint --html -o ../results/results_final_analysis_thesis/base-non-relational-interval/  tests/regression/21-relational/21-precisioncomparison3.c --set ana.int.interval true --set ana.int.trier false
// BASE TRIER: goblint --html -o ../results/results_final_analysis_thesis/base-non-relational-trier/  tests/regression/21-relational/21-precisioncomparison3.c

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
    for(localInt1 = 5; localInt1 > 0; localInt1 --){
        globalStruct.i = globalStruct.i - 1;
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
    localStruct = return_struct();
    localInt5 = return_int();
    return localStruct.i;
}

