// PARAM: --set ana.int.refinement fixpoint --enable ana.int.def_exc --enable ana.int.enums  --enable ana.int.interval --set sem.int.signed_overflow assume_none
// NOTIMEOUT: Used to not reach terminate (https://github.com/goblint/analyzer/issues/1671) and (https://github.com/goblint/analyzer/issues/1673)
int main() {
   int count = 0;
   while (1) {
     count++;
     count++;
   }
 }
