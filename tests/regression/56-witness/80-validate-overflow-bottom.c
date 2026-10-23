// PARAM: --set sem.int.signed_overflow assume_none --set witness.yaml.validate 80-validate-overflow-bottom.yml
// The invariant (x + 1) && 0 is false. Evaluating it overflows under assume_none, so x + 1 evaluates to bottom.
// This bottom comes from an operation in an expression constructed by the witness, not from contradicting states of the analyses.
// Treating it as dead code would wrongly confirm the invariant.
#include <limits.h>

int main() {
  int x = INT_MAX;
  ; // FAIL (witness)
  return 0;
}
