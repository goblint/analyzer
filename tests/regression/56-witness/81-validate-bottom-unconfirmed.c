// PARAM: --set sem.int.signed_overflow assume_none --set witness.yaml.validate 81-validate-bottom-unconfirmed.yml
// The invariant (char)(x + 1) is false: (char)(INT_MAX + 1) is 0.
// Evaluating it overflows under assume_none, so it evaluates to bottom, although the state is reachable.
// Bottom used to be taken as dead code, which confirmed the invariant.
#include <limits.h>

int main() {
  int x = INT_MAX;
  ; // UNKNOWN (witness)
  return 0;
}
