// Code from https://github.com/goblint/cil/pull/98

int main() {
  // for loop
  int x = 42;
  for (x = 0; x < 10; x++) { // there shouldn't be invariants x <= 9, x <= 10 and 0 <= x before this line
    // ...
  }

  // expression with side effect
  int i, k;
  i = k = 0; // there shouldn't be invariant k == 0 before this line

  // compound initializers
  struct kala {
    int kaal;
    int hind;
  };

  struct kala a = {2, 3}; // there shouldn't be invariant a.kaal == 2 before this line
  return 0;
}
