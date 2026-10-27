// SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none
// F passes the address of its local x to its recursive call, so the callee's
// relation keeps the caller activation's locals and formals, among them a == 0
// and b == 5. body then assigns the formals a and b from their arguments while
// they carry those constraints. assign_var_parallel must replace them, not
// combine them with the new values into a contradiction. A contradiction
// there makes the relation bot (), whose environment is empty; y = 0 then
// adds only y, and forgetting rand()'s result raises "Environment.dim_of_var:
// unknown variable in the environment".
#include <goblint.h>
#include <stdlib.h>

void F(int a, int b, int *p) {
  int y = 0;
  int w = rand() % 100;
  if (p == NULL) {
    int x = 0;
    F(w, w + 1, &x);
  } else {
    *p = 1;
    __goblint_check(b == a + 1);
  }
}

int main(void) {
  F(0, 5, NULL);
  return 0;
}
