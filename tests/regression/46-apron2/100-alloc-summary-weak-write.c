// PARAM: --set ana.activated[+] apron

// Two blocks from one `malloc` site are live at once, so one `mallocWrapper`
// variable stands for both and `Queries.IsMultiple` holds of it. A write
// through a pointer into such a block joins with the constraints the variable
// already carries instead of replacing them. `*a == 3` holds in every
// execution that reaches it, and a strong relational update reports it as
// impossible. `assign_to_global_wrapper` reaches the same decision through its
// `#out` branch here, since an allocation block has `vglob` set.

#include <goblint.h>
#include <stdlib.h>

// Goblint's default `sem.malloc.fail` models `malloc` as always succeeding,
// so `p` is never null here.
static int *make(int v) {
  int *p = malloc(sizeof(int));
  *p = v;
  return p;
}

int main(void) {
  int *a = make(3);
  int *b = make(7);
  __goblint_check(*a == 3); // UNKNOWN (one variable per allocation site carries both blocks)
  __goblint_check(*b == 7); // UNKNOWN (the same variable)
  return 0;
}
