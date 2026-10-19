// PARAM: --enable ana.float.interval
// A pointer to a malloc'd double or float keeps its target when it is
// converted to void * and back, so the object can be read through the
// converted pointer and freed through void *. Converting it on to a pointer
// to an integer type still gives the unknown pointer, so that a stored
// floating-point value is never read as an integer.
#include <stdlib.h>
#include <goblint.h>

int main(void) {
  double *d = malloc(sizeof(double));
  *d = 1.5;
  void *p = d;
  double *d2 = p;
  __goblint_check(*d2 > 1.0);
  long *l = p;
  __goblint_check(*l == 1); //UNKNOWN!
  free(p); //NOWARN

  float *f = malloc(sizeof(float));
  *f = 2.5f;
  void *q = f;
  float *f2 = q;
  __goblint_check(*f2 > 2.0f);
  free(f); //NOWARN
  return 0;
}
