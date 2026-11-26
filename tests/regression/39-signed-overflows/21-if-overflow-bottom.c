// PARAM: --set sem.int.signed_overflow "assume_none"
// Reduced with creduce from sv-benchmarks/c/hardness/hardness_kloop_25_1loop_file-27.i
#include <goblint.h>

int main(void) {
  if (24007455 * 128 / 8 != 0); // WARN NOCRASH
  return 0;
}
