// PARAM: --set sem.int.signed_overflow "assume_none"
// Reduced with creduce from sv-benchmarks/c/hardness/hardness_kloop_25_1loop_file-27.i
int a;

int main(void) {
  if (24007455 * 128 / 8 != a); // NOCRASH
  return 0;
}
