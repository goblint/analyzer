// PARAM: --set sem.int.signed_overflow "assume_none"
// Exercises a similar issue to 21-if-overflow-bottom.c in a different way
int a;
main() {
  __goblint_assume(24007455 * 128 / 8 != a); // NOCRASH
}

