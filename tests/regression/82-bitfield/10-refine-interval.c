// PARAM: --enable ana.int.interval --enable ana.int.bitfield --set ana.int.refinement fixpoint

int main() {
  unsigned char r; // non-neg rand
  char x = r % 64;

  if ((r | x) == 0) {
    __goblint_check(r == 0); // SUCCESS
    __goblint_check(x == 0); // SUCCESS
  }

  if ((r & x) == 63) {
    __goblint_check(r & 63 == 63); // SUCCESS
    __goblint_check(x == 63); // SUCCESS
  }

  if ((x ^ 3) == 5) {
    __goblint_check(x == 6); // SUCCESS
  }

}
