// PARAM: --enable ana.int.interval

int main() {
  unsigned int r; // non-neg rand
  int x = r % 100;

  // exclude lower bound
  if (x != 0)
    __goblint_check(x >= 1);

  if (x == 0) {}
  else
    __goblint_check(x >= 1);

  // exclude upper bound
  if (x != 99)
    __goblint_check(x <= 98);

  if (x == 99) {}
  else
    __goblint_check(x <= 98);
}
