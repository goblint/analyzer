// just a few sanity checks on checks

int main() {
  int success = 1;
  int silence = 1;
  int fail = 0;
  int unknown;
  __goblint_check(success);
  __goblint_check(fail); // FAIL!
  __goblint_check(unknown == 4); // UNKNOWN!
  return 0;
  __goblint_check(silence); // NOWARN!
}
