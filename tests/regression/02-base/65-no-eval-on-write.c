//PARAM: --enable ana.int.interval --enable exp.earlyglobs --enable ana.int.enums

// Test case that shows how avoiding reading integral globals can reduce the number of solver evaluations.
// Avoiding to evaluate integral globals when setting them reduced the number of necessary evaluations from 25 to 12 in this test case.
int glob = 10;

void foo() {
  glob = 3;
  glob = 4;
  glob = 1;
}

void bar() {
  glob = 2;
}

int main() {
  foo();
  bar();
  return 0;
}
