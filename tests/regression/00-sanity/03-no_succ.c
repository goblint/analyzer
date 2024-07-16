int main() {
  __goblint_check(1); // reachable, formerly TERM
  return 0;
}

void f() {
  return;

  switch (1) {
    case 1:;
  }
}
