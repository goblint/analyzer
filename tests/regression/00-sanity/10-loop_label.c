int main () {
  while (1) {
    while_1_continue: /* CIL label */ ;
  }
  __goblint_check(0); // NOWARN (unreachable), formerly NONTERM
  return 0;
}
