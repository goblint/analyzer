int main() {
  __asm__ ("nop");
  __goblint_check(1); // reachable, formerly TERM
  return (0);
}
