// PARAM: --set ana.activated[+] uninit --disable asm_is_nop

int main() {
  int x, y, z;
  asm ("nop" : "=x" (y) : "x" (x)); // WARN
  z = y + 1; // NOWARN
  return 0;
}
