// PARAM: --enable ana.int.enums

int main() {
  int r; // rand
  int x = r & 1; // NOCRASH
  return 0;
}
