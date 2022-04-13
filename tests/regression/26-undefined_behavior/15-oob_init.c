// PARAM: --enable ana.arrayoob

int a[1];

int main() {
  a[0] = 5; // NOWARN
  return 0;
}
