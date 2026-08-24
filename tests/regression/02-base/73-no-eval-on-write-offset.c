// PARAM: --enable exp.earlyglobs
// NOCRASH
char a;
int c;

int main() {
  *(&c + 0) = a;
  return 0;
}
