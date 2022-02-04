// PARAM: --enable exp.earlyglobs
char a;
int c;

int main() {
  *(&c + 0) = a;
  return 0;
}
