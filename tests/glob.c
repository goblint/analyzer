
int x = 0;

int main() {
  int y = 7;
  int z = 7;
  if (x) {
    x = 2;
    z = x;
  } else {
    x = 1;
  }
  z = 0;
  return 0;
}
