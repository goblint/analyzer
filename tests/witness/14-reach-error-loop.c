void reach_error() {}

int main() {
  int x = 1;
  while( x < 10) {
    x = x + 2;
    if( x > 10) {
      reach_error();
    }
  }
  return 0;
}