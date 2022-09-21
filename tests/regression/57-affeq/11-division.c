//SKIP PARAM: --set ana.activated[+] affeq   --set ana.apron.domain "affeq" --enable ana.int.interval
int main() {
  int two = 2;
  int three = 3;
  int six = 6;

  int x, y;
  if (x == three && y/x == two) {
    // y could for example also be 7
    assert(y == six);  // UNKNOWN!
  }
}