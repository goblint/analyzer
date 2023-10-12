// PARAM: --set ana.activated[+] "localTraces"
// --set witness.violation.unreach-functions [\"reach_error\",\"reach_error_2\"]

void reach_error() {}
void reach_error_2() {}

int main() {
  int x = 1;
  if (x <= 1) reach_error_2();
  else reach_error();
}