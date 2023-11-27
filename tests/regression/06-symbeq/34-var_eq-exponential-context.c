// SKIP PARAM: --set ana.activated[+] var_eq

void level0(int *p) {

}

void level1(int *p, int *q) {
  level0(p);
  level0(q);
}

void level2(int *p, int *q) {
  level1(p, q);
  level1(q, p);
}

void level3(int *p, int *q) {
  level2(p, q);
  level2(q, p);
}

void level4(int *p, int *q) {
  level3(p, q);
  level3(q, p);
}

void level5(int *p, int *q) {
  level4(p, q);
  level4(q, p);
}

void level6(int *p, int *q) {
  level5(p, q);
  level5(q, p);
}

void level7(int *p, int *q) {
  level6(p, q);
  level6(q, p);
}

void level8(int *p, int *q) {
  level7(p, q);
  level7(q, p);
}

void level9(int *p, int *q) {
  level8(p, q);
  level8(q, p);
}


int main() {
  int i = 0;
  int j = 0;
  level9(&i, &j);
  return 0;
}