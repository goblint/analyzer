// SKIP PARAM: --set ana.activated[+] apron
// NOCHECK
void main(void) {
  int i = 0;
  int j = i;

  i++;
  j = i;

  int x = (int) j-1;
  int z = x +1;
}
