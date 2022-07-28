// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','apron','mallocWrapper','assert']" --set ana.base.privatization none
void main(void) {
  int i = 0;
  int j = i;

  i++;
  j = i;

  int x = (int) j-1;
  int z = x +1;
}
