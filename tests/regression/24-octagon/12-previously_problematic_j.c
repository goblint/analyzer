// PARAM: --sets solver td3 --disable exp.fast_global_inits --set ana.activated "['base','baseflag','octagon']"
void main(void) {
  int i = 0;
  int j = i;

  i++;
  j = i;

  int x = (int) j-1;
  int z = x +1;
}
