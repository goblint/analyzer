// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation','octagon']"
// Example from https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf
void main(void) {
  int X = 0;
  int N = rand();
  if(N < 0) { N = 0; }

  while(X < N) {
    X++;
  }

  assert(X-N == 0);
  // assert(X == N); // Currently not able to assert this because octagon doesn't handle it

  if(X == N) {
    N = 8;
  } else {
    // is dead code but if that is detected or not depends on what we do in branch
    // currenlty we can't detect this
    N = 42;
  }
}
