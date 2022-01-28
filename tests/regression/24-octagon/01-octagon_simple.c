// SKIP PARAM: --set solver td3 --enable ana.int.interval  --set ana.base.arrays.domain partitioned  --set ana.activated "['base','threadid','threadflag','expRelation','apron','mallocWrapper']" --set ana.base.privatization none
// Example from https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf
void main(void) {
  int X = 0;
  int N = rand();
  if(N < 0) { N = 0; }

  while(X < N) {
    X++;
  }

  assert(X-N == 0);
  assert(X == N);

  if(X == N) {
    N = 8;
  } else {
    // is dead code but if that is detected or not depends on what we do in branch
    // currenlty we can't detect this
    N = 42;
  }
}
