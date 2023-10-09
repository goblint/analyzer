extern void __goblint_check(int); // NOWARN
extern void __goblint_assume(int);
extern void __goblint_assert(int);      // NOWARN
extern void __goblint_unknown(void*);

#define check(x)   __goblint_check(x) // NOWARN
#define assume(x)  __goblint_assume(x)
#define assert(x)  __goblint_assert(x)  // NOWARN
#define unknown(x) __goblint_unknown(x)

int main(){
  int i, j, k, n;

  n=0;
  assert(n==0); // SUCCESS

  unknown(&i);
  assert(i==8); // UNKNOWN (refines)
  assert(i==8); // SUCCESS

  j=3;
  check(j==3); // assert

  unknown(&j);
  check(j==6); // assert UNKNOWN
  check(j==6); // assert UNKNOWN

  unknown(&k);
  assume(k==4);
  check(k==4);  // assert SUCCESS

  unknown(&k);
  assume(k+1==n);

  assume(n==5);  // contradiction
  assert(0);     // NOWARN (unreachable)

  return 0;
}