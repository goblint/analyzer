extern void __goblint_check(int);
extern void __goblint_commit(int);
extern void __goblint_assert(int);      // NOWARN
extern void __goblint_unknown(void*);

#define check(x)   __goblint_check(x)
#define commit(x)  __goblint_commit(x)
#define assert(x)  __goblint_assert(x)  // NOWARN
#define unknown(x) __goblint_unknown(x)

int main(){
  int i, j, k, n;
  
  n=0;
  assert(n==0); // SUCCESS
  
  unknown(&i);   
  assert(i==8); // UNKNOWN 
  assert(i==8); // SUCCESS
  
  j=3;
  check(j==3); // assert

  unknown(&j);   
  check(j==6); // assert UNKNOWN
  check(j==6); // assert UNKNOWN
  
  unknown(&k);
  commit(k==4); // assert SUCCESS
  check(k==4);  // assert SUCCESS
  
  unknown(&k);
  commit(k+1==n); // FAIL
  
  commit(n==5);  // NOWARN
  assert(0);     // NOWARN
  
  return 0;
}