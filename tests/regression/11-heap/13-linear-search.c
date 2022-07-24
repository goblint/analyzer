// PARAM: --set ana.malloc.unique_address_count 3  --enable ana.sv-comp.functions
extern void *calloc(unsigned int num, unsigned int size);

void __VERIFIER_as(int cond) {
  if (!(cond)) {
    assert(1); // reachable
  }
  return;
}
unsigned int __VERIFIER_nondet_uint();
unsigned int  SIZE;
const unsigned int MAX = 100000;
int linear_search(int *a, int n, int q) {
  unsigned int j=0;
  while (j<n && a[j]!=q) {
  j++;
  if (j==20) j=-1;
  }
  if (j<SIZE) return 1;
  else return 0;
}
int main() {
  SIZE=(__VERIFIER_nondet_uint()/2)+1;

  if (SIZE > 1 && SIZE < MAX) {
    int *a = calloc(SIZE,sizeof(int));
    a[SIZE/2]=3;
    __VERIFIER_as(linear_search(a,SIZE,3));
  }
}
