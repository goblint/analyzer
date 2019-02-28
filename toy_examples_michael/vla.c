// This is an example for VLAs, CIL turns them into alloca so our domain is not
// used here. 
int main(void) {
  int top;
  int l = 5;

  if(top) {
    l = 6;
  }

  int a[l];

  for(int i=0; i < l-1; i++) {
    a[i] = 42;
  }
}