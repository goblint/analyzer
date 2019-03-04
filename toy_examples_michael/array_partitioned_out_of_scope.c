// Example to investigate what we need to do when a variable contained in an expression
// used to partition goes out of scope

int main(void) {
  int a[20];

  init_array(a);

  int x = a[5];


  int b[20];

  for(int i=0;i < 10;i++) {
    b[i] = 5;
  }

  do_first(b);

  return 0;
}

void do_first(int* a) {
  int x = a[0];
}

void init_array(int* a) {
  for(int i =0; i < 20; i++) {
    a[i] = 42;
  }
}