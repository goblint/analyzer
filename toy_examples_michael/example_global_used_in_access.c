int global;

int main(void) {
  int a[10];

  a[global] = 4;

  for(int i=0; i <5; i++) {
    a[i] = 42;
  }
}