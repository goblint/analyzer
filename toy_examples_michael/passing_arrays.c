int main(void) {
  int i = 0;
  int a;
  int array1[10000000];

  int array2[10];
  array2[9] = 42;

  a = 5;
  a = 7;

  while(i < 10000000) {
    array1[i] = 42;
    i++;
    a=2;
  }

  a = 5;

  int x = array1[i] + 7;
  int y = array1[0];

  // getFirst(array2);
  // getFirst(array);
}
