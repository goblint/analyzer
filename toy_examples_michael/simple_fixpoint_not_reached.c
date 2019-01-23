int main(void) {
  int array1[10000000];
  int* ptr = &array1;

  *ptr = 5;
  ptr++;
  *ptr = 5;


  int val = *ptr;
  
  int b = 8;
  array1[7] = 17;
}