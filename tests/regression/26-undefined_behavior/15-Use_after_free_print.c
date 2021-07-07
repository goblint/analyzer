// PARAM:  --sets ana.spec.file useafterfree.spec --set ana.activated[+] "'spec'"
#include<stdio.h>
#include<stdlib.h>

int main() {
  int* a = malloc(10*sizeof(int));
  for(int i = 0; i < 10; i++) {
    a[i] = 0xff;
  }

  free(a);

  for(int i = 0; i < 10; i++) {
    printf("%d ", a[i]); // WARN
  }
  printf("\n");
  return 0;
}
