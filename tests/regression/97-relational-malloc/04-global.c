//PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra


#include <stdlib.h>
extern int __VERIFIER_nondet_int(void);

int test_fun(int a[], int N)
{
    int i;
    int res = 0;
    for (i = 0; i < N; i++) {
        while (a[i] > 0) { //NOWARN
            a[i]--; 
            res++;
        }
    }
    return res;
}

int main() {
  int array_size = __VERIFIER_nondet_int();
  if (array_size < 1 || array_size >= 2147483647 / sizeof(int)) {
     array_size = 1;
  }
  int* numbers = (int*) alloca(array_size * sizeof(int));
	
	for(int k = 0; k < array_size; k++)
	{
	  numbers[k] = __VERIFIER_nondet_int(); //NOWARN
	}
	
  test_fun(numbers, array_size);
}

