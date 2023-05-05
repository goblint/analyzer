//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include <stdlib.h>
#include <goblint.h>

int g = 0;

void write_into_array(int *arr[], int size){
	arr[size - 1] = &g;
}

void test_array(){
	int size = 10;
	int j;
	int *a[size];

	for(int i = 0; i < size; i++){
		a[i] = NULL;
	}

	__goblint_check(a[0] == NULL);
	__goblint_check(a[size - 1] == NULL);
	__goblint_check(a[size - 1] != &j);

	write_into_array(a, size);

	__goblint_check(a[size - 1] != NULL); //UNKNOWN
	__goblint_check(a[size - 1] == &g); //UNKNOWN

	// Requires not having `Top as the value a.
	__goblint_check(a[size - 1] != &j);

}

int main(){
	test_array();
	return 0;
}