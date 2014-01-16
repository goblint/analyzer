#include <stdlib.h>
#include <stdio.h>

int main(){
	int *ip;
	//*ip = 5; // segfault
	//printf("%i", *ip); // segfault
	ip = malloc(sizeof(int)); // assume malloc never fails

	// do stuff
	//*ip = 5;

	free(ip);
	//free(ip); // crash: double free or corruption
	*ip = 5; // undefined but no crash
	printf("%i", *ip); // undefined but printed 5
	ip = NULL; // make sure the pointer is not used anymore
	*ip = 5; // segfault
}
