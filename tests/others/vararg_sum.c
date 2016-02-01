#include <stdio.h>
#include <stdarg.h>


int sum(int n, ...){
	printf("n: %d\n", n);
	va_list args;
	va_start(args, n);
	int x, sum = 0;
	for(int i = 0; i < n; i++){
		x = va_arg(args, int);
		sum += x;
		printf("x: %d, sum: %d\n", x, sum);
	}
	return sum;
}

int main(){
	sum(1, 2);
	sum(3, 1, 2, 3);
	return 0;
}
