// PARAM: --enable ana.int.congruence
#include <goblint.h>
// #include <assert.h>
// #define __goblint_check(e) assert(e)

int basic(){
	unsigned int two_pow_16 = 65536;
	unsigned int a;
	unsigned int b;

	if (a % two_pow_16 == 3)
	{
		if (b % two_pow_16 == 5)
		{
			__goblint_check(a % two_pow_16 == 3);
			__goblint_check(b % two_pow_16 == 5);

			unsigned int e = a * b;
			__goblint_check(e % two_pow_16 == 15);
			__goblint_check(e == 15); // UNKNOWN!

			// Congruence lost by cast to signed int
			int e_sign = e;
			__goblint_check(e_sign % two_pow_16 == 15); //UNKNOWN!
			__goblint_check(e_sign == 15); // UNKNOWN!
		}
	}
}

int constant_result(){
	unsigned int two_pow_18 = 262144;
	unsigned int two_pow_17 = 131072;

	unsigned int a;

	if (a % two_pow_18 == two_pow_17)
	{
		__goblint_check(a % two_pow_18 == two_pow_17);

		// There can only be one result in Z/2^32
		unsigned int e = a * a;
		__goblint_check(e == 0);
	}

	// Hint why the above works:
	__goblint_check( two_pow_17 * two_pow_17 == 0);
}


int constant_result2(){
	unsigned int two_pow_30 = 1073741824;
	unsigned int two_pow_18 = 262144;
	unsigned int two_pow_17 = 131072;
	unsigned int two_pow_15 = 32768;

	unsigned int x = two_pow_17 + two_pow_15;

	unsigned int a;

	if (a % two_pow_18 == x)
	{
		__goblint_check(a % two_pow_18 == x);

		// There can only be one result in Z/2^32
		unsigned int e = a * a;
		__goblint_check(e == two_pow_30);
	}

	// Hint why the above holds:
	__goblint_check( x * x  == two_pow_30);
}

int main()
{
	basic();
	constant_result();
	constant_result2();
}
