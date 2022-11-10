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

			int e = a * b;
			__goblint_check(e % two_pow_16 == 15);
			__goblint_check(e == 15); // UNKNOWN!
		}
	}
}

int special(){
	unsigned int two_pow_18 = 262144;
	unsigned int two_pow_17 = 131072;

	unsigned int a;
	unsigned int b;

	if (a % two_pow_18 == two_pow_17)
	{
		if (b % two_pow_18 == two_pow_17)
		{
			__goblint_check(a % two_pow_18 == two_pow_17);
			__goblint_check(b % two_pow_18 == two_pow_17);

			int e = a * b;
			__goblint_check(e == 0);
		}
	}

	// Hint why the above works:
	__goblint_check( two_pow_17 * two_pow_17 == 0);
}


int special2(){
	unsigned int two_pow_30 = 1073741824;
	unsigned int two_pow_18 = 262144;
	unsigned int two_pow_17 = 131072;
	unsigned int two_pow_16 = 65536;
	unsigned int two_pow_15 = 32768;

	unsigned int x = two_pow_17 + two_pow_15;

	unsigned int a;
	unsigned int b;

	if (a % two_pow_18 == x)
	{
		if (b % two_pow_18 == x)
		{
			__goblint_check(a % two_pow_18 == x);
			__goblint_check(b % two_pow_18 == x);

			int e = a * b;
			__goblint_check(e == two_pow_30);
		}
	}

	// Hint why the above holds:
	__goblint_check( x * x  == two_pow_30);
}




int main()
{
	basic();
	special();
	special2();
}
