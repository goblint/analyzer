// PARAM: --enable ana.int.congruence
#include <goblint.h>
// #include <assert.h>
// #define __goblint_check(e) assert(e)

int basic(){
	int two_pow_16 = 65536;
	int a;
	int b;

	if (a % two_pow_16 == 3)
	{
		if (b % two_pow_16 == 5)
		{
			__goblint_check(a % two_pow_16 == 3);
			__goblint_check(b % two_pow_16 == 5);

			unsigned int e = a * b;
			__goblint_check(e % two_pow_16 == 15); // UNKNOWN!
			__goblint_check(e == 15); // UNKNOWN!

			// Congruence lost by cast to signed int
			int e_sign = e;
			__goblint_check(e_sign % two_pow_16 == 15); //UNKNOWN!
			__goblint_check(e_sign == 15); // UNKNOWN!
		}
	}
}

int special(){
	int two_pow_18 = 262144;
	int two_pow_17 = 131072;

	int a;

	if (a % two_pow_18 == two_pow_17)
	{
		__goblint_check(a % two_pow_18 == two_pow_17);

		unsigned int e = a * a;
		__goblint_check(e == 0); //UNKNOWN!
	}
}


int special2(){
	int two_pow_30 = 1073741824;
	int two_pow_18 = 262144;
	int two_pow_17 = 131072;
	int two_pow_15 = 32768;

	int x = two_pow_17 + two_pow_15;

	int a;
	int b;

	if (a % two_pow_18 == x)
	{
		__goblint_check(a % two_pow_18 == x); //TODO

		unsigned int e = a * a;
		__goblint_check(e == two_pow_30); //UNKNOWN!
	}
}

int main()
{
	basic();
	special();
	special2();
}
