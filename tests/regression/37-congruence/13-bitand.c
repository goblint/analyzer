// PARAM: --enable ana.int.congruence --set sem.int.signed_overflow assume_none
#include <goblint.h>

int main()
{
	// Assuming modulo expression

	unsigned long x;
	__goblint_assume(x % 2 == 1);
	__goblint_check(x % 2 == 1);
	__goblint_check(x & 1);

	long xx;
	__goblint_assume(xx % 2 == 1);
	__goblint_check(xx % 2 == 1); //TODO (Refine xx to be positive above, and reuse information in %)
	__goblint_check(xx & 1);

	long y;
	__goblint_assume(y % 2 == 0);
	__goblint_check(y % 2 == 0);
	__goblint_check(y & 1); //FAIL

	long z;
	__goblint_check(z & 1); //UNKNOWN!
	__goblint_assume(z % 8 == 1);
	__goblint_check(z & 1);

	long xz;
	__goblint_assume(xz % 3 == 1);
	__goblint_check(xz & 1); //UNKNOWN!
	__goblint_assume(xz % 6 == 1);
	__goblint_check(xz & 1);

	// Assuming bitwise expression
	// Does NOT actually lead to modulo information, as negative values may also have their last bit set!

	long a;
	__goblint_assume(a & 1);
	__goblint_check(a % 2 == 1); //UNKNOWN!
	__goblint_check(a & 1);

	int b;
	__goblint_assume(b & 1);
	__goblint_check(b % 2 == 1); //UNKNOWN!
	__goblint_check(b & 1);
}
