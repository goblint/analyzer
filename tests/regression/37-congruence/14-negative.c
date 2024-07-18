// PARAM: --enable ana.int.congruence --set sem.int.signed_overflow assume_none
#include <goblint.h>

int main()
{
	int top;

	int c = -5;
	if (top)
	{
		c = -7;
	}
	__goblint_check(c % 2 == 1); //UNKNOWN! (Does not hold at runtime)
	__goblint_check(c % 2 == -1); //TODO (Track information that c is negative)
}
