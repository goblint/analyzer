// PARAM: --enable ana.int.congruence
#include <goblint.h>
int main()
{
	unsigned int a = 0;
	unsigned int b = 16;
	while (1)
	{
		a = a + b;
		b = b + b;
		__goblint_check(a % 16 == 0);
	}
	return 0;
}
