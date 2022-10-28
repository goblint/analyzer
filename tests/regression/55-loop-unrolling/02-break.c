// PARAM: --set exp.unrolling-factor 5
#include <goblint.h>

int main(void) {
	int r=5;
    for (int i = 0; i < 2; ++i) {
		switch (i) {
		case 0:
			break;
		case 1:
            r = 8;
		}
		r = 17;
		break;
	}

	__goblint_check(r==17);
	return 0;
}
