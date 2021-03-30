// PARAM: --set octagon_vars "['t__11','t__12','a','b','#ret']"

typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int a, b;
	a = __VERIFIER_nondet_int();
	b = __VERIFIER_nondet_int();
	while (a < 30) {
		while (b < a) {
			if (b > 5) {
				b = b + 7;
			} else {
				b = b + 2;
            }
			if (b >= 10 && b <= 12) {
				a = a + 10;
			} else {
				a = a + 1;
            }
		}
		a = a + 2;
		b = b - 10;
	}
	return 0;
}