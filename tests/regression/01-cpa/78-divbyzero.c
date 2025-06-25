// PARAM: --enable ana.int.interval

int main(void) {

	int a = 0;
	for (int i = 0; i < 5; i++) {
		a += 100/i; //WARN
	}
	a = 5;
	a /= 0; //FAIL

	return 0;
}
