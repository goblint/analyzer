//PARAM: --enable ana.int.congruence --disable ana.int.def_exc --disable ana.int.enums

int main() {
	int a = 1;
	int b = 2;
	int c = 3;
	int d = 4;
	int e = 0;

	while (d < 9) {
		b = 2 * a;
		d = d + 4;
		e = e - 4 * a;

		a = b - a;
		c = e + d;
	}

	a = d / 2;
	b = d % 2;

	assert (c == 4); // UNKNOWN!
	assert (d == 12); // UNKNOWN
	assert (a == 6); // UNKNOWN
	assert (b == 0);

	return 0;
}
