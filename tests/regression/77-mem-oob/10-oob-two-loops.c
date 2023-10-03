// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info --set sem.int.signed_overflow assume_none
int main() {
	int *p = malloc(1048 * sizeof(int));

    for (int i = 0; i < 1048; ++i) {
		p[i] = __VERIFIER_nondet_int(); //NOWARN
	}

	int *q = p;

	while (*q >= 0 && q < p + 1048 * sizeof(int)) { //WARN
		if (__VERIFIER_nondet_int()) {
			q++;
		} else {
			(*q)--; //WARN
		}
	}
	free(p);
	return 0;
}
