// PARAM: --enable ana.int.interval

int two = 2;
int three = 5;

int main() {
	int* ptr;
	int top;

	int indicator = 1;

	if(top) {
		ptr = &two;
	} else {
		ptr = &three;
	}

	// Evaluating this in the interval domain yields [2,5]
	// Only trying to optimize per-pointer discovers that this in fact dead code (https://github.com/goblint/analyzer/pull/1659)
	if(*ptr == 3) {
		// Dead
		indicator = 0;
	}

	__goblint_check(indicator == 1);
  	return 0;
}
