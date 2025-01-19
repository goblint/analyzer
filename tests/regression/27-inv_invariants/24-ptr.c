// PARAM: --enable ana.int.interval

int two = 2;
int three = 3;

int main() {
	int* ptr;
	int top;

	if(top) {
		ptr = &two;
	} else {
		ptr = &three;
	}

	if(*ptr == 2) {
		__goblint_check(ptr == &two);
	}

  	return 0;
}
