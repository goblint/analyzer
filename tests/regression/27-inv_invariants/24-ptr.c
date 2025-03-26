// PARAM: --enable ana.int.interval

int two = 2;
int three = 3;

struct s { int content; };
struct s twostruct = {2};
struct s threestruct = {3};

int main() {
	int* ptr;
	struct s* ptrstruct;
	int top;

	if(top) {
		ptr = &two;
		ptrstruct = &twostruct;
	} else {
		ptr = &three;
		ptrstruct = &threestruct;
	}

	if(*ptr == 2) {
		__goblint_check(ptr == &two);
	}

	if(ptrstruct->content == 2) {
		__goblint_check(ptrstruct == &twostruct);
	}

  	return 0;
}
