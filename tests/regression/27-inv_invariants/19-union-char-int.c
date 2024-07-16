// PARAM: --enable ana.float.interval
#include <goblint.h>
union u {
	int x;
	char c;
};

int main(){
	union u a;
	union u b;

	a.x = 12;
	b.c = 12;

	int i = 0;
	if(a.x == b.x){
		i++;
	}
	// Should not be dead after if
	__goblint_check(1);

	a.x = 257;
	b.c = 1;

	if(a.x == b.x){
		i++;
	}
	// Should not be dead after if
	__goblint_check(1);

	if(a.c == b.c){
		i++;
	}
	// Should not be dead after if
	__goblint_check(1);

	return 0;
}
