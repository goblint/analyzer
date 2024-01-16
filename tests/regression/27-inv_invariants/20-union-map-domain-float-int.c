// PARAM: --enable ana.float.interval --set ana.base.unions.domain map
#include <goblint.h>
union u {
	int x;
	float f;
};

int main(){
	union u a;
	union u b;

	a.x = 12;
	b.f = 129.0;

	int i = 0;
	if(a.x == b.x){
		i++;
	}
	// Should not be dead after if
	__goblint_check(1);

	if(a.f == b.f){
		i++;
	}
	// Should not be dead after if
	__goblint_check(1);
	return 0;
}
