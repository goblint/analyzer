// PARAM: --set ana.activated[0][+] "'var_eq'" 
#include <stdio.h>

extern short * anShortPlease();
extern int * anIntPlease();
int main(){
	t1();
	t2();
	t3();
	t4();
	t5();
	t6();
	t7();
	t8();
	t9();
	t10();
	t11();
	t12();
	t13();
	t14();
	t15();
	t16();
	t17();
	return 0;
}

struct s {
	int    i;
	char *cp;
};

struct t {
	struct s * ss;
	int    o;
};

extern void f(int *);
extern struct s ** get_s();

int t17(){
	int i = i % 6;
	struct s ss[6], *ps;

	ps = &ss[i];
	assert(ps == &ss[i]);

	i = 2;
	assert(ps == &ss[i]); // UNKNOWN

	return 0;
}

int t16(){
	int i;
	struct t tt, *pt, tt2;
	struct s ss,ss2;

	pt->ss->i = i;
	assert(pt->ss->i == i);

	tt = tt2;
	assert(pt->ss->i == i); // UNKNOWN

	return 0;
}

int t15(){
	int i;
	struct t tt, *pt;
	struct s ss,ss2;

//	pt = &tt;
	tt.ss = &ss;

	pt->ss->i = i;
	assert(pt->ss->i == i);

	ss = ss2;
	assert(pt->ss->i == i); // UNKNOWN

	return 0;
}

int t14(){
	int i;
	struct t tt, *pt;
	struct s ss;

//	pt = &tt;
//	tt.ss = &ss;

	pt->ss->i = i;
	assert(pt->ss->i == i);
	ss.i = 1;
	assert(pt->ss->i == i); // UNKNOWN

	return 0;
}

int t13(){
	int i;
	struct t tt, *pt;
	struct s ss,ss2;

	pt = &tt;
	tt.ss = &ss;

	pt->ss->i = i;
	assert(pt->ss->i == i);
	ss = ss2;
	assert(pt->ss->i == i); // UNKNOWN

	return 0;
}

int t12(){
	int i;
	struct t tt, *pt;
	struct s ss;

	pt = &tt;
	tt.ss = &ss;

	pt->ss->i = i;
	assert(pt->ss->i == i);
	ss.i = 1;
	assert(pt->ss->i == i); // UNKNOWN

	return 0;
}

int t11(){
	int x, *q;
	char *y, *w;
	struct s z;
	struct s *a, *b;

	q = &a->i;
	y = a->cp;
	z.i = 8;
	*(get_s()) = a;

	assert(q == &a->i); // ???
	assert(y == a->cp); // UNKNOWN
	assert(z.i == 8);

	return 0;
}

int t10(){
	int x, *q;
	char *y, *w;
	struct s z;
	struct s *a, *b;

	q = &a->i;
	y = a->cp;
	z.i = 8;
	a = b;

	assert(q == &a->i); // UNKNOWN
	assert(y == a->cp); // UNKNOWN
	assert(z.i == 8);

	return 0;
}


int t9(){
	int x,q;
	char *y;
	struct s z;
	struct s *a, *b;

	q = &b->i;
	y = b->cp;
	z.i = 8;

	assert(q == &b->i);
//	assert(y == b->cp);

	return 0;
}

int t8(){
	int x;
	struct s z;
	struct s *a;

	a = &z;
	x = 8;
	assert(a == &z);

	return 0;
}

int t7(){
	int x, y;
	struct s z;
	struct s *a, *b;

	x = y;
	a = b;
	assert(x == y);

	return 0;
}


int t6(){
	int x, y;
	struct s z;
	struct s *a, *b;

	x = y;
	*a = *b;
	assert(x == y);

	return 0;
}


int t5(){
	int x, y, z;
	short *a = anShortPlease();

	x = y;
	*a = 3;
	assert(x == y); // UNKNOWN

	return 0;
}

int t4(){
	int x, y, z;
	int *a = anIntPlease();

	x = y;
	*a = 3;
	assert(x == y); // UNKNOWN
	assert(a == &z); // UNKNOWN

	return 0;
}

int t3(){
	int x, y, z;
	int *a;

	a = &z;

	x = y;
	*a = 3;
	assert(x == y);
	assert(a == &z);


	return 0;
}


int t2(){
	int x, y;
	int *a;

	a = &y;

	x = y;	assert(x == y);
	*a = 3;
	assert(x == y); // UNKNOWN
	assert(a == &y);

	return 0;
}

int t1(){
	int x, y;
	int *a;

	a = &y;

	x = y;
	assert(x == y);
	f(a);
	assert(x == y); // UNKNOWN
	assert(a == &y);

	return 0;
}
