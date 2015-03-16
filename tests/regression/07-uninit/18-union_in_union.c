// PARAM: --set ana.activated "['base','escape','uninit']" 
typedef union {
	union {
		struct {
			int a;
			int b;
		} i;
		struct {
			int c;
			int d;
			double w;
		} j;
	} s;
} S;


int main(){
	S ss;
	int q = 0;

	ss.s.i.a = 0;
	q = ss.s.i.a; // NOWARN
	q = ss.s.i.b; // WARN
	q = ss.s.j.c; // NOWARN
	q = ss.s.j.d; // WARN
	q = ss.s.j.w; // WARN


	ss.s.j.d = 0;
	q = ss.s.i.a; // NOWARN
	q = ss.s.i.b; // NOWARN
	q = ss.s.j.c; // NOWARN
	q = ss.s.j.d; // NOWARN
	q = ss.s.j.w; // WARN

	return 0;
}
