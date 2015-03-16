// PARAM: --set ana.activated "['base','escape','uninit']"
typedef union {
	struct {
		short a;
		int b;
	} i;
	struct {
		int c;
		int d;
	} j;
} S;


int main(){
	S s;
	int i = 0;

	s.i.b = 0; // NOWARN
	i = s.i.b; // NOWARN
	i = s.i.a; // WARN
	i = s.j.c; // WARN
	i = s.j.d; // WARN

	return 0;
}
