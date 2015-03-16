// PARAM: --set ana.activated "['base','escape','uninit']"
typedef union {
	struct {
		int a;
		int b;
	} i;
	struct {
		int c;
		int d;
	} j;
} S;


int main(){
	S s1,s2;
	s1.i.a = 0; // NOWARN
	s2.j.d = 0; // NOWARN
	return s1.j.c + s1.i.a + s2.i.b + s2.j.d; // NOWARN
}
