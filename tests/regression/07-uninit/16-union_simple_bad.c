// PARAM: --set ana.activated "['base','escape','uninit']"
typedef union {
	double i;
	int j;
} S;


int main(){
	S s;
	s.i = 0; // NOWARN
	return s.j; // WARN
}
