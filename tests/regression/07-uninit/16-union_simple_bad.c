// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"
typedef union {
	double i;
	int j;
} S;


int main(){
	S s;
	s.i = 0; // NOWARN
	return s.j; // WARN
}
