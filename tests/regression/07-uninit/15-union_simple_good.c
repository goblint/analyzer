// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper','assert']"  --set ana.base.privatization none
typedef union {
	int i;
	int j;
} S;


int main(){
	S s;
	s.i = 0; // NOWARN
	return s.j; // NOWARN
}
