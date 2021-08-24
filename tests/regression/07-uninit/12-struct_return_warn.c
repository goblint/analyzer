// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"  --sets exp.privatization none
typedef struct  {
	int i, j;
} S;


S some_function(){
	S xx;
	xx.i = 42;
	return xx; //WARN
}

int main(){
	S ss;
	ss = some_function();
	return ss.j; //NOWARN
}
