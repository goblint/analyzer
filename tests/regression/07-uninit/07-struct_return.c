// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"  --set ana.base.privatization none
typedef struct  {
	int i;
} S;

S some_function(){
	S xx;
	xx.i = 42;
	return xx;
}

int main(){
	S ss;
	ss = some_function();
	return ss.i; //NOWARN
}
