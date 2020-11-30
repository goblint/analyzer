// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"
typedef struct  {
	int i;
} S;

void some_function(S* xx){
	(*xx).i = 42;
}

int main(){
	S ss;
	some_function(&ss);
	return ss.i; //NOWARN
}
