// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"  --set exp.privatization none
typedef struct  {
	int i;
} S;

int main(){
	S ss;
	ss.i = 0;
	return ss.i; //NOWARN
}
