// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit']"
typedef struct  {
	int i,j;
} S;


int some_function(S xx){
	return xx.j; //NOWARN
}

int main(){
	S ss;
	some_function(ss); //WARN
	return 0;
}
