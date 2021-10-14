// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"  --set exp.privatization none
typedef struct  {
	int i;
} S;

typedef struct  {
	S   s;
	int j;
} T;


int main(){
	T tt;
	return tt.s.i + tt.j; //WARN
}
