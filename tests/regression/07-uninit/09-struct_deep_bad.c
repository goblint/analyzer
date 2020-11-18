// PARAM: --set ana.activated "['base','escape','uninit','mallocWrapper']"
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
