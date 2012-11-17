// PARAM: --set ana.activated "[['base','escape','uninit']]"
typedef struct  {
	int i;
} S;

typedef struct  {
	S   s;
	int j;
} T;

S ss;

int main(){
	T tt;
	tt.s = ss;
	tt.j = 0;
	return tt.s.i + tt.j; //NOWARN
}