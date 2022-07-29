// PARAM: --set ana.activated[+] uninit
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
