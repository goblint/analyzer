// PARAM: --set ana.activated "['base','escape','uninit','mallocWrapper']"
typedef struct  {
	int i;
} S;

int main(){
	S ss;
	return ss.i; //WARN
}
