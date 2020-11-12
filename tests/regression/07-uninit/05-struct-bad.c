// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit']"
typedef struct  {
	int i;
} S;

int main(){
	S ss;
	return ss.i; //WARN
}
