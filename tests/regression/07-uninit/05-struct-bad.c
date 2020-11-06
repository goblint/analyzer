// PARAM: --set ana.activated "['base','baseflag','escape','uninit']"
typedef struct  {
	int i;
} S;

int main(){
	S ss;
	return ss.i; //WARN
}
