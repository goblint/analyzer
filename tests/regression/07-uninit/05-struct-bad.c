// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper','assert']"  --set ana.base.privatization none
typedef struct  {
	int i;
} S;

int main(){
	S ss;
	return ss.i; //WARN
}
