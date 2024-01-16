//PARAM: --enable ana.arrayoob --enable ana.int.interval   --set ana.activated[+] apron   --set sem.int.signed_overflow assume_none 

int readUntil(char arr[], unsigned len) {
	for(unsigned int i=0;i < len;i++) {
			char s  = arr[i]; //NOWARN
	}
}

int main() {
	unsigned int len;
	unsigned int top;

	if(top) {
		len = 5;
	} else {
		len = 10;
	}

	char ptr[len];
	readUntil(ptr, len);
}
