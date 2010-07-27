// SKIP PARAM: --analysis containment
#include <stdio.h>
#include <stdlib.h>


typedef struct
{
	int data1;
	int*data2;
} DS;

extern int ext(int* i);
extern int ext2(int i);

DS* DS_member_const(int* ext_state)
{
	DS* pDS=(DS*)malloc(sizeof(DS));
	// warn here??:
	pDS->data2=ext;
}

int DS_member_2(DS* this,int* x)
{
	this->data1=*x; // NOWARN
}

int DS_member_1(DS* this,int x,int* y)
{
//no static data:
	static int i; // WARN

//don't call funcs that do not belong to the class itself with addresses
	ext(&x); // NOWARN
	ext(y); // NOWARN
	ext(&this->data1); // WARN

	ext2(this->data1); // NOWARN

	DS_member2(this,&this->data1); //NOWARN

//don't write to anything except this
	*y=x; // WARN
	*y=this->data1; // WARN

	this->data1=x; // NOWARN
	this->data1=*y; // NOWARN


//don't return addresses
	if(this->data1==1)
		return (int)&x; // WARN

	if(this->data1==2)
		return (int)x; // NOWARN

	if(this->data1==3)
		return (int)this->data1; // NOWARN

	if(this->data1==4)
		return (int)this; // WARN

//don't use external state:
	int tmp=*(this->data2); // WARN

	return tmp;
}

int main() {
	int i=1;
	DS* pDS=DS_member_const(&i);

	int res=DS_member_1(pDS,10,&i);

  return 0;
}
