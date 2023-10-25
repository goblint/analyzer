//PARAM: --set ana.activated[+] useAfterFree --enable ana.int.interval
#include <stdlib.h>

void double_free_001()
{
	char* ptr= (char*) malloc(sizeof(char));

	free(ptr); //NOWARN (Double Free (CWE-415))
}

void double_free_002()
{
	char* ptr= (char*) malloc(10*sizeof(char));
	int i;
	
	for(i=0;i<10;i++)
	{
		ptr[i]='a';
		if(i==10)
		free(ptr);
	}
	free(ptr); //NOWARN (Double Free (CWE-415))
}

void double_free_003()
{
	char* ptr= (char*) malloc(10*sizeof(char));
	int i;
	
	for(i=0;i<10;i++)
	{
		*(ptr+i)='a';
		
	}

	free(ptr); //NOWARN (Double Free (CWE-415))
}

void double_free_004()
{
	char* ptr= (char*) malloc(10*sizeof(char));
	int i;
	for(i=0;i<10;i++)
	{
		*(ptr+i)='a';

	}
	free(ptr); //NOWARN (Double Free (CWE-415))
}

void double_free_005()
{
	char* ptr= (char*) malloc(sizeof(char));

	if(ptr)
	free(ptr); //NOWARN (Double Free (CWE-415))
}

void double_free_006()
{
	char* ptr= (char*) malloc(sizeof(char));
	if(0)
	free(ptr);

	free(ptr); //NOWARN (Double Free (CWE-415))
}

void double_free_007()
{
	char* ptr= (char*) malloc(sizeof(char));
	int flag=0;
	
	if(flag<0)
	free(ptr);

	free(ptr); //NOWARN (Double Free (CWE-415))
}

char *double_free_function_008_gbl_ptr;

void double_free_function_008()
{
	free (double_free_function_008_gbl_ptr); //NOWARN (Double Free (CWE-415))
}

void double_free_008()
{
	double_free_function_008_gbl_ptr= (char*) malloc(sizeof(char));

	double_free_function_008();
}

void double_free_009()
{
	char* ptr= (char*) malloc(sizeof(char));
	int flag=0;

	while(flag==1)
	{
		free(ptr);
		flag++;
	}
	free(ptr); //NOWARN (Double Free (CWE-415))
}

void double_free_010()
{
	char* ptr= (char*) malloc(sizeof(char));
	int flag=1;

	while(flag)
	{
		// We're currently too unprecise to properly detect this below (due to the loop)
		free(ptr); // (Double Free (CWE-415))
		flag--;
	}
}

void double_free_011()
{
	char* ptr= (char*) malloc(sizeof(char));
	int flag=1,a=0,b=1;

	while(a<b)
	{
		if(flag ==1)
		// We're currently too unprecise to properly detect this below (due to the loop)
		free(ptr);  // (Double Free (CWE-415))
		a++;
	}
}

void double_free_012()
{
	char* ptr= (char*) malloc(sizeof(char));
	int a=0;

	for(a=0;a<1;a++)
	{
		// We're currently too unprecise to properly detect this below (due to the loop)
		free(ptr); // (Double Free (CWE-415))
	}
}

extern volatile int vflag;

int main ()
{
	if (vflag == 1 || vflag ==888)
	{
		double_free_001 ();
	}

    if (vflag == 2 || vflag ==888)
    {
    	double_free_002 ();
    }

    if (vflag == 3 || vflag ==888)
    {
    	double_free_003 ();
    }

    if (vflag == 4 || vflag ==888)
    {
    	double_free_004 ();
    }

    if (vflag == 5 || vflag ==888)
    {
    	double_free_005 ();
    }

    if (vflag == 6 || vflag ==888)
    {
    	double_free_006 ();
    }

    if (vflag == 7 || vflag ==888)
    {
    	double_free_007 ();
    }

    if (vflag == 8 || vflag ==888)
    {
    	double_free_008 ();
    }

    if (vflag == 9 || vflag ==888)
    {
    	double_free_009 ();
    }

    if (vflag == 10 || vflag ==888)
    {
    	double_free_010 ();
    }

    if (vflag == 11 || vflag ==888)
    {
    	double_free_011 ();
    }

    if (vflag == 12 || vflag ==888)
    {
    	double_free_012 ();
    }

}
