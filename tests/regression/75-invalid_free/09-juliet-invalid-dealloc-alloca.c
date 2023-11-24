#include <stdlib.h>
#include <alloca.h>

typedef struct twoIntsStruct {
   int intOne ;
   int intTwo ;
} twoIntsStruct;

void CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54_bad(void) {
    twoIntsStruct *data;
    data = (twoIntsStruct *)0;
    {
        twoIntsStruct *dataBuffer = __builtin_alloca(800UL);
        {
            size_t i;
            i = 0UL;
            
            goto ldv_3204;
            ldv_3203: 
            ;
            
            (dataBuffer + i)->intOne = 1;
            (dataBuffer + i)->intTwo = 1;
            
            i += 1UL;
            ldv_3204: 
            ;
            
            if (i <= 99UL) 
                goto ldv_3203;
            else
                goto ldv_3205;
            ldv_3205: 
            ;
        }

        data = dataBuffer;
    }

    CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54b_badSink(data);
    return;
}

void CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54b_badSink(twoIntsStruct *data) {  
    CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54c_badSink(data);
    return;
}

void CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54c_badSink(twoIntsStruct *data) {
    CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54d_badSink(data);
    return;
}

void CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54d_badSink(twoIntsStruct *data) {
    CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54e_badSink(data);
    return;
}

void CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54e_badSink(twoIntsStruct *data) {
    free((void *)data); //WARN
    return;
}

int main(int argc, char **argv) {
    int __retres;
    {
        CWE590_Free_Memory_Not_on_Heap__free_struct_alloca_54_bad();
        __retres = 0;
        goto return_label;
    }

    __retres = 0;
    return_label: 
        return __retres;
}
