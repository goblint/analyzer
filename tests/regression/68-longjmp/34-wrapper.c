//PARAM: --set ana.malloc.wrappers[+] my_dirty_little_malloc
#include<setjmp.h>
#include<stdlib.h>
#include <goblint.h>

typedef void ( *png_longjmp_ptr) (jmp_buf, int);
struct png_struct_def
{
   jmp_buf jmp_buf_local;
   png_longjmp_ptr longjmp_fn;
   jmp_buf *jmp_buf_ptr;
   size_t jmp_buf_size;
};

typedef struct png_struct_def png_struct;
typedef png_struct * __restrict png_structrp;

void* my_dirty_little_malloc(size_t size) {
    return malloc(size);
}


jmp_buf* png_set_longjmp_fn(png_structrp png_ptr, png_longjmp_ptr longjmp_fn, size_t jmp_buf_size)
{

   if (png_ptr ==((void *)0) )
      return ((void *)0);

   if (png_ptr->jmp_buf_ptr == ((void *)0))
   {
      png_ptr->jmp_buf_size = 0;

      if (jmp_buf_size <= (sizeof png_ptr->jmp_buf_local))
         png_ptr->jmp_buf_ptr = &png_ptr->jmp_buf_local;

      else
      {
         png_ptr->jmp_buf_ptr = my_dirty_little_malloc (jmp_buf_size);

         if (png_ptr->jmp_buf_ptr ==((void *)0))
            return ((void *)0);

         png_ptr->jmp_buf_size = jmp_buf_size;
      }
   }

   else
   {
      size_t size = png_ptr->jmp_buf_size;

      if (size == 0)
      {
         size = (sizeof png_ptr->jmp_buf_local);
         if (png_ptr->jmp_buf_ptr != &png_ptr->jmp_buf_local)
         {
         }
      }

      if (size != jmp_buf_size)
      {
         return ((void *)0);
      }
   }

    png_ptr->longjmp_fn = longjmp_fn;
   return png_ptr->jmp_buf_ptr;
}


int main(void) {
    png_struct *read_ptr = my_dirty_little_malloc(sizeof(png_struct));
    int *soup = my_dirty_little_malloc(sizeof(int));
    *soup = 8;
    int local = 5;

    if (setjmp ((*png_set_longjmp_fn((read_ptr), longjmp, (sizeof (jmp_buf)))))) {
        int z = local; //WARN
        return 48;

    } else {
        local = 8;
        read_ptr->longjmp_fn(*read_ptr->jmp_buf_ptr, 1);
    }
}
