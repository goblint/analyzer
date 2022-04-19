// Extracted from zstd
#include <stddef.h>
#include <assert.h>

typedef void* (*ZSTD_allocFunction) (void* opaque, size_t size);
typedef void  (*ZSTD_freeFunction) (void* opaque, void* address);
typedef struct { ZSTD_allocFunction customAlloc; ZSTD_freeFunction customFree; void* opaque; } ZSTD_customMem;

ZSTD_customMem const ZSTD_defaultCMem = { NULL, NULL, NULL };

#define ZSTD_malloc(s) malloc(s)

void* ZSTD_customMalloc(size_t size, ZSTD_customMem customMem)
{
    if (customMem.customAlloc) // WARN (dead branch)
        return customMem.customAlloc(customMem.opaque, size);
    return ZSTD_malloc(size);
}

int* ZSTD_createCCtx_advanced(ZSTD_customMem customMem)
{
  if ((!customMem.customAlloc) ^ (!customMem.customFree)) // WARN (dead branch)
    return NULL;

  return ZSTD_customMalloc(sizeof(int), customMem);
}

int main() {
  int *p = ZSTD_createCCtx_advanced(ZSTD_defaultCMem);
  assert(p != NULL);
  return 0;
}
