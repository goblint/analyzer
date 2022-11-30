// PARAM: --set ana.malloc.wrappers '["Malloc", "Realloc", "Malloc2", "Realloc2", "ARR_CreateInstance", "realloc_array", "ARR_GetNewElement"]' --disable sem.unknown_function.spawn --disable sem.unknown_function.invalidate.globals
// extracted from chrony
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>

// memory.c

void *
Malloc(size_t size)
{
  void *r;

  r = malloc(size);

  return r;
}

void *
Realloc(void *ptr, size_t size)
{
  void *r;

  r = realloc(ptr, size);

  return r;
}

static size_t
get_array_size(size_t nmemb, size_t size)
{
  size_t array_size;

  array_size = nmemb * size;

  return array_size;
}

void *
Realloc2(void *ptr, size_t nmemb, size_t size)
{
  return Realloc(ptr, get_array_size(nmemb, size));
}

#define MallocNew(T) ((T *) Malloc(sizeof(T)))
#define MallocArray(T, n) ((T *) Malloc2(n, sizeof(T)))
#define ReallocArray(T, n, x) ((T *) Realloc2((void *)(x), n, sizeof(T)))
#define Free(x) free(x)

// array.c

struct ARR_Instance_Record {
  void *data;
  unsigned int elem_size;
  unsigned int used;
  unsigned int allocated;
};

typedef struct ARR_Instance_Record *ARR_Instance;

ARR_Instance
ARR_CreateInstance(unsigned int elem_size)
{
  ARR_Instance array;

  assert(elem_size > 0);

  array = MallocNew(struct ARR_Instance_Record);

  array->data = NULL;
  array->elem_size = elem_size;
  array->used = 0;
  array->allocated = 0;

  return array;
}

void *
ARR_GetElement(ARR_Instance array, unsigned int index)
{
  assert(index < array->used); // UNKNOWN
  return (void *)((char *)array->data + (size_t)index * array->elem_size);
}

static void
realloc_array(ARR_Instance array, unsigned int min_size)
{
  assert(min_size <= 2 * min_size); // UNKNOWN
  if (array->allocated >= min_size && array->allocated <= 2 * min_size)
    return;

  if (array->allocated < min_size) {
    while (array->allocated < min_size)
      array->allocated = array->allocated ? 2 * array->allocated : 1;
  } else {
    array->allocated = min_size;
  }

  array->data = Realloc2(array->data, array->allocated, array->elem_size);
}

void *
ARR_GetNewElement(ARR_Instance array)
{
  array->used++;
  realloc_array(array, array->used);
  return ARR_GetElement(array, array->used - 1);
}

unsigned int
ARR_GetSize(ARR_Instance array)
{
  return array->used;
}

// sched.c

typedef void* SCH_ArbitraryArgument;
typedef void (*SCH_FileHandler)(SCH_ArbitraryArgument);

typedef struct {
  SCH_FileHandler       handler;
  SCH_ArbitraryArgument arg;
} FileHandlerEntry;

static ARR_Instance file_handlers;

void
SCH_Initialise(void)
{
  file_handlers = ARR_CreateInstance(sizeof (FileHandlerEntry));
}

void
SCH_AddFileHandler
(SCH_FileHandler handler, SCH_ArbitraryArgument arg)
{
  int fd; // rand
  FileHandlerEntry *ptr;

  while (ARR_GetSize(file_handlers) <= fd) {
    ptr = ARR_GetNewElement(file_handlers);
    ptr->handler = NULL;
    ptr->arg = NULL;
  }

  ptr = ARR_GetElement(file_handlers, fd);

  /* Don't want to allow the same fd to register a handler more than
     once without deleting a previous association - this suggests
     a bug somewhere else in the program. */
  assert(!ptr->handler); // UNKNOWN

  ptr->handler = handler;
  ptr->arg = arg;
}

static void
dispatch_filehandlers()
{
  FileHandlerEntry *ptr;
  int fd; // rand

  ptr = (FileHandlerEntry *)ARR_GetElement(file_handlers, fd);
  SCH_FileHandler stuff = *(ptr->handler);
  if (ptr->handler)
    (ptr->handler)(ptr->arg);
}

// nameserv_async.c

typedef void (*DNS_NameResolveHandler)(void *anything);

struct DNS_Async_Instance {
  DNS_NameResolveHandler handler;
  void *arg;
  pthread_t thread;
};

static void *
start_resolving(void *anything)
{
  struct DNS_Async_Instance *inst = (struct DNS_Async_Instance *)anything;

  return NULL;
}

static void
end_resolving(void *anything)
{
  struct DNS_Async_Instance *inst = (struct DNS_Async_Instance *)anything;

  DNS_NameResolveHandler h = inst->handler;
  (inst->handler)(inst->arg);

  Free(inst);
}

void
DNS_Name2IPAddressAsync(DNS_NameResolveHandler handler, void *anything)
{
  struct DNS_Async_Instance *inst;

  inst = MallocNew(struct DNS_Async_Instance);
  inst->handler = handler;
  inst->arg = anything;

  pthread_create(&inst->thread, NULL, start_resolving, inst);

  SCH_AddFileHandler(end_resolving, inst);
}

// stub

void foo(void *arg) {
  assert(1); // reachable
}

void bar(void *arg) {
  int *p = arg;
  int y = *p;
  assert(1); // reachable
  assert(y); // TODO
}

int main() {
  SCH_Initialise();
  DNS_Name2IPAddressAsync(foo, NULL);
  int x = 1;
  DNS_Name2IPAddressAsync(bar, &x);
  dispatch_filehandlers();
  return 0;
}