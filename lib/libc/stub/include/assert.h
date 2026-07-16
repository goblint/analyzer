#ifndef GOBLINT_NO_ASSERT

void __goblint_assert(_Bool expression);
#undef assert
#define assert(expression) __goblint_assert(expression)

#endif
