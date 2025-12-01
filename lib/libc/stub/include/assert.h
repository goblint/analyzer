#ifndef GOBLINT_NO_ASSERT

void __goblint_assert_inner(_Bool exp, char *expstr);
#undef assert
#define assert(exp) __goblint_assert_inner(exp, #exp)

#endif
