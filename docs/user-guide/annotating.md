# Annotating code

The analysis can be fine-tuned by annotating the source code.
This page gives an overview of the supported means.

## Attributes
Attributes (`__attribute__`) can be attached to various source code elements.

### Function attributes
Attributes cannot be attached to a function definition directly, rather must be attached to a separate declaration.
For example:
```c
int f(int x) __attribute__((goblint_context("no-widen")));
int f(int x) {
  // ...
}
```

#### Context attributes
The attribute `goblint_context` can be used to fine-tune function contexts.
The following string arguments are supported:

1. `base.interval`/`base.no-interval` to override the `ana.base.context.interval` option.
2. `base.int`/`base.no-int` to override the `ana.base.context.interval` option.
3. `base.non-ptr`/`base.no-non-ptr` to override the `ana.base.context.non-ptr` option.
4. `apron.context`/`apron.no-context` to override the `ana.apron.context` option.
5. `widen`/`no-widen` to override the `ana.context.widen` option.

### Apron attributes
The Apron library can be set to only track variables with the attribute `goblint_apron_track`

### Array attributes
Arrays can be annotated with the domain that should be used for it ("unroll", "parttioned", or "trivial"):

```c
int x[4] __attribute__((goblint_array_domain("unroll")));
__attribute__((goblint_array_domain("trivial"))) int x[4];

struct array {
	int arr[5] __attribute__((goblint_array_domain("partitioned")));
};
```
It is also possible to annotate a type, so that all arrays of this type without an own attribute will use this one: 

```c
typedef int unrollInt __attribute__((goblint_array_domain("trivial")));
unrollInt x[4];
```
Lastly, pointer parameters can have this attribute and the array arguments may get converted to the choosen domain. This does not always happen because the pointers can change during the analysis which arrays they could be pointing at.



## Functions
Goblint-specific functions can be called in the code, where they assist the analyzer but have no runtime effect.

* `__goblint_assume_join(id)` is like `pthread_join(id)`, but considers the given thread IDs must-joined even if Goblint cannot, e.g. due to non-uniqueness.
  Notably, this annotation can be used after a threads joining loop to make the assumption that the loop correctly joined all those threads.
  _Misuse of this annotation can cause unsoundness._
