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


## Functions
Goblint-specific functions can be called in the code, where they assist the analyzer but have no runtime effect.

* `__goblint_split_begin(exp)` begins path-sensitivity w.r.t. the value of `exp`.
  _Expsplit analysis must be activated._
* `__goblint_split_end(exp)` ends path-sensitivity w.r.t. the value of `exp`.
  _Expsplit analysis must be activated._
* `__goblint_assume_join(id)` is like `pthread_join(id)`, but considers the given thread IDs must-joined even if Goblint cannot, e.g. due to non-uniqueness.
  Notably, this annotation can be used after a threads joining loop to make the assumption that the loop correctly joined all those threads.
  _Misuse of this annotation can cause unsoundness._
