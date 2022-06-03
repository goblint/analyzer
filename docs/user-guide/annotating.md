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
4. `relation.context`/`relation.no-context` to override the `ana.relation.context` option.
5. `widen`/`no-widen` to override the `ana.context.widen` option.
