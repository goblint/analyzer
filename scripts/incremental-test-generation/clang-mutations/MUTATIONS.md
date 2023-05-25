# Mutations
In this document is described what the different mutations do. In the [Readme](README.md) file it is described how to run the mutations.

## Remove Function Body - RFB
**readability-remove-function-body**<br>
Removes the function body and adds when needed a generic return statement.
```
int sum(int a, int b) {
    return a + b;
}
```
`clang-tidy -checks=-*,readability-remove-function-body -fix test.c --`
```
int sum(int a, int b) { return 0; /* [MUTATION][RFB] Stripped function of its body */ }
```
### Special Option
`-config="{CheckOptions: {readability-remove-function-body.RemoveOnlyFunctionName: 'sum, divide, ...'}}"` Special option for the **remove-function-body** check to only remove the function body of functions named foo1 and foo2.

## Unary Operator Inversion - UOI
**readability-unary-operator-inversion**<br>
Flips if statements by adding a negation.
```
if (a < b) {
    printf("a is less than b\n");
}
```
`clang-tidy -checks=-*,readability-unary-operator-inversion -fix comparisons.c --`
```
if (!(a < b) /* [MUTATION][UOI] Inverted if statement */) {
    printf("a is less than b\n");
}
```

## Relational Operator Replacement - ROR
**readability-relational-operator-replacement**<br>
Replaces `<=` with `<`, `<` with `<=`, `>=` with `>` and `>` with `>=`.
```
if (a <= b) {
    printf("a is less than or equal to b\n");
}
```
`clang-tidy -checks=-*,readability-relational-operator-replacement -fix comparisons.c --`
```
if (a < /* [MUTATION][ROR] Replaced Relational Operator */ b) {
    printf("a is less than or equal to b\n");
}
```

## Constant Replacement - CR
**readability-constant-replacement**<br>
Replaces constants unequal 0 and unequal 1 with 1. The usage of macros is replaced, but not the definition. This is marked with `[MACRO][macro_name]`.
```
#define MY_MACRO_5 5
#define MY_MACRO_0 0
int a = 42;
int b = 0;
int c = MY_MACRO_5;
int d = MY_MACRO_0;
```
`clang-tidy -checks=-*,readability-constant-replacement -fix constants.c --`
```
#define MY_MACRO_5 5
#define MY_MACRO_0 0
int a = 1 /* [MUTATION][CR] Replaced Constant 42 */;
int b = 0;
int c = 1 /* [MUTATION][CR][MACRO][MY_MACRO_5] Replaced Constant 5 */;
int d = MY_MACRO_0;
```

## Remove Thread - RT
**readability-remove-thread**<br>
Replaces a `pthread_create` call with the function call itself. Additionally `0;` is added for the case that the result was checked in the form `result = pthread_create()` The arguments of the function call are kept. Symbols like `*` or `&` in front of the function name are ignored.
```
result = pthread_create(&thread, &attr, thread_function, NULL);
```
`clang-tidy -checks=-*,readability-remove-thread -fix pthread.c --`
```
result = 0; thread_function(NULL) /* [MUTATION][RT][FUNCTION_NAME][thread_function] Thread creation was substituted with function call */;
```

### Remove Thread Wrapper - RTW
**readability-remove-thread-wrapper**<br>
Wraps the given Function Name for a pthread_create call. This should be run before `remove-thread`. The function name has to be passed.
```
void *threadFunction(void *arg) {
    //...
}
```
`clang-tidy -checks=-*,readability-remove-thread-wrapper -config="{CheckOptions: {readability-remove-thread-wrapper.WrapFunctionName: 'threadFunction'}}" -fix pthread.c --`
```
void *threadFunction(void *arg) {
    //...
}
int threadFunction_wrap(void *arg) {
    /*[MUTATION][RTW] Wrapped function for remove-thread */
    threadFunction(arg);
    return 0;
}
```

## Logical Connector Replacement - LCR
**readability-logical-connector-replacement**<br>
Replaces the operator `&&` with `||` and vice versa.
```
if (a && b) {
    printf("Both a and b are non-zero\n");
}
```
`clang-tidy -checks=-*,readability-logical-connector-replacement -fix comparisons.c --`
```
if (a || /* [MUTATION][LCR] Replaced Logical Connector */ b) {
    printf("Both a and b are non-zero\n");
}
```