#include <stdarg.h>

int sum(int n, ...) {
    va_list args;
    va_start(args, n);
    int sum = 0;
    for(int i = 0; i < n; i++) {
        int x = va_arg(args, int);
        sum += x;
    }
    va_end(args);
    return sum;
}

// manual imitation of what CIL does with va_arg internally
void my__builtin_va_arg(__builtin_va_list arg0, unsigned long arg1, void *arg2) {
    return;
}

int sum2(int n, ...) {
    va_list args;
    va_start(args, n);
    int sum = 0;
    for(int i = 0; i < n; i++) {
        int x;
        my__builtin_va_arg(args, sizeof (int), (void *)(&x));
        sum += x;
    }
    va_end(args);
    return sum;
}

// preprocessed from SV-COMP
struct __va_list_tag;
typedef struct __va_list_tag __va_list_tag;
typedef __builtin_va_list __gnuc_va_list[1U];
typedef __gnuc_va_list va_list[1U];

int sum3(int n, ...) {
    va_list args;
    __builtin_va_start((__va_list_tag *)(&args), n);
    int sum = 0;
    for(int i = 0; i < n; i++) {
        int x;
        my__builtin_va_arg(args, sizeof (int), (void *)(&x));
        sum += x;
    }
    __builtin_va_end((__va_list_tag *)(&args));
    return sum;
}

// more preprocessed from SV-COMP
void my__builtin_va_start(__builtin_va_list arg0) {
  return;
}
void my__builtin_va_end(__builtin_va_list arg0) {
  return;
}

int sum4(int n, ...) {
    va_list args;
    my__builtin_va_start((__va_list_tag *)(&args), n);
    int sum = 0;
    for(int i = 0; i < n; i++) {
        int x;
        my__builtin_va_arg(args, sizeof (int), (void *)(&x));
        sum += x;
    }
    my__builtin_va_end((__va_list_tag *)(&args));
    return sum;
}

int main() {
    sum(1, 2);
    sum(3, 1, 2, 3);

    sum2(1, 2);
    sum2(3, 1, 2, 3);

    sum3(1, 2);
    sum3(3, 1, 2, 3);

    sum4(1, 2);
    sum4(3, 1, 2, 3);
    return 0;
}
