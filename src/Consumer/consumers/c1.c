#include "broken-arith.h"
#include <stdio.h>

int main(int argc, char **argv) {
    int a = 42;
    const int b = argc;
    const int c = broken_add(a, b);
    printf("add returns %d", c);
    const int d = segfault_minus(b, a);
    printf("minus returns %d", d);
    const int e = stateful_multiply(a, b + 1);
    printf("mul returns %d", e);
    const int f = limited_input_range_negate(b);
    printf("neg returns %d", f);
    return 0;
}
