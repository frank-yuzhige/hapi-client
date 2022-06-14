#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>


/*
 * No crash, yet return value is wrong under certain inputs.
 */
int broken_add(int a, int b) {
    if (a + b == 20000) {
        return 42;
    }
    return a + b;
}

/*
 * Crash under certain inputs.
 */
int segfault_minus(int a, int b) {
    if (a == -20000) {
        *(int *)0 = 1;   // Segfault
    }
    return (int) (a - b);
}

/*
 * This function is good
 */
int multiply(int a, int b) {
    return a * b;
}

/*
 * This function is good, remember b != 0.
 */
int divide(int a, int b) {
    return a / b;
}

/*
 * Limited input domain results exit failure, but does not imply bug.
 */
int negate(int a) {
    // fprintf(stderr, "%d\n", a);
    if (a > 65535 || a < -42) {
        fprintf(stderr, "My negate function only allows input domain to be [-42, 65535], sad!\n");
        exit(EXIT_FAILURE);
    }
    return -a;
}
