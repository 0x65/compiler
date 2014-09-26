#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"

value_t ENTRY_POINT();

static value_t error(char* msg) {
    fprintf(stderr, "%s\n", msg);
    exit(1);
    return 0;
}

value_t add(value_t a, value_t b) {
    return ((a >> FIXNUM_SHIFT) + (b >> FIXNUM_SHIFT)) << 2;
}

static void show(value_t p) {
    if ((p & FIXNUM_MASK) == FIXNUM_TAG) {
        printf("%d", ((int)p) >> FIXNUM_SHIFT);
    } else if (p == BOOL_F) {
        printf("#f");
    } else if (p == BOOL_T) {
        printf("#t");
    } else if (p == NIL) {
        printf("()");
    } else {
        printf("#<unknown 0x%08x>", p);
    }
}

int main() {
    show(ENTRY_POINT());
    printf("\n");
    return 0;
}
