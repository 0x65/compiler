#include <stdio.h>
#include <stdlib.h>

#define FIXNUM_MASK     0x3
#define FIXNUM_TAG      0x0
#define FIXNUM_SHIFT    0x2
#define BOOL_F  0x2F
#define BOOL_T  0x6F
#define NIL     0x3F

typedef unsigned int val;
val _entry_point();

static val error(char* msg) {
    fprintf(stderr, msg);
    exit(1);
    return 0;
}

val add(val a, val b) {
    return ((a >> FIXNUM_SHIFT) + (b >> FIXNUM_SHIFT)) << 2;
}

static void show(val p) {
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
    show(_entry_point());
    printf("\n");
    return 0;
}
