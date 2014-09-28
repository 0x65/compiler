#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"

value_t ENTRY_POINT();

static void show(value_t, FILE*);
static void fatal_error(char*);
static void fatal_error_value(char*, value_t);

value_t add(value_t a, value_t b) {
    if (!IS_FIXNUM(a)) fatal_error_value("Can't apply `add` to non-numeric value:", a);
    if (!IS_FIXNUM(b)) fatal_error_value("Can't apply `add` to non-numeric value:", b);
    return FIXNUM_TO_VALUE(VALUE_TO_FIXNUM(a) + VALUE_TO_FIXNUM(b));
}

value_t cons(value_t a, value_t b) {
    pair_t* pair = malloc(sizeof(pair_t));
    if (pair == NULL) fatal_error("Out of memory!");

    // TODO: remove this?
    assert(((value_t)pair & POINTER_MASK) == 0);

    pair->first = a;
    pair->second = b;

    return PAIR_TO_VALUE(pair);
}

value_t car(value_t a) {
    if (!IS_PAIR(a)) fatal_error_value("Can't apply `car` to non-pair value:", a);
    return VALUE_TO_PAIR(a).first;
}

value_t cdr(value_t a) {
    if (!IS_PAIR(a)) fatal_error_value("Can't apply `cdr` to non-pair value:", a);
    return VALUE_TO_PAIR(a).second;
}

static void show(value_t p, FILE* stream) {
    if (IS_FIXNUM(p)) {
        fprintf(stream, "%ld", VALUE_TO_FIXNUM(p));
    } else if (IS_PAIR(p)) {
        pair_t pair = VALUE_TO_PAIR(p);
        fprintf(stream, "(");
        show(pair.first, stream);
        fprintf(stream, " ");
        show(pair.second, stream);
        fprintf(stream, ")");
    } else if (IS_IMMEDIATE(p)) {
        if (p == BOOL_F) {
            fprintf(stream, "#f");
        } else if (p == BOOL_T) {
            fprintf(stream, "#t");
        } else if (p == NIL) {
            fprintf(stream, "()");
        } else {
            fprintf(stream, "#<immediate 0x%08lx>", p);
        }
    } else {
        fprintf(stream, "#<unknown 0x%08lx>", p);
    }
}

static void fatal_error(char* msg) {
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

static void fatal_error_value(char* msg, value_t value) {
    fprintf(stderr, "%s ", msg);
    show(value, stderr);
    fprintf(stderr, "\n");
    exit(1);
}

int main() {
    show(ENTRY_POINT(), stdout);
    printf("\n");
    return 0;
}
