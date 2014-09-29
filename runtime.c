#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"

value_t ENTRY_POINT();

static void* allocate_bytes(size_t);
static void show(value_t, FILE*);
static void fatal_error(char*);
static void fatal_error_value(char*, value_t);

value_t make_closure(value_t (*func)(), int arity) {
    closure_t* closure = allocate_bytes(sizeof(closure_t));
    closure->func = func;
    closure->arity = arity;
    return CLOSURE_TO_VALUE(closure);
}

// TODO: rewrite with macros
value_t apply_closure(value_t a, ...) {
    if (!IS_CLOSURE(a)) fatal_error_value("Can't apply `apply_closure` to non-closure value:", a);

    closure_t closure = VALUE_TO_CLOSURE(a);
    value_t result;

    va_list var_args;
    va_start(var_args, a);

    value_t op1, op2;

    switch (closure.arity) {
        case 0:
            result = closure.func();
            break;
        case 1:
            op1 = va_arg(var_args, value_t);
            result = closure.func(op1);
            break;
        case 2:
            op1 = va_arg(var_args, value_t);
            op2 = va_arg(var_args, value_t);
            result = closure.func(op1, op2);
            break;
        default:
            fatal_error("Can't apply that many arguments to a function");
            result = 0;
    }

    va_end(var_args);
    return result;
}

// TODO: rewrite funcs as closures
value_t add(value_t a, value_t b) {
    if (!IS_FIXNUM(a)) fatal_error_value("Can't apply `add` to non-numeric value:", a);
    if (!IS_FIXNUM(b)) fatal_error_value("Can't apply `add` to non-numeric value:", b);
    return FIXNUM_TO_VALUE((VALUE_TO_FIXNUM(a) + VALUE_TO_FIXNUM(b)));
}

value_t cons(value_t a, value_t b) {
    pair_t* pair = allocate_bytes(sizeof(pair_t));
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

static void* allocate_bytes(size_t num) {
    void* mem = malloc(num);
    if (mem == NULL) fatal_error("Out of memory!");

    // TODO: remove this?
    assert(((value_t)mem & POINTER_MASK) == 0);

    return mem;
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
    } else if (IS_CLOSURE(p)) {
        fprintf(stream, "#<closure 0x%08lx>", p);
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
