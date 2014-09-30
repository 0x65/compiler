#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"
#include "primitives.h"

value_t ENTRY_POINT();

value_t _make_closure(value_t (*func)(), int arity) {
    closure_t* closure = _allocate_bytes(sizeof(closure_t));
    closure->func = func;
    closure->arity = arity;
    return CLOSURE_TO_VALUE(closure);
}

value_t _apply_closure(value_t a, ...) {
    ASSERT_CLOSURE(a);

    closure_t closure = VALUE_TO_CLOSURE(a);

    va_list var_args;
    va_start(var_args, a);

    value_t op[32];
    for (size_t i = 0; i < closure.arity; i++) {
        op[i] = va_arg(var_args, value_t);
    }

    va_end(var_args);
    value_t result;

    #define OP_0
    #define OP_1        op[0]
    #define OP_2        OP_1 , op[1]
    #define OP_3        OP_2 , op[2]
    #define OP(n)       OP_##n
    #define CASE(n)     case n: result = closure.func( OP(n) ); break;

    switch (closure.arity) {
        CASE(0);
        CASE(1);
        CASE(2);
        CASE(3);
        default:
            _fatal_error("Can't apply that many arguments to a function");
            result = 0;
    }

    return result;
}

void* _allocate_bytes(size_t num) {
    void* mem = malloc(num);
    if (mem == NULL) _fatal_error("Out of memory!");

    // TODO: remove this?
    assert(((value_t)mem & POINTER_MASK) == 0);

    return mem;
}

void _show(value_t p, FILE* stream) {
    if (IS_FIXNUM(p)) {
        fprintf(stream, "%ld", VALUE_TO_FIXNUM(p));
    } else if (IS_PAIR(p)) {
        pair_t pair = VALUE_TO_PAIR(p);
        fprintf(stream, "(");
        _show(pair.first, stream);
        fprintf(stream, " ");
        _show(pair.second, stream);
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

void _fatal_error(char* msg) {
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

void _fatal_error_value(char* msg, value_t value) {
    fprintf(stderr, "%s ", msg);
    _show(value, stderr);
    fprintf(stderr, "\n");
    exit(1);
}

int main() {
    _initialize_primitives();
    _show(ENTRY_POINT(), stdout);
    printf("\n");
    return 0;
}
