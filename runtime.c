#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"
#include "primitives.h"

value_t ENTRY_POINT();

value_t _make_closure(value_t (*func)(), int arity, int num_free, ...) {
    closure_t* closure = _allocate_bytes(sizeof(closure_t));
    value_t closure_val = CLOSURE_TO_VALUE(closure);

    closure->func = func;
    closure->arity = arity;

    va_list var_args;
    va_start(var_args, num_free);

    size_t i = 0;
    value_t* env = malloc(sizeof(value_t) * (num_free + 2));
    for (; i < num_free; i++) {
        env[i] = va_arg(var_args, value_t);
    }

    va_end(var_args);

    env[i] = closure_val;
    env[i+1] = 0;

    closure->env = env;

    return closure_val;
}

value_t _apply_closure(value_t a, ...) {
    ASSERT_CLOSURE(a);

    closure_t closure = VALUE_TO_CLOSURE(a);

    va_list var_args;
    va_start(var_args, a);

    value_t op[32];
    size_t i = 0, j = 0;

    for (; i < closure.arity; i++) {
        op[i] = va_arg(var_args, value_t);
    }

    va_end(var_args);

    for (; closure.env != NULL && closure.env[j] != 0; j++, i++) {
        op[i] = closure.env[j];
    }

    value_t result;

    #define OP_0
    #define OP_1        op[0]
    #define OP_2        OP_1 , op[1]
    #define OP_3        OP_2 , op[2]
    #define OP_4        OP_3 , op[3]
    #define OP_5        OP_4 , op[4]
    #define OP_6        OP_5 , op[5]
    #define OP_7        OP_6 , op[6]
    #define OP_8        OP_7 , op[7]
    #define OP(n)       OP_##n
    #define CASE(n)     case n: result = closure.func( OP(n) ); break;

    switch (closure.arity + j) {
        CASE(0);
        CASE(1);
        CASE(2);
        CASE(3);
        CASE(4);
        CASE(5);
        CASE(6);
        CASE(7);
        CASE(8);
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
