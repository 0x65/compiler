#include "runtime.h"
#include "primitives.h"

#define MAKE_CLOSURE(f, a)     static const closure_t f##_closure = { .func = f , .arity = a, .env = NULL }

static value_t _add(value_t a, value_t b) {
    ASSERT_FIXNUM(a);
    ASSERT_FIXNUM(b);
    return FIXNUM_TO_VALUE((VALUE_TO_FIXNUM(a) + VALUE_TO_FIXNUM(b)));
}

MAKE_CLOSURE(_add, 2);

static value_t _cons(value_t a, value_t b) {
    pair_t* pair = _allocate_bytes(sizeof(pair_t));
    pair->first = a;
    pair->second = b;
    return PAIR_TO_VALUE(pair);
}

MAKE_CLOSURE(_cons, 2);

static value_t _car(value_t a) {
    ASSERT_PAIR(a);
    return VALUE_TO_PAIR(a).first;
}

MAKE_CLOSURE(_car, 1);

static value_t _cdr(value_t a) {
    ASSERT_PAIR(a);
    return VALUE_TO_PAIR(a).second;
}

MAKE_CLOSURE(_cdr, 1);

static value_t _is_nil(value_t a) {
    if (a == NIL)
        return BOOL_T;
    else
        return BOOL_F;
}

MAKE_CLOSURE(_is_nil, 1);

void _initialize_primitives() {
    #define X(name) name = CLOSURE_TO_VALUE(&_##name##_closure);
    PRIMITIVES_LIST
    #undef X
}
