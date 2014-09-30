#include "runtime.h"
#include "primitives.h"

#define MAKE_CLOSURE(f, a)     static const closure_t f##_closure = { .func = f , .arity = a };

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

void _initialize_primitives() {
    add = CLOSURE_TO_VALUE(&_add_closure);
    cons = CLOSURE_TO_VALUE(&_cons_closure);
    car = CLOSURE_TO_VALUE(&_car_closure);
    cdr = CLOSURE_TO_VALUE(&_cdr_closure);
}
