#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "runtime.h"

#define PRIMITIVES_LIST \
    X(add)    \
    X(cons)   \
    X(is_nil) \
    X(car)    \
    X(cdr)

#define X(name) value_t name;
PRIMITIVES_LIST
#undef X

void _initialize_primitives();
#endif
