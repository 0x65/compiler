#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>
#include <stdio.h>

#define FIXNUM_SHIFT    0x2

#define FIXNUM_MASK     0x3
#define POINTER_MASK    0x7

#define BOOL_F          0x2F
#define BOOL_T          0x6F
#define NIL             0x3F

#define ENTRY_POINT     _entry_point

#define IS_FIXNUM(x)        ((x & FIXNUM_MASK) == FIXNUM)
#define IS_PAIR(x)          ((x & POINTER_MASK) == PAIR)
#define IS_CLOSURE(x)       ((x & POINTER_MASK) == CLOSURE)
#define IS_IMMEDIATE(x)     ((x & POINTER_MASK) == IMMEDIATE)

#define VALUE_TO_FIXNUM(x)  (x >> FIXNUM_SHIFT)
#define VALUE_TO_PAIR(x)    (*((pair_t*)(x & ~POINTER_MASK)))
#define VALUE_TO_CLOSURE(x) (*((closure_t*)(x & ~POINTER_MASK)))

#define FIXNUM_TO_VALUE(x)  (x << FIXNUM_SHIFT)
#define PAIR_TO_VALUE(x)    ((value_t)x | PAIR)
#define CLOSURE_TO_VALUE(x) ((value_t)x | CLOSURE)

#define ASSERT_FIXNUM(x)    if (!IS_FIXNUM(x)) _fatal_error_value("Can't apply function to non-numeric value:", x);
#define ASSERT_PAIR(x)      if (!IS_PAIR(x)) _fatal_error_value("Can't apply function to non-pair value:", x);
#define ASSERT_CLOSURE(x)   if (!IS_CLOSURE(x)) _fatal_error_value("Can't apply function to non-closure value:", x);

typedef uintptr_t value_t;

typedef enum {
    FIXNUM      = 0,
    PAIR        = 1,
    CLOSURE     = 6,
    IMMEDIATE   = 7
} tag_t;

typedef struct {
    value_t first;
    value_t second;
} pair_t;

typedef struct {
    value_t (*func)();
    int arity;
    value_t* env;
} closure_t;

void* _allocate_bytes(size_t);
void _show(value_t, FILE*);
void _fatal_error(char*);
void _fatal_error_value(char*, value_t);
value_t _make_closure(value_t (*func)(), int arity, int num_free, ...);
value_t _apply_closure(value_t, ...);
#endif
