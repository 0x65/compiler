#include <stdint.h>

#define FIXNUM_SHIFT    0x2

#define FIXNUM_MASK     0x3
#define POINTER_MASK    0x7

#define BOOL_F          0x2F
#define BOOL_T          0x6F
#define NIL             0x3F

#define ENTRY_POINT     _entry_point

#define IS_FIXNUM(x)    ((x & FIXNUM_MASK) == FIXNUM)
#define IS_PAIR(x)      ((x & POINTER_MASK) == PAIR)
#define IS_IMMEDIATE(x) ((x & POINTER_MASK) == IMMEDIATE)

#define VALUE_TO_FIXNUM(x)  (x >> FIXNUM_SHIFT)
#define VALUE_TO_PAIR(x)    (*((pair_t*)(x & ~POINTER_MASK)))

#define FIXNUM_TO_VALUE(x)  (x << FIXNUM_SHIFT)
#define PAIR_TO_VALUE(x)    ((value_t)x | PAIR)

typedef uintptr_t value_t;

typedef enum {
    FIXNUM      = 0,
    PAIR        = 1,
    IMMEDIATE   = 7
} tag_t;

typedef struct {
    value_t first;
    value_t second;
} pair_t;

value_t add(value_t, value_t);
value_t cons(value_t, value_t);
value_t car(value_t);
value_t cdr(value_t);
