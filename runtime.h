#define FIXNUM_MASK     0x3
#define FIXNUM_TAG      0x0
#define FIXNUM_SHIFT    0x2
#define BOOL_F          0x2F
#define BOOL_T          0x6F
#define NIL             0x3F
#define ENTRY_POINT     _entry_point

typedef unsigned int value_t;

value_t add(value_t, value_t);
